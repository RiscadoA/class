package pt.inescid.cllsj.compiler;

import java.util.HashMap;
import java.util.Map;
import pt.inescid.cllsj.ast.ASTNodeVisitor;
import pt.inescid.cllsj.ast.nodes.*;

public class Generator extends ASTNodeVisitor {
  private String generatedCode = "";
  private int indentLevel = 0;
  private int labelIndex = 0;
  private Map<String, Integer> sessionRecordSizes = new HashMap<>();
  private Map<String, Boolean> sessionPolarities = new HashMap<>();

  public static String generate(ASTNode ast) {
    Generator generator = new Generator();

    generator.putLine("#include <stdlib.h>");
    generator.putLine("#include <stdio.h>");
    generator.putLine("");
    generator.putLine("int main() {");
    generator.indentLevel++;
    generator.putLine("void* " + generator.sessionContSwapRegister() + ";");
    generator.putLine("");
    ast.accept(generator);
    generator.putLine("");
    generator.putLine("return 0;");
    generator.indentLevel--;
    generator.putLine("}");

    return generator.generatedCode;
  }

  @Override
  public void visit(ASTNode node) {
    throw new UnsupportedOperationException(
        "Nodes of type "
            + node.getClass().getSimpleName()
            + " are not yet supported by the generator");
  }

  @Override
  public void visit(ASTCase node) {
    // We pop a label from the session queue and use it as the switch expression.
    this.beginLine("switch (");
    this.popLabel(node.getCh());
    this.endLine(") {");
    this.indentLevel++;

    for (int i = 0; i < node.getCaseCount(); i++) {
      String label = node.getCaseLabelFromIndex(i);
      this.putLine("case " + i + " /* " + label + " */: {");
      this.indentLevel++;

      // Generate the continuation for each case.
      // We preserve the session polarities so that we generate the correct code for the next cases.
      Map<String, Boolean> oldSessionPolarities = new HashMap<>(sessionPolarities);
      this.putPrint(node.getCh() + ".pop(" + label + ")");
      this.generateContinuation(node.getCase(label));
      sessionPolarities = oldSessionPolarities;

      this.indentLevel--;
      this.putLine("} break;");
    }

    this.indentLevel--;
    this.putLine("}");
  }

  @Override
  public void visit(ASTClose node) {
    // Push a close token onto the session queue.
    this.pushClose(node.getCh());

    // Jump to the continuation, which the type checker guarantees to eventually contain a matching
    // wait process.
    this.putLine("goto *" + sessionContRegister(node.getCh()) + ";");
  }

  @Override
  public void visit(ASTCoClose node) {
    // Pop a close token from the session queue.
    this.popClose(node.getCh());

    // The session has been closed, we can get rid of the session record.
    this.freeSessionRecord(node.getCh());

    this.generateContinuation(node.getRhs());
  }

  @Override
  public void visit(ASTCut node) {
    assert !sessionPolarities.containsKey(node.getCh());
    assert !sessionRecordSizes.containsKey(node.getCh());

    String cutRhs = this.makeLabel("cut_rhs_" + node.getCh());
    String cutEnd = this.makeLabel("cut_end_" + node.getCh());

    // Allocate a new session record for the channel, with the continuation set to the right hand
    // side of the cut.
    int size = SizeCalculator.calculate(node.getChType());
    sessionRecordSizes.put(node.getCh(), size);
    this.allocSessionRecord(node.getCh(), size, cutRhs);

    // The first code to be executed for a given session must be positive, so we set the polarity to
    // true.
    this.sessionPolarities.put(node.getCh(), true);
    this.generateContinuation(node.getLhs());
    this.putJump(cutEnd);

    // The right hand side code only executes right after the left hand side jumps to it.
    // Thus, we set the initial session polarity to false, so that the right hand side doesn't jump
    // back to the left hand side if it is a negative node.
    this.sessionPolarities.put(node.getCh(), false);
    this.putLabel(cutRhs);
    this.generateContinuation(node.getRhs());
    this.putLabel(cutEnd);

    // We clean up the session state on the compiler side.
    this.sessionPolarities.remove(node.getCh());
    this.sessionRecordSizes.remove(node.getCh());
  }

  @Override
  public void visit(ASTEmpty node) {
    putPrint("empty");
  }

  @Override
  public void visit(ASTMix node) {
    // Simply execute both sides of the mix sequentially.
    node.getLhs().accept(this);
    node.getRhs().accept(this);
  }

  @Override
  public void visit(ASTSelect node) {
    // Push the label's index onto the session queue.
    this.pushLabel(node.getCh(), node.getLabelIndex(), node.getLabel());
    this.generateContinuation(node.getRhs());
  }

  private void generateContinuation(ASTNode node) {
    String ch = node.getSubjectCh();
    if (ch != null) {
      assert sessionPolarities.containsKey(ch) : "No polarity previously set for channel " + ch;
      if (sessionPolarities.get(ch) && !node.isPos()) {
        // If we're continuing a positive session and we encounter a negative node,
        // we need to first return to the continuation.
        String label = this.makeLabel("cont");
        this.putLine(
            sessionContSwapRegister() + " = " + sessionContRegister(ch) + "; /* polarity swap */");
        this.putLine(sessionContRegister(ch) + " = &&" + label + ";");
        this.putLine("goto *" + sessionContSwapRegister() + ";");
        this.putLabel(label);
      }

      // Update the session polarity.
      sessionPolarities.put(ch, node.isPos());
    }

    node.accept(this);
  }

  private void pushClose(String ch) {
    // The close token is zero-sized, so we don't actually do anything to the queue.
    putPrint(ch + ".push(close)");
  }

  private void popClose(String ch) {
    // The close token is zero-sized, so we don't actually do anything to the queue.
    putPrint(ch + ".pop(close)");
  }

  // Creates a new statement which pushes a label onto the session queue.
  private void pushLabel(String ch, int labelIndex, String label) {
    pushLiteral(ch, "unsigned char", Integer.toString(labelIndex) + " /* " + label + " */");
    putPrint(ch + ".push(" + label + ")");
  }

  // Creates a new expression which pops a label from the session queue.
  private void popLabel(String ch) {
    popLiteral(ch, "unsigned char");
  }

  // Creates a new statement which pushes a literal onto the session queue.
  private void pushLiteral(String ch, String cType, String literal) {
    putLine("*(unsigned char*)" + sessionQueueWriteRegister(ch) + " = " + literal + ";");
    putLine(sessionQueueWriteRegister(ch) + " += sizeof(" + cType + ");");
  }

  // Creates a new expression which pops a literal from the session queue.
  private void popLiteral(String ch, String cType) {
    generatedCode +=
        "*(("
            + cType
            + "*)(("
            + sessionQueueReadRegister(ch)
            + " += sizeof("
            + cType
            + ")) - sizeof("
            + cType
            + ")))";
  }

  private void allocSessionRecord(String ch, int size, String contLabel) {
    putLine("void* " + sessionContRegister(ch) + " = &&" + contLabel + ";");

    // Only allocate the session queue if we actually need one.
    if (size > 0) {
      putLine("unsigned char* " + sessionQueueRegister(ch) + " = malloc(" + size + ");");
      putLine(
          "unsigned char* "
              + sessionQueueWriteRegister(ch)
              + " = "
              + sessionQueueRegister(ch)
              + ";");
      putLine(
          "unsigned char* "
              + sessionQueueReadRegister(ch)
              + " = "
              + sessionQueueRegister(ch)
              + ";");
    }

    putPrint(ch + ".alloc(" + size + ")");
  }

  private void freeSessionRecord(String ch) {
    Integer size = sessionRecordSizes.get(ch);
    assert size != null : "No session record allocated for channel " + ch;

    // Only free the session record if we actually allocated one.
    if (size > 0) {
      putLine("free((unsigned char*)" + sessionQueueRegister(ch) + ");");
    }

    putPrint(ch + ".free()");
  }

  private String sessionContRegister(String ch) {
    return "scont_" + ch;
  }

  private String sessionContSwapRegister() {
    return "swap_scont";
  }

  private String sessionQueueRegister(String ch) {
    return "squeue_m_" + ch;
  }

  private String sessionQueueReadRegister(String ch) {
    return "squeue_r_" + ch;
  }

  private String sessionQueueWriteRegister(String ch) {
    return "squeue_w_" + ch;
  }

  private void putPrint(String msg) {
    putLine("puts(\"" + escapeString(msg) + "\");");
  }

  private void putJump(String label) {
    putLine("goto " + label + ";");
  }

  // Adds an indented line to the generated code
  private void putLine(String line) {
    this.beginLine(line);
    this.endLine("");
  }

  // Adds an indented line to the generated code, without a newline at the end
  private void beginLine(String line) {
    if (!line.isBlank()) {
      generatedCode += "  ".repeat(indentLevel) + line;
    }
  }

  private void endLine(String line) {
    generatedCode += line + "\n";
  }

  // Adds a label to the generated code
  private void putLabel(String label) {
    generatedCode += label + ":\n";
  }

  private String escapeString(String string) {
    return string.replace("\\", "\\\\").replace("\"", "\\\"");
  }

  private String makeLabel(String prefix) {
    return prefix + "_lbl_" + labelIndex++;
  }
}
