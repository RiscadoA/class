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
    generator.putLine("struct session_record {");
    generator.indentLevel++;
    generator.putLine("void* cont;");
    generator.putLine("unsigned char* queue_m;");
    generator.putLine("unsigned char* queue_r;");
    generator.putLine("unsigned char* queue_w;");
    generator.indentLevel--;
    generator.putLine("};");
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
    this.putPrint("case " + node.getCh());

    // We pop a label from the session queue and use it as the switch expression.
    this.putLine("switch (" + this.popLabel(node.getCh()) + ") {");
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
    this.putPrint("close " + node.getCh());

    String closeEnd = this.makeLabel("close_end_" + node.getCh());

    // Push a close token onto the session queue.
    this.pushClose(node.getCh());

    // Set the new continuation to point to after the close.
    // This won't be used normally, but it may be necessary when there's another process next to
    // this one which should execute.
    this.putLine(sessionContSwapRegister() + " = " + sessionContRegister(node.getCh()) + ";");
    this.putLine(sessionContRegister(node.getCh()) + " = &&" + closeEnd + ";");

    // So that a forward can detect that the session has been closed.
    this.putLine(sessionQueueWriteRegister(node.getCh()) + " = NULL;");

    // Jump to the previously stored continuation, which the type checker guarantees to eventually
    // contain a matching wait process.
    this.putLine("goto *" + sessionContSwapRegister() + ";");
    this.putLabel(closeEnd);
  }

  @Override
  public void visit(ASTCoClose node) {
    this.putPrint("wait " + node.getCh());

    // Pop a close token from the session queue.
    this.popClose(node.getCh());

    // The session has been closed, we can get rid of the session record.
    this.freeSessionRecord(node.getCh());

    this.generateContinuation(node.getRhs());
  }

  @Override
  public void visit(ASTCut node) {
    this.putPrint("begin cut " + node.getCh());

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

    this.putPrint("end cut " + node.getCh());
  }

  @Override
  public void visit(ASTEmpty node) {
    putPrint("()");
  }

  @Override
  public void visit(ASTFwd node) {
    this.putPrint("fwd " + node.getCh1() + " " + node.getCh2());

    // Check which of the channels is positive.
    String negative, positive;
    if (node.getCh2Type().isPos()) {
      positive = node.getCh2();
      negative = node.getCh1();
    } else {
      positive = node.getCh1();
      negative = node.getCh2();
    }

    assert sessionPolarities.containsKey(negative);
    if (sessionPolarities.get(negative)) {
      // We've been writing to the negative end point, which means that we should jump back to it.
      this.putLine(sessionContSwapRegister() + " = " + sessionContRegister(negative) + ";");
      this.putLine(sessionContRegister(negative) + " = " + sessionContRegister(positive) + ";");
  } else {
      // We've been reading from the negative end point, which means that we should jump to the positive end point.
      this.putLine(sessionContSwapRegister() + " = " + sessionContRegister(positive) + ";");
    }

    // We free the negative session record, as its pointer will now be pointing to the positive session record.
    this.freeSessionRecord(positive);
    this.putLine(sessionRecordRegister(positive) + " = " + sessionRecordRegister(negative) + ";");
    this.putPrint(positive + " = " + negative);

    // Jump to the address we decided on earlier.
    this.putLine("goto *" + sessionContSwapRegister() + ";");
    // TODO: what if there is code after the forward which should be executed? For example, a parallel process.
  }

  @Override
  public void visit(ASTMix node) {
    this.putPrint("begin par");

    // Simply execute both sides of the mix sequentially.
    node.getLhs().accept(this);
    node.getRhs().accept(this);

    this.putPrint("end par");
  }

  @Override
  public void visit(ASTRecv node) {
    this.putPrint("recv " + node.getChr() + " " + node.getChi());

    String recvRhs = this.makeLabel("recv_" + node.getChr() + "_" + node.getChi() + "_rhs");

    assert !this.sessionPolarities.containsKey(node.getChi());
    assert !this.sessionRecordSizes.containsKey(node.getChi());

    // Pop a session record from the session queue.
    this.popSessionRecord(node.getChr(), node.getChi());

    // We set the continuation to the right hand side of the receive, and jump to it.
    this.putLine(sessionContSwapRegister() + " = " + sessionContRegister(node.getChi()) + ";");
    this.putLine(sessionContRegister(node.getChi()) + " = &&" + recvRhs + ";");
    this.putLine("goto *" + sessionContSwapRegister() + ";");

    // The session is fresh, so we consider its previous polarity to be positive.
    this.sessionPolarities.put(node.getChi(), true);
    this.sessionRecordSizes.put(node.getChi(), SizeCalculator.calculate(node.getChiType()));
    this.putLabel(recvRhs);
    this.generateContinuation(node.getRhs());
    this.sessionRecordSizes.remove(node.getChi());
    this.sessionPolarities.remove(node.getChi());
  }

  @Override
  public void visit(ASTSelect node) {
    this.putPrint(node.getCh() + "." + node.getLabel());

    // Push the label's index onto the session queue.
    this.pushLabel(node.getCh(), node.getLabelIndex(), node.getLabel());
    this.generateContinuation(node.getRhs());
  }

  @Override
  public void visit(ASTSend node) {
    this.putPrint("send " + node.getChs() + "(" + node.getCho() + ")");

    String sendLhs = this.makeLabel("send_" + node.getChs() + "_" + node.getCho() + "_lhs");
    String sendRhs = this.makeLabel("send_" + node.getChs() + "_" + node.getCho() + "_rhs");

    assert !this.sessionPolarities.containsKey(node.getCho());
    assert !this.sessionRecordSizes.containsKey(node.getCho());

    // Initialize a new session record.
    int size = SizeCalculator.calculate(node.getLhsType());
    sessionRecordSizes.put(node.getCho(), size);
    this.allocSessionRecord(node.getCho(), size, sendLhs);
    this.putJump(sendRhs);

    // The session is fresh, so we consider its previous polarity to be positive.
    this.sessionPolarities.put(node.getCho(), true);

    // Generate the continuation for the left hand side.
    // Additionally, we jump to the receiver continuation after the left hand side is done.
    // This is necessary in cases where the session is not closed by the left hand side.
    this.putLabel(sendLhs);
    this.generateContinuation(node.getLhs());
    this.putLine("goto *" + sessionContRegister(node.getCho()) + ";");
    this.sessionRecordSizes.remove(node.getCho());
    this.sessionPolarities.remove(node.getCho());

    // Push the session record onto the session queue and continue with the right hand side.
    this.putLabel(sendRhs);
    this.pushSessionRecord(node.getChs(), node.getCho());
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
  private String popLabel(String ch) {
    return popLiteral(ch, "unsigned char");
  }

  // Creates a new statement which pushes a session record onto the session queue.
  private void pushSessionRecord(String ch, String pushedCh) {
    pushLiteral(ch, "struct session_record*", sessionRecordRegister(pushedCh));
    putPrint(ch + ".push(" + pushedCh + ")");
  }

  // Creates statements which create a new session record from a session queue.
  private void popSessionRecord(String ch, String poppedCh) {
    putLine(
        "struct session_record* "
            + sessionRecordRegister(poppedCh)
            + " = "
            + popLiteral(ch, "struct session_record*")
            + ";");
    putPrint(ch + ".pop(" + poppedCh + ")");
  }

  // Creates statements which pushes a literal onto the session queue.
  private void pushLiteral(String ch, String cType, String literal) {
    putLine("*(" + cType + "*)" + sessionQueueWriteRegister(ch) + " = " + literal + ";");
    putLine(sessionQueueWriteRegister(ch) + " += sizeof(" + cType + ");");
  }

  // Returns a new expression which pops a literal from the session queue.
  private String popLiteral(String ch, String cType) {
    return "*(("
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
    putLine("struct session_record session_record_mem_" + ch + ";");
    putLine(
        "struct session_record* "
            + sessionRecordRegister(ch)
            + " = &session_record_mem_"
            + ch
            + ";");

    putLine(sessionContRegister(ch) + " = &&" + contLabel + ";");

    // Only allocate the session queue if we actually need one.
    if (size > 0) {
      putLine(sessionQueueRegister(ch) + " = malloc(" + size + ");");
      putLine(sessionQueueWriteRegister(ch) + " = " + sessionQueueRegister(ch) + ";");
      putLine(sessionQueueReadRegister(ch) + " = " + sessionQueueRegister(ch) + ";");
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

  private String sessionContSwapRegister() {
    return "swap_scont";
  }

  private String sessionRecordRegister(String ch) {
    return "session_record_ptr_" + ch;
  }

  private String sessionContRegister(String ch) {
    return sessionRecordRegister(ch) + "->cont";
  }

  private String sessionQueueRegister(String ch) {
    return sessionRecordRegister(ch) + "->queue_m";
  }

  private String sessionQueueReadRegister(String ch) {
    return sessionRecordRegister(ch) + "->queue_r";
  }

  private String sessionQueueWriteRegister(String ch) {
    return sessionRecordRegister(ch) + "->queue_w";
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
