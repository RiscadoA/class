package pt.inescid.cllsj.compiler;

import java.util.HashMap;
import java.util.Map;
import pt.inescid.cllsj.Env;
import pt.inescid.cllsj.ast.ASTVisitor;
import pt.inescid.cllsj.ast.nodes.*;

public class Generator extends ASTVisitor {
  private String generatedCode = "";
  private int indentLevel = 0;
  private final Map<String, SessionRecord> sessionRecords = new HashMap<>();

  public static String generate(ASTNode ast) {
    Generator generator = new Generator();

    generator.putLine("#include <stdio.h>");
    generator.putLine("");
    generator.putLine("int main() {");

    generator.indentLevel++;
    ast.accept(generator);
    generator.indentLevel--;

    generator.putLine("  return 0;");
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
  public void visit(ASTClose node) {
    assert sessionRecords.containsKey(node.getCh())
        : "Session record not found for channel " + node.getCh();
    // The close token is zero-sized, so we don't actually do anything to the stack.
    putPrint("push 'close' to " + node.getCh());
  }

  @Override
  public void visit(ASTCoClose node) {
    SessionRecord sessionRecord = sessionRecords.get(node.getCh());
    assert sessionRecord != null : "Session record not found for channel " + node.getCh();

    // Generate code for the other side of the session.
    // Will guarantedly end in a close.
    sessionRecord.continuation.accept(this);

    // The close token is zero-sized, so we don't actually do anything to the stack.
    putPrint("pop 'close' from " + node.getCh());

    // TODO: deallocate the session record here.
    sessionRecords.remove(node.getCh());

    // Continue executing.
    node.getRhs().accept(this);
  }

  @Override
  public void visit(ASTCut node) {
    boolean typeIsPos;
    try {
      typeIsPos = node.getType().isPos(new Env<>());
    } catch (Exception e) {
      throw new RuntimeException(e);
    }

    ASTNode first = typeIsPos ? node.getRhs() : node.getLhs();
    ASTNode second = typeIsPos ? node.getLhs() : node.getRhs();

    assert !sessionRecords.containsKey(node.getId())
        : "Session record already exists for node " + node.getId();
    sessionRecords.put(node.getId(), new SessionRecord(node.getType(), first));
    second.accept(this);
  }

  @Override
  public void visit(ASTEmpty node) {
    putPrint("empty");
  }

  @Override
  public void visit(ASTMix node) {
    node.getLhs().accept(this);
    node.getRhs().accept(this);
  }

  private void putPrint(String msg) {
    putLine("puts(\"" + escapeString(msg) + "\");");
  }

  // Adds an indented line to the generated code
  private void putLine(String line) {
    generatedCode += "  ".repeat(indentLevel) + line + "\n";
  }

  private String escapeString(String string) {
    return string.replace("\\", "\\\\").replace("\"", "\\\"");
  }
}
