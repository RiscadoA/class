package pt.inescid.cllsj.compiler;

import pt.inescid.cllsj.ast.ASTVisitor;
import pt.inescid.cllsj.ast.nodes.*;

public class Generator extends ASTVisitor {
  private String generatedCode = "";
  private int indentLevel = 0;

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
    putPrint("close " + node.getCh());
  }

  @Override
  public void visit(ASTCoClose node) {
    putPrint("wait " + node.getCh());
    node.getRhs().accept(this);
  }

  @Override
  public void visit(ASTCut node) {
    putPrint("cut lhs " + node.getId());
    node.getLhs().accept(this);
    putPrint("cut rhs " + node.getId());
    node.getRhs().accept(this);
    putPrint("cut end " + node.getId());
  }

  @Override
  public void visit(ASTEmpty node) {
    putPrint("empty");
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
