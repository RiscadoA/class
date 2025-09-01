package pt.inescid.cllsj.compiler.ast;

import java.io.PrintStream;
import java.util.Map;
import pt.inescid.cllsj.ast.ASTExprVisitor;
import pt.inescid.cllsj.ast.ASTNodeVisitor;
import pt.inescid.cllsj.ast.nodes.*;
import pt.inescid.cllsj.ast.types.ASTType;

public class ASTPrinter extends ASTNodeVisitor {
  private PrintStream out;
  private int indentLevel = 0;

  public ASTPrinter(PrintStream stream) {
    this.out = stream;
  }

  private void indent() {
    for (int i = 0; i < indentLevel; i++) {
      out.print("  ");
    }
  }

  private void indentPrintln(String str) {
    indent();
    out.println(str);
  }

  private void indentPrint(String str) {
    indent();
    out.print(str);
  }

  private class ASTExprPrinter extends ASTExprVisitor {
    @Override
    public void visit(ASTExpr expr) {
      throw new UnsupportedOperationException(
          "Printing of expressions of type "
              + expr.getClass().getSimpleName()
              + " is not yet supported");
    }

    @Override
    public void visit(ASTInt node) {
      out.print(node.getValue());
    }

    @Override
    public void visit(ASTBool node) {
      out.print(node.getValue() ? "true" : "false");
    }

    @Override
    public void visit(ASTString node) {
      out.print("\"" + node.getValue() + "\"");
    }

    @Override
    public void visit(ASTVId node) {
      out.print(node.getCh());
    }

    @Override
    public void visit(ASTAdd expr) {
      out.print("(");
      expr.getLhs().accept(this);
      out.print(" + ");
      expr.getRhs().accept(this);
      out.print(")");
    }

    @Override
    public void visit(ASTSub expr) {
      out.print("(");
      expr.getLhs().accept(this);
      out.print(" - ");
      expr.getRhs().accept(this);
      out.print(")");
    }

    @Override
    public void visit(ASTMul expr) {
      out.print("(");
      expr.getLhs().accept(this);
      out.print(" * ");
      expr.getRhs().accept(this);
      out.print(")");
    }

    @Override
    public void visit(ASTDiv expr) {
      out.print("(");
      expr.getLhs().accept(this);
      out.print(" / ");
      expr.getRhs().accept(this);
      out.print(")");
    }

    @Override
    public void visit(ASTEq expr) {
      out.print("(");
      expr.getLhs().accept(this);
      out.print(" == ");
      expr.getRhs().accept(this);
      out.print(")");
    }

    @Override
    public void visit(ASTNEq expr) {
      out.print("(");
      expr.getLhs().accept(this);
      out.print(" != ");
      expr.getRhs().accept(this);
      out.print(")");
    }

    @Override
    public void visit(ASTLt expr) {
      out.print("(");
      expr.getLhs().accept(this);
      out.print(" < ");
      expr.getRhs().accept(this);
      out.print(")");
    }

    @Override
    public void visit(ASTGt expr) {
      out.print("(");
      expr.getLhs().accept(this);
      out.print(" > ");
      expr.getRhs().accept(this);
      out.print(")");
    }

    @Override
    public void visit(ASTAnd expr) {
      out.print("(");
      expr.getLhs().accept(this);
      out.print(" and ");
      expr.getRhs().accept(this);
      out.print(")");
    }

    @Override
    public void visit(ASTOr expr) {
      out.print("(");
      expr.getLhs().accept(this);
      out.print(" or ");
      expr.getRhs().accept(this);
      out.print(")");
    }

    @Override
    public void visit(ASTNot expr) {
      out.print("!");
      expr.getExpr().accept(this);
    }
  }

  public static void print(PrintStream stream, ASTNode node) {
    node.accept(new ASTPrinter(stream));
  }

  @Override
  public void visit(ASTNode node) {
    throw new UnsupportedOperationException(
        "Printing of nodes of type " + node.getClass().getSimpleName() + " is not yet supported");
  }

  @Override
  public void visit(ASTBang node) {
    indentPrintln("!" + node.getChr() + "(" + node.getChi() + ");");
    node.getRhs().accept(this);
  }

  @Override
  public void visit(ASTCall node) {
    indentPrintln("call " + node.getChr() + "(" + node.getChi() + ");");
    node.getRhs().accept(this);
  }

  @Override
  public void visit(ASTCase node) {
    indentPrintln("case " + node.getCh() + " of {");
    for (Map.Entry<String, ASTNode> branch : node.getCases().entrySet()) {
      indentPrintln("|" + branch.getKey() + ":");
      indentLevel++;
      branch.getValue().accept(this);
      indentLevel--;
    }
    indentPrintln("}");
  }

  @Override
  public void visit(ASTClose node) {
    indentPrintln("close " + node.getCh());
  }

  @Override
  public void visit(ASTCoClose node) {
    indentPrintln("wait " + node.getCh());
    node.getRhs().accept(this);
  }

  @Override
  public void visit(ASTCut node) {
    indentPrintln("cut {");
    indentLevel++;
    node.getLhs().accept(this);
    indentPrintln("|" + node.getCh() + "|");
    node.getRhs().accept(this);
    indentLevel--;
    indentPrintln("}");
  }

  @Override
  public void visit(ASTShare node) {
    indentPrintln("share " + node.getCh() + " {");
    indentLevel++;
    node.getLhs().accept(this);
    indentPrintln("||");
    node.getRhs().accept(this);
    indentLevel--;
    indentPrintln("}");
  }

  @Override
  public void visit(ASTEmpty node) {
    indentPrintln("()");
  }

  @Override
  public void visit(ASTFwd node) {
    indentPrintln("fwd " + node.getCh1() + " " + node.getCh2());
  }

  @Override
  public void visit(ASTFwdB node) {
    indentPrintln("fwd! " + node.getCh1() + " " + node.getCh2());
  }

  @Override
  public void visit(ASTId node) {
    indent();
    out.print(node.getId());
    boolean first = true;
    for (ASTType type : node.getTPars()) {
      if (first) {
        out.print("<");
        first = false;
      } else {
        out.print(", ");
      }
      out.print(type.toString());
    }
    if (!first) {
      out.print(">");
    }

    out.print("(");
    first = true;
    for (String par : node.getPars()) {
      if (first) {
        first = false;
      } else {
        out.print(", ");
      }
      out.print(par);
    }

    first = true;
    for (String gpar : node.getGPars()) {
      if (first) {
        first = false;
        out.print("; ");
      } else {
        out.print(", ");
      }
      out.print(gpar);
    }
    out.println(")");
  }

  @Override
  public void visit(ASTMix node) {
    indentPrintln("par {");
    indentLevel++;
    node.getLhs().accept(this);
    indentPrintln("||");
    node.getRhs().accept(this);
    indentLevel--;
    indentPrintln("}");
  }

  @Override
  public void visit(ASTPrintLn node) {
    indentPrint("println(");
    node.getExpr().accept(new ASTExprPrinter());
    out.println(");");
    node.getRhs().accept(this);
  }

  @Override
  public void visit(ASTScan node) {
    indentPrintln("scan(" + node.getCh() + ")");
  }

  @Override
  public void visit(ASTCoExpr node) {
    indentPrint("let " + node.getCh() + " ");
    node.getExpr().accept(new ASTExprPrinter());
    out.println();
  }

  @Override
  public void visit(ASTPromoCoExpr node) {
    indentPrint("let! " + node.getCh() + " ");
    node.getExpr().accept(new ASTExprPrinter());
    out.println();
  }

  @Override
  public void visit(ASTProcDef node) {
    indentPrint("proc " + node.getId());

    boolean first = true;
    for (String arg : node.getTArgs()) {
      if (first) {
        out.print("<");
        first = false;
      } else {
        out.print(", ");
      }
      out.print(arg);
    }
    if (!first) {
      out.print(">");
    }

    out.print("(");
    first = true;
    for (String arg : node.getArgs()) {
      if (first) {
        first = false;
      } else {
        out.print(", ");
      }
      out.print(arg);
    }
    first = true;
    for (String gArg : node.getGArgs()) {
      if (first) {
        first = false;
        out.print("; ");
      } else {
        out.print(", ");
      }
      out.print(gArg);
    }
    out.println(") {");

    indentLevel++;
    node.getRhs().accept(this);
    indentLevel--;
    indentPrintln("};;");
  }

  @Override
  public void visit(ASTProgram node) {
    boolean first = true;
    for (ASTProcDef procDef : node.getProcDefs()) {
      if (first) {
        first = false;
      } else {
        out.println();
      }
      procDef.accept(this);
    }
  }

  @Override
  public void visit(ASTRecv node) {
    indentPrintln("recv " + node.getChr() + "(" + node.getChi() + ");");
    node.getRhs().accept(this);
  }

  @Override
  public void visit(ASTSelect node) {
    indentPrintln(node.getLabel() + " " + node.getCh() + ";");
    node.getRhs().accept(this);
  }

  @Override
  public void visit(ASTSend node) {
    indentPrintln("send " + node.getChs() + "(" + node.getCho() + ".");
    indentLevel++;
    node.getLhs().accept(this);
    indentLevel--;
    indentPrintln(");");
    node.getRhs().accept(this);
  }

  @Override
  public void visit(ASTUnfold node) {
    indentPrintln("unfold " + node.getCh() + ";");
    node.getRhs().accept(this);
  }

  @Override
  public void visit(ASTWhy node) {
    indentPrintln("?" + node.getCh() + ";");
    node.getRhs().accept(this);
  }

  @Override
  public void visit(ASTIf node) {
    indentPrint("if ");
    node.getExpr().accept(new ASTExprPrinter());
    out.println(" {");
    indentLevel++;
    node.getThen().accept(this);
    indentLevel--;
    indentPrintln("} else {");
    indentLevel++;
    node.getElse().accept(this);
    indentLevel--;
    indentPrintln("}");
  }

  @Override
  public void visit(ASTSendTy node) {
    indentPrintln("sendty " + node.getChs() + "(" + node.getType() + ");");
    node.getRhs().accept(this);
  }

  @Override
  public void visit(ASTRecvTy node) {
    indentPrintln("recvty " + node.getChs() + "(" + node.getTyid() + ");");
    node.getRhs().accept(this);
  }

  @Override
  public void visit(ASTAffine node) {
    indentPrintln("affine " + node.getCh() + ";");
    node.getRhs().accept(this);
  }

  @Override
  public void visit(ASTUse node) {
    indentPrintln("use " + node.getCh() + ";");
    node.getRhs().accept(this);
  }

  @Override
  public void visit(ASTDiscard node) {
    indentPrintln("discard " + node.getCh());
  }

  @Override
  public void visit(ASTCell node) {
    indentPrintln("cell " + node.getCh() + "(" + node.getChc() + ".");
    indentLevel++;
    node.getRhs().accept(this);
    indentLevel--;
    indentPrintln(")");
  }

  @Override
  public void visit(ASTPut node) {
    indentPrintln("put " + node.getChs() + "(" + node.getCho() + ".");
    indentLevel++;
    node.getRhs().accept(this);
    indentLevel--;
    indentPrintln(");");
    node.getRhs().accept(this);
  }

  @Override
  public void visit(ASTTake node) {
    indentPrintln("take " + node.getChr() + "(" + node.getChi() + ");");
    node.getRhs().accept(this);
  }

  @Override
  public void visit(ASTRelease node) {
    indentPrintln("release " + node.getChr());
  }

  @Override
  public void visit(ASTSleep node) {
    indentPrintln("sleep " + node.getMsecs() + ";");
    node.getRhs().accept(this);
  }

  @Override
  public void visit(ASTUnreachable node) {
    indentPrintln("unreachable " + node.getCh());
  }
}
