package pt.inescid.cllsj.compiler.ast;

import java.io.PrintStream;
import java.util.Map;
import pt.inescid.cllsj.ast.ASTExprVisitor;
import pt.inescid.cllsj.ast.ASTNodeVisitor;
import pt.inescid.cllsj.ast.ASTTypeVisitor;
import pt.inescid.cllsj.ast.nodes.*;
import pt.inescid.cllsj.ast.types.*;

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
    public void visit(ASTMod expr) {
      out.print("(");
      expr.getLhs().accept(this);
      out.print(" % ");
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

  private class ASTTypePrinter extends ASTTypeVisitor {
    @Override
    public void visit(ASTBangT type) {
      out.print("!");
      type.getin().accept(this);
    }

    @Override
    public void visit(ASTBotT type) {
      out.print("wait");
    }

    @Override
    public void visit(ASTCaseT type) {
      out.print("choice of {");
      boolean first = true;
      for (Map.Entry<String, ASTType> branch : type.getcases().entrySet()) {
        if (first) {
          first = false;
        } else {
          out.print(" ");
        }
        out.print("|" + branch.getKey() + ": ");
        branch.getValue().accept(this);
      }
      out.print("}");
    }

    @Override
    public void visit(ASTCoRecT type) {
      out.print("corec " + type.getid() + ". ");
      type.getin().accept(this);
    }

    @Override
    public void visit(ASTIdT type) {
      out.print(type.getid());
      if (!type.getargs().isEmpty()) {
        out.print("(");
        boolean first = true;
        for (ASTType arg : type.getargs()) {
          if (first) {
            first = false;
          } else {
            out.print(", ");
          }
          arg.accept(this);
        }
        out.print(")");
      }
    }

    @Override
    public void visit(ASTNotT type) {
      out.print("~");
      type.getin().accept(this);
    }

    @Override
    public void visit(ASTOfferT type) {
      out.print("offer of {");
      boolean first = true;
      for (Map.Entry<String, ASTType> branch : type.getcases().entrySet()) {
        if (first) {
          first = false;
        } else {
          out.print(" ");
        }
        out.print("|" + branch.getKey() + ": ");
        branch.getValue().accept(this);
      }
      out.print("}");
    }

    @Override
    public void visit(ASTOneT type) {
      out.print("close");
    }

    @Override
    public void visit(ASTRecT type) {
      out.print("rec " + type.getid() + ". ");
      type.getin().accept(this);
    }

    @Override
    public void visit(ASTRecvT type) {
      out.print("recv ");
      type.getlhs().accept(this);
      out.print("; ");
      type.getrhs().accept(this);
    }

    @Override
    public void visit(ASTSendT type) {
      out.print("send ");
      type.getlhs().accept(this);
      out.print("; ");
      type.getrhs().accept(this);
    }

    @Override
    public void visit(ASTWhyT type) {
      out.print("?");
      type.getin().accept(this);
    }

    @Override
    public void visit(ASTintT type) {
      out.print("int");
    }

    @Override
    public void visit(ASTCointT type) {
      out.print("coint");
    }

    @Override
    public void visit(ASTLintT type) {
      out.print("lint");
    }

    @Override
    public void visit(ASTLCointT type) {
      out.print("lcoint");
    }

    @Override
    public void visit(ASTLboolT type) {
      out.print("lbool");
    }

    @Override
    public void visit(ASTCoLboolT type) {
      out.print("colbool");
    }

    @Override
    public void visit(ASTLstringT type) {
      out.print("lstring");
    }

    @Override
    public void visit(ASTCoLstringT type) {
      out.print("colstring");
    }

    @Override
    public void visit(ASTSendTT type) {
      out.print("sendty " + type.getid());
      out.print("; ");
      type.getrhs().accept(this);
    }

    @Override
    public void visit(ASTRecvTT type) {
      out.print("recvty " + type.getid());
      out.print("; ");
      type.getrhs().accept(this);
    }

    @Override
    public void visit(ASTAffineT type) {
      out.print("affine ");
      type.getin().accept(this);
    }

    @Override
    public void visit(ASTCoAffineT type) {
      out.print("coaffine ");
      type.getin().accept(this);
    }

    @Override
    public void visit(ASTCellT type) {
      out.print("state ");
      type.getin().accept(this);
    }

    @Override
    public void visit(ASTUsageT type) {
      out.print("usage ");
      type.getin().accept(this);
    }

    @Override
    public void visit(ASTCellLT type) {
      out.print("statel ");
      type.getin().accept(this);
    }

    @Override
    public void visit(ASTUsageLT type) {
      out.print("usagel ");
      type.getin().accept(this);
    }
  }

  public static void print(PrintStream stream, ASTNode node) {
    node.accept(new ASTPrinter(stream));
  }

  @Override
  public void visit(ASTBang node) {
    indentPrint("!" + node.getChr() + "(" + node.getChi() + ": ");
    node.getType().accept(new ASTTypePrinter());
    out.println("); ");
    node.getRhs().accept(this);
  }

  @Override
  public void visit(ASTCall node) {
    indentPrint("call " + node.getChr() + "(" + node.getChi() + ": ");
    node.getType().accept(new ASTTypePrinter());
    out.println("); ");
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
    indentPrintln("wait " + node.getCh() + ";");
    node.getRhs().accept(this);
  }

  @Override
  public void visit(ASTCut node) {
    indentPrintln("cut {");
    indentLevel++;
    node.getLhs().accept(this);
    indentPrint("|" + node.getCh() + ": ");
    node.getChType().accept(new ASTTypePrinter());
    out.println("|");
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
      type.accept(new ASTTypePrinter());
    }
    if (!first) {
      out.print(">");
    }

    out.print("(");
    first = true;
    for (int i = 0; i < node.getPars().size(); ++i) {

      if (first) {
        first = false;
      } else {
        out.print(", ");
      }
      out.print(node.getPars().get(i));
      out.print(": ");
      node.getParTypes().get(i).accept(new ASTTypePrinter());
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
    for (int i = 0; i < node.getArgs().size(); i++) {
      if (first) {
        first = false;
      } else {
        out.print(", ");
      }
      out.print(node.getArgs().get(i) + ": ");
      node.getArgTypes().get(i).accept(new ASTTypePrinter());
    }
    first = true;
    for (int i = 0; i < node.getGArgs().size(); i++) {
      if (first) {
        first = false;
        out.print("; ");
      } else {
        out.print(", ");
      }
      out.print(node.getGArgs().get(i) + ": ");
      node.getGArgTypes().get(i).accept(new ASTTypePrinter());
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
    indentPrint("recv " + node.getChr() + "(" + node.getChi() + ": ");
    node.getChiType().accept(new ASTTypePrinter());
    out.println(");");
    node.getRhs().accept(this);
  }

  @Override
  public void visit(ASTSelect node) {
    indentPrintln(node.getLabel() + " " + node.getCh() + ";");
    node.getRhs().accept(this);
  }

  @Override
  public void visit(ASTSend node) {
    indentPrint("send " + node.getChs() + "(" + node.getCho() + ": ");
    node.getLhsType().accept(new ASTTypePrinter());
    out.println(".");
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
    indentPrint("cell " + node.getCh() + "(" + node.getChc() + ": ");
    node.getTypeRhs().accept(new ASTTypePrinter());
    out.println(".");
    indentLevel++;
    node.getRhs().accept(this);
    indentLevel--;
    indentPrintln(")");
  }

  @Override
  public void visit(ASTPut node) {
    indentPrint("put " + node.getChs() + "(" + node.getCho() + ": ");
    node.getLhsType().accept(new ASTTypePrinter());
    out.println(".");
    indentLevel++;
    node.getLhs().accept(this);
    indentLevel--;
    indentPrintln(");");
    node.getRhs().accept(this);
  }

  @Override
  public void visit(ASTTake node) {
    indentPrint("take " + node.getChr() + "(" + node.getChi() + ": ");
    node.getChiType().accept(new ASTTypePrinter());
    out.println(");");
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

  @Override
  public void visit(ASTShareL node) {
    indentPrintln("shareL " + node.getCh() + " {");
    indentLevel++;
    node.getLhs().accept(this);
    indentPrintln("||");
    node.getRhs().accept(this);
    indentLevel--;
    indentPrintln("}");
  }

  @Override
  public void visit(ASTShareR node) {
    indentPrintln("shareR " + node.getCh() + " {");
    indentLevel++;
    node.getLhs().accept(this);
    indentPrintln("||");
    node.getRhs().accept(this);
    indentLevel--;
    indentPrintln("}");
  }

  @Override
  public void visit(ASTExpr node) {
    node.accept(new ASTExprPrinter());
  }

  @Override
  public void visit(ASTCLLType node) {
    indentPrintln("clltype " + node.getId() + ";");
    node.getRhs().accept(this);
  }
}
