package pt.inescid.cllsj.compiler.ir;

import pt.inescid.cllsj.compiler.ir.expressions.IRAdd;
import pt.inescid.cllsj.compiler.ir.expressions.IRBool;
import pt.inescid.cllsj.compiler.ir.expressions.IRDiv;
import pt.inescid.cllsj.compiler.ir.expressions.IRExpression;
import pt.inescid.cllsj.compiler.ir.expressions.IRInt;
import pt.inescid.cllsj.compiler.ir.expressions.IRMul;
import pt.inescid.cllsj.compiler.ir.expressions.IRString;
import pt.inescid.cllsj.compiler.ir.expressions.IRSub;
import pt.inescid.cllsj.compiler.ir.expressions.IRVar;

public abstract class IRExpressionVisitor {
  // Catch all for expressions which do not have their own visit method
  public abstract void visit(IRExpression expr);

  public void visit(IRInt expr) {
    visit((IRExpression) expr);
  }

  public void visit(IRBool expr) {
    visit((IRExpression) expr);
  }

  public void visit(IRString expr) {
    visit((IRExpression) expr);
  }

  public void visit(IRVar expr) {
    visit((IRExpression) expr);
  }

  public void visit(IRAdd expr) {
    visit((IRExpression) expr);
  }

  public void visit(IRSub expr) {
    visit((IRExpression) expr);
  }

  public void visit(IRMul expr) {
    visit((IRExpression) expr);
  }

  public void visit(IRDiv expr) {
    visit((IRExpression) expr);
  }
}
