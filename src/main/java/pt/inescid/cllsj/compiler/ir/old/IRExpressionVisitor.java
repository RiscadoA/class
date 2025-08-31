package pt.inescid.cllsj.compiler.ir.old;

import pt.inescid.cllsj.compiler.ir.old.expressions.IRAdd;
import pt.inescid.cllsj.compiler.ir.old.expressions.IRAnd;
import pt.inescid.cllsj.compiler.ir.old.expressions.IRBool;
import pt.inescid.cllsj.compiler.ir.old.expressions.IRDiv;
import pt.inescid.cllsj.compiler.ir.old.expressions.IREq;
import pt.inescid.cllsj.compiler.ir.old.expressions.IRExponentialVar;
import pt.inescid.cllsj.compiler.ir.old.expressions.IRExpression;
import pt.inescid.cllsj.compiler.ir.old.expressions.IRGt;
import pt.inescid.cllsj.compiler.ir.old.expressions.IRInt;
import pt.inescid.cllsj.compiler.ir.old.expressions.IRLt;
import pt.inescid.cllsj.compiler.ir.old.expressions.IRMul;
import pt.inescid.cllsj.compiler.ir.old.expressions.IRNot;
import pt.inescid.cllsj.compiler.ir.old.expressions.IROr;
import pt.inescid.cllsj.compiler.ir.old.expressions.IRString;
import pt.inescid.cllsj.compiler.ir.old.expressions.IRSub;
import pt.inescid.cllsj.compiler.ir.old.expressions.IRVar;

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

  public void visit(IRExponentialVar expr) {
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

  public void visit(IREq expr) {
    visit((IRExpression) expr);
  }

  public void visit(IRLt expr) {
    visit((IRExpression) expr);
  }

  public void visit(IRGt expr) {
    visit((IRExpression) expr);
  }

  public void visit(IRAnd expr) {
    visit((IRExpression) expr);
  }

  public void visit(IROr expr) {
    visit((IRExpression) expr);
  }

  public void visit(IRNot expr) {
    visit((IRExpression) expr);
  }
}
