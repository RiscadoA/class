package pt.inescid.cllsj.compiler.ir;

import java.util.function.Function;
import pt.inescid.cllsj.ast.ASTExprVisitor;
import pt.inescid.cllsj.ast.nodes.*;
import pt.inescid.cllsj.ast.types.ASTType;
import pt.inescid.cllsj.compiler.ir.expression.*;
import pt.inescid.cllsj.compiler.ir.expression.arithmetic.*;
import pt.inescid.cllsj.compiler.ir.expression.bool.*;
import pt.inescid.cllsj.compiler.ir.expression.literal.*;
import pt.inescid.cllsj.compiler.ir.slot.IRSlot;

public class IRExpressionGenerator extends ASTExprVisitor {
  private IRExpression ir;
  private IREnvironment env;
  private Function<ASTType, IRSlot> slotFromType;

  public static IRExpression generate(
      IREnvironment env, ASTExpr expr, Function<ASTType, IRSlot> slotFromType) {
    IRExpressionGenerator gen = new IRExpressionGenerator();
    gen.env = env;
    gen.slotFromType = slotFromType;
    expr.accept(gen);
    return gen.ir;
  }

  private IRExpression recurse(ASTExpr expr) {
    return generate(env, expr, slotFromType);
  }

  @Override
  public void visit(ASTInt expr) {
    ir = new IRIntLiteral(expr.getValue());
  }

  @Override
  public void visit(ASTBool expr) {
    ir = new IRBoolLiteral(expr.getValue());
  }

  @Override
  public void visit(ASTString expr) {
    ir = new IRStringLiteral(expr.getValue());
  }

  @Override
  public void visit(ASTVId expr) {
    IREnvironment.Channel session = env.getChannel(expr.getCh());
    IRSlot slot = slotFromType.apply(expr.getType());
    if (expr.isLinear()) {
      ir = new IRMove(session.getLocalData(), slot);
    } else {
      ir = new IRClone(session.getLocalData(), slot);
    }
  }

  @Override
  public void visit(ASTAdd expr) {
    ir = new IRAdd(recurse(expr.getLhs()), recurse(expr.getRhs()));
  }

  @Override
  public void visit(ASTSub expr) {
    ir = new IRSubtract(recurse(expr.getLhs()), recurse(expr.getRhs()));
  }

  @Override
  public void visit(ASTMul expr) {
    ir = new IRMultiply(recurse(expr.getLhs()), recurse(expr.getRhs()));
  }

  @Override
  public void visit(ASTDiv expr) {
    ir = new IRDivide(recurse(expr.getLhs()), recurse(expr.getRhs()));
  }

  @Override
  public void visit(ASTMod expr) {
    ir = new IRModulo(recurse(expr.getLhs()), recurse(expr.getRhs()));
  }

  @Override
  public void visit(ASTEq expr) {
    ir = new IREqual(recurse(expr.getLhs()), recurse(expr.getRhs()));
  }

  @Override
  public void visit(ASTNEq expr) {
    ir = new IRNot(new IREqual(recurse(expr.getLhs()), recurse(expr.getRhs())));
  }

  @Override
  public void visit(ASTLt expr) {
    ir = new IRLessThan(recurse(expr.getLhs()), recurse(expr.getRhs()));
  }

  @Override
  public void visit(ASTLeq expr) {
    ir = new IRNot(new IRGreaterThan(recurse(expr.getLhs()), recurse(expr.getRhs())));
  }

  @Override
  public void visit(ASTGt expr) {
    ir = new IRGreaterThan(recurse(expr.getLhs()), recurse(expr.getRhs()));
  }

  @Override
  public void visit(ASTGeq expr) {
    ir = new IRNot(new IRLessThan(recurse(expr.getLhs()), recurse(expr.getRhs())));
  }

  @Override
  public void visit(ASTAnd expr) {
    ir = new IRAnd(recurse(expr.getLhs()), recurse(expr.getRhs()));
  }

  @Override
  public void visit(ASTOr expr) {
    ir = new IROr(recurse(expr.getLhs()), recurse(expr.getRhs()));
  }

  @Override
  public void visit(ASTNot expr) {
    ir = new IRNot(recurse(expr.getExpr()));
  }

  @Override
  public void visit(ASTOrd expr) {
    ir = new IROrdinal(recurse(expr.getExpr()));
  }
}
