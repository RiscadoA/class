package pt.inescid.cllsj.compiler.ir.expression;

import pt.inescid.cllsj.compiler.ir.expression.arithmetic.*;
import pt.inescid.cllsj.compiler.ir.expression.bool.*;
import pt.inescid.cllsj.compiler.ir.expression.literal.IRBoolLiteral;
import pt.inescid.cllsj.compiler.ir.expression.literal.IRIntLiteral;
import pt.inescid.cllsj.compiler.ir.expression.literal.IRStringLiteral;

public abstract class IRExpressionVisitor {
  public abstract void visit(IRRead read);

  public abstract void visit(IRAdd add);

  public abstract void visit(IRSubtract sub);

  public abstract void visit(IRMultiply mul);

  public abstract void visit(IRDivide div);

  public abstract void visit(IRModulus mod);

  public abstract void visit(IRAnd and);

  public abstract void visit(IROr or);

  public abstract void visit(IRNot not);

  public abstract void visit(IRGreaterThan gt);

  public abstract void visit(IRLessThan lt);

  public abstract void visit(IREqual eq);

  public abstract void visit(IRIntLiteral lit);

  public abstract void visit(IRBoolLiteral lit);

  public abstract void visit(IRStringLiteral lit);
}
