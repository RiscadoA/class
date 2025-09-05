package pt.inescid.cllsj.compiler.ir.slot;

public abstract class IRSlotVisitor {
  public abstract void visit(IRIntS slot);

  public abstract void visit(IRBoolS slot);

  public abstract void visit(IRStringS slot);

  public abstract void visit(IRTagS slot);

  public abstract void visit(IRSessionS slot);

  public abstract void visit(IRExponentialS slot);

  public abstract void visit(IRTypeS slot);

  public abstract void visit(IRVarS slot);

  public abstract void visit(IRKnownVarS slot);
}
