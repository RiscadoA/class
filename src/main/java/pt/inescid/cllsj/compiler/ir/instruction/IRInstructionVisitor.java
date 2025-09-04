package pt.inescid.cllsj.compiler.ir.instruction;

public abstract class IRInstructionVisitor {
  public abstract void visit(IRInitializeSession instr);

  public abstract void visit(IRContinueSession instr);

  public abstract void visit(IRFinishSession instr);

  public abstract void visit(IRBindSession instr);

  public abstract void visit(IRWriteExpression instr);

  public abstract void visit(IRWriteScan instr);

  public abstract void visit(IRWriteSession instr);

  public abstract void visit(IRWriteTag instr);

  public abstract void visit(IRMoveValue instr);

  public abstract void visit(IRCloneValue instr);

  public abstract void visit(IRPrint instr);

  public abstract void visit(IRPushTask instr);

  public abstract void visit(IRPopTask instr);

  public abstract void visit(IRCallProcess instr);

  public abstract void visit(IRWriteExponential instr);

  public abstract void visit(IRCallExponential instr);

  public abstract void visit(IRTieSessions instr);

  public abstract void visit(IRBranchTag instr);

  public abstract void visit(IRBranchExpression instr);

  public abstract void visit(IRDeferDrop instr);
}
