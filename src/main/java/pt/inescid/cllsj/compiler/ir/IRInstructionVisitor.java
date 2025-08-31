package pt.inescid.cllsj.compiler.ir;

import pt.inescid.cllsj.compiler.ir.instructions_old.*;

public abstract class IRInstructionVisitor {
  // Catch all for instructions which do not have their own visit method
  public abstract void visit(IRInstruction instruction);

  public void visit(IRNewThread instruction) {
    visit((IRInstruction) instruction);
  }

  public void visit(IRNewTask instruction) {
    visit((IRInstruction) instruction);
  }

  public void visit(IRNextTask instruction) {
    visit((IRInstruction) instruction);
  }

  public void visit(IRFreeSession instruction) {
    visit((IRInstruction) instruction);
  }

  public void visit(IRNewSession instruction) {
    visit((IRInstruction) instruction);
  }

  public void visit(IRPush instruction) {
    visit((IRInstruction) instruction);
  }

  public void visit(IRPop instruction) {
    visit((IRInstruction) instruction);
  }

  public void visit(IRPushClose instruction) {
    visit((IRPush) instruction);
  }

  public void visit(IRPopClose instruction) {
    visit((IRPop) instruction);
  }

  public void visit(IRPushTag instruction) {
    visit((IRPush) instruction);
  }

  public void visit(IRPopTag instruction) {
    visit((IRPop) instruction);
  }

  public void visit(IRPushSession instruction) {
    visit((IRPush) instruction);
  }

  public void visit(IRPopSession instruction) {
    visit((IRPop) instruction);
  }

  public void visit(IRResetSession instruction) {
    visit((IRResetSession) instruction);
  }

  public void visit(IRFlip instruction) {
    visit((IRInstruction) instruction);
  }

  public void visit(IRReturn instruction) {
    visit((IRInstruction) instruction);
  }

  public void visit(IRForward instruction) {
    visit((IRInstruction) instruction);
  }

  public void visit(IRFlipForward instruction) {
    visit((IRInstruction) instruction);
  }

  public void visit(IRCallProcess instruction) {
    visit((IRInstruction) instruction);
  }

  public void visit(IRJump instruction) {
    visit((IRInstruction) instruction);
  }

  public void visit(IRBranch instruction) {
    visit((IRInstruction) instruction);
  }

  public void visit(IRNewExponentialProcess instruction) {
    visit((IRInstruction) instruction);
  }

  public void visit(IRNewExponentialExpression instruction) {
    visit((IRInstruction) instruction);
  }

  public void visit(IRNewExponentialScan instruction) {
    visit((IRInstruction) instruction);
  }

  public void visit(IRPushExponential instruction) {
    visit((IRPush) instruction);
  }

  public void visit(IRPopExponential instruction) {
    visit((IRPop) instruction);
  }

  public void visit(IRCallExponential instruction) {
    visit((IRInstruction) instruction);
  }

  public void visit(IRPrint instruction) {
    visit((IRInstruction) instruction);
  }

  public void visit(IRPushScan instruction) {
    visit((IRPush) instruction);
  }

  public void visit(IRPushExpression instruction) {
    visit((IRPush) instruction);
  }

  public void visit(IRPushType instruction) {
    visit((IRPush) instruction);
  }

  public void visit(IRPopType instruction) {
    visit((IRPop) instruction);
  }

  public void visit(IRCleanRecord instruction) {
    visit((IRInstruction) instruction);
  }

  public void visit(IRIncRefExponential instruction) {
    visit((IRInstruction) instruction);
  }

  public void visit(IRDecRefExponential instruction) {
    visit((IRInstruction) instruction);
  }

  public void visit(IRIncRefCell instruction) {
    visit((IRInstruction) instruction);
  }

  public void visit(IRDecRefCell instruction) {
    visit((IRInstruction) instruction);
  }

  public void visit(IRPushCell instruction) {
    visit((IRPush) instruction);
  }

  public void visit(IRPutCell instruction) {
    visit((IRInstruction) instruction);
  }

  public void visit(IRTakeCell instruction) {
    visit((IRInstruction) instruction);
  }

  public void visit(IRSleep instruction) {
    visit((IRInstruction) instruction);
  }

  public void visit(IRPanic instruction) {
    visit((IRInstruction) instruction);
  }

  public void visit(IRCallLoop instruction) {
    visit((IRCallLoop) instruction);
  }
}
