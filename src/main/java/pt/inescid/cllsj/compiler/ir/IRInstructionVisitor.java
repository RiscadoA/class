package pt.inescid.cllsj.compiler.ir;

import pt.inescid.cllsj.compiler.ir.instructions.*;

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

  public void visit(IRPushClose instruction) {
    visit((IRInstruction) instruction);
  }

  public void visit(IRPopClose instruction) {
    visit((IRInstruction) instruction);
  }

  public void visit(IRPushTag instruction) {
    visit((IRInstruction) instruction);
  }

  public void visit(IRPopTag instruction) {
    visit((IRInstruction) instruction);
  }

  public void visit(IRPushSession instruction) {
    visit((IRInstruction) instruction);
  }

  public void visit(IRPopSession instruction) {
    visit((IRInstruction) instruction);
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

  public void visit(IRCallProcess instruction) {
    visit((IRInstruction) instruction);
  }

  public void visit(IRJump instruction) {
    visit((IRInstruction) instruction);
  }

  public void visit(IRBranch instruction) {
    visit((IRInstruction) instruction);
  }

  public void visit(IRPushExponential instruction) {
    visit((IRInstruction) instruction);
  }

  public void visit(IRForwardExponential instruction) {
    visit((IRInstruction) instruction);
  }

  public void visit(IRPopExponential instruction) {
    visit((IRInstruction) instruction);
  }

  public void visit(IRCallExponential instruction) {
    visit((IRInstruction) instruction);
  }

  public void visit(IRPrint instruction) {
    visit((IRInstruction) instruction);
  }

  public void visit(IRPushExpression instruction) {
    visit((IRInstruction) instruction);
  }

  public void visit(IRPushType instruction) {
    visit((IRInstruction) instruction);
  }

  public void visit(IRPopType instruction) {
    visit((IRInstruction) instruction);
  }

  public void visit(IRPushUnfold instruction) {
    visit((IRInstruction) instruction);
  }

  public void visit(IRPopUnfold instruction) {
    visit((IRInstruction) instruction);
  }

  public void visit(IRIncRefExponential instruction) {
    visit((IRInstruction) instruction);
  }

  public void visit(IRDecRefExponential instruction) {
    visit((IRInstruction) instruction);
  }
}
