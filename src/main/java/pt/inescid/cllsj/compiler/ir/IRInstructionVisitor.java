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

  public void visit(IRRead instruction) {
    visit((IRInstruction) instruction);
  }

  public void visit(IRReadSession instruction) {
    visit((IRRead) instruction);
  }

  public void visit(IRReadExponential instruction) {
    visit((IRRead) instruction);
  }

  public void visit(IRReadTag instruction) {
    visit((IRRead) instruction);
  }

  public void visit(IRReadType instruction) {
    visit((IRRead) instruction);
  }

  public void visit(IRWrite instruction) {
    visit((IRInstruction) instruction);
  }

  public void visit(IRWriteSession instruction) {
    visit((IRWrite) instruction);
  }

  public void visit(IRWriteExponential instruction) {
    visit((IRWrite) instruction);
  }

  public void visit(IRWriteTag instruction) {
    visit((IRWrite) instruction);
  }

  public void visit(IRWriteType instruction) {
    visit((IRWrite) instruction);
  }

  public void visit(IRWriteExpression instruction) {
    visit((IRWrite) instruction);
  }

  public void visit(IRWriteCell instruction) {
    visit((IRWrite) instruction);
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

  public void visit(IRNewExponential instruction) {
    visit((IRInstruction) instruction);
  }


  public void visit(IRCallExponential instruction) {
    visit((IRInstruction) instruction);
  }

  public void visit(IRDetachExponential instruction) {
    visit((IRInstruction) instruction);
  }

  public void visit(IRPrint instruction) {
    visit((IRInstruction) instruction);
  }

  public void visit(IRScan instruction) {
    visit((IRInstruction) instruction);
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
}
