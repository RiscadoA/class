package pt.inescid.cllsj.compiler.ir;

import pt.inescid.cllsj.compiler.ir.instructions.IRBranchOnPolarity;
import pt.inescid.cllsj.compiler.ir.instructions.IRCallExponential;
import pt.inescid.cllsj.compiler.ir.instructions.IRCallProcess;
import pt.inescid.cllsj.compiler.ir.instructions.IRFlip;
import pt.inescid.cllsj.compiler.ir.instructions.IRForward;
import pt.inescid.cllsj.compiler.ir.instructions.IRFreeSession;
import pt.inescid.cllsj.compiler.ir.instructions.IRInstruction;
import pt.inescid.cllsj.compiler.ir.instructions.IRJump;
import pt.inescid.cllsj.compiler.ir.instructions.IRNewSession;
import pt.inescid.cllsj.compiler.ir.instructions.IRNewTask;
import pt.inescid.cllsj.compiler.ir.instructions.IRNextTask;
import pt.inescid.cllsj.compiler.ir.instructions.IRPopClose;
import pt.inescid.cllsj.compiler.ir.instructions.IRPopExponential;
import pt.inescid.cllsj.compiler.ir.instructions.IRPopSession;
import pt.inescid.cllsj.compiler.ir.instructions.IRPopTag;
import pt.inescid.cllsj.compiler.ir.instructions.IRPrint;
import pt.inescid.cllsj.compiler.ir.instructions.IRPushClose;
import pt.inescid.cllsj.compiler.ir.instructions.IRPushExponential;
import pt.inescid.cllsj.compiler.ir.instructions.IRPushExpression;
import pt.inescid.cllsj.compiler.ir.instructions.IRPushSession;
import pt.inescid.cllsj.compiler.ir.instructions.IRPushTag;
import pt.inescid.cllsj.compiler.ir.instructions.IRReturn;

public abstract class IRInstructionVisitor {
  // Catch all for instructions which do not have their own visit method
  public abstract void visit(IRInstruction instruction);

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

  public void visit(IRBranchOnPolarity instruction) {
    visit((IRInstruction) instruction);
  }

  public void visit(IRPushExponential instruction) {
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
}
