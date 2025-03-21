package pt.inescid.cllsj.compiler.ir;

import pt.inescid.cllsj.compiler.ir.instructions.IRBranchOnPolarity;
import pt.inescid.cllsj.compiler.ir.instructions.IRCall;
import pt.inescid.cllsj.compiler.ir.instructions.IRFlip;
import pt.inescid.cllsj.compiler.ir.instructions.IRForward;
import pt.inescid.cllsj.compiler.ir.instructions.IRFreeSession;
import pt.inescid.cllsj.compiler.ir.instructions.IRInstruction;
import pt.inescid.cllsj.compiler.ir.instructions.IRJump;
import pt.inescid.cllsj.compiler.ir.instructions.IRNewSession;
import pt.inescid.cllsj.compiler.ir.instructions.IRNewTask;
import pt.inescid.cllsj.compiler.ir.instructions.IRNextTask;
import pt.inescid.cllsj.compiler.ir.instructions.IRPopClose;
import pt.inescid.cllsj.compiler.ir.instructions.IRPopSession;
import pt.inescid.cllsj.compiler.ir.instructions.IRPopTag;
import pt.inescid.cllsj.compiler.ir.instructions.IRPushClose;
import pt.inescid.cllsj.compiler.ir.instructions.IRPushSession;
import pt.inescid.cllsj.compiler.ir.instructions.IRPushTag;

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

  public void visit(IRForward instruction) {
    visit((IRInstruction) instruction);
  }

  public void visit(IRCall instruction) {
    visit((IRInstruction) instruction);
  }

  public void visit(IRJump instruction) {
    visit((IRInstruction) instruction);
  }

  public void visit(IRBranchOnPolarity instruction) {
    visit((IRInstruction) instruction);
  }
}
