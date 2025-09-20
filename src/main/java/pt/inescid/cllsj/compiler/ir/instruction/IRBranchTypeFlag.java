package pt.inescid.cllsj.compiler.ir.instruction;

import java.util.List;
import java.util.function.BiFunction;
import java.util.function.Function;
import pt.inescid.cllsj.compiler.ir.IRTypeFlagRequisites;
import pt.inescid.cllsj.compiler.ir.id.IRTypeFlag;
import pt.inescid.cllsj.compiler.ir.id.IRTypeId;
import pt.inescid.cllsj.compiler.ir.slot.IRSlotTree;

public class IRBranchTypeFlag extends IRBranch {
  IRTypeFlagRequisites requisites;
  Case then;
  Case otherwise;

  public IRBranchTypeFlag(IRTypeFlagRequisites requisites, Case then, Case otherwise) {
    this.requisites = requisites;
    this.then = then;
    this.otherwise = otherwise;
  }

  public IRTypeFlagRequisites getRequisites() {
    return requisites;
  }

  public Case getThen() {
    return then;
  }

  public Case getOtherwise() {
    return otherwise;
  }

  @Override
  public List<Case> getCases() {
    return List.of(then, otherwise);
  }

  @Override
  public void accept(IRInstructionVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public IRInstruction clone() {
    return new IRBranchTypeFlag(requisites, then.clone(), otherwise.clone());
  }

  @Override
  public String toString() {
    return "branchTypeFlag(" + requisites + ", " + then + ", " + otherwise + ")";
  }

  @Override
  public void replaceTypes(
      Function<IRTypeId, IRSlotTree> slotReplacer,
      BiFunction<IRTypeId, IRTypeFlag, IRTypeFlagRequisites> reqReplacer) {
    super.replaceTypes(slotReplacer, reqReplacer);
    requisites = requisites.expandTypes(reqReplacer);
  }
}
