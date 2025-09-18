package pt.inescid.cllsj.compiler.ir.instruction;

import java.util.function.Function;
import pt.inescid.cllsj.compiler.ir.IRValueRequisites;
import pt.inescid.cllsj.compiler.ir.id.IRDataLocation;
import pt.inescid.cllsj.compiler.ir.id.IRTypeId;
import pt.inescid.cllsj.compiler.ir.slot.IRCellS;
import pt.inescid.cllsj.compiler.ir.slot.IRSlotTree;

public class IRReleaseCell extends IRAccess {
  private IRCellS cell;

  public IRReleaseCell(IRDataLocation location, IRCellS cell) {
    super(location);
    this.cell = cell;
  }

  public IRCellS getCell() {
    return cell;
  }

  @Override
  public void accept(IRInstructionVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public IRInstruction clone() {
    return new IRReleaseCell(location, cell);
  }

  @Override
  public String toString() {
    return "releaseCell(" + location + ", " + cell + ")";
  }

  @Override
  public void replaceTypes(
      Function<IRTypeId, IRSlotTree> slotReplacer,
      Function<IRTypeId, IRValueRequisites> reqReplacer) {
    cell = (IRCellS) cell.replaceTypes(slotReplacer, reqReplacer).singleHead().get();
  }
}
