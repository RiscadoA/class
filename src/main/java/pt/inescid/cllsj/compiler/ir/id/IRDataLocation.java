package pt.inescid.cllsj.compiler.ir.id;

import java.util.Optional;
import java.util.function.Function;
import pt.inescid.cllsj.compiler.ir.slot.IRSlotOffset;
import pt.inescid.cllsj.compiler.ir.slot.IRSlotTree;

public class IRDataLocation {
  private int index;
  private boolean remote; // True if the location is remote, false if local

  // If we're pointing to a cell's content, this is used
  // In that case, index is irrelevant and remote should be false
  private Optional<IRDataLocation> cell;

  // Offset within the data (can be zero)
  private IRSlotOffset offset;

  public static IRDataLocation local(IRLocalDataId id, IRSlotOffset offset) {
    return new IRDataLocation(id.getIndex(), false, Optional.empty(), offset);
  }

  public static IRDataLocation remote(IRSessionId id, IRSlotOffset offset) {
    return new IRDataLocation(id.getIndex(), true, Optional.empty(), offset);
  }

  public static IRDataLocation cell(IRDataLocation cell, IRSlotOffset offset) {
    return new IRDataLocation(0, false, Optional.of(cell), offset);
  }

  private IRDataLocation(
      int index, boolean remote, Optional<IRDataLocation> cell, IRSlotOffset offset) {
    this.index = index;
    this.remote = remote;
    this.cell = cell;
    this.offset = offset;
  }

  public IRLocalDataId getLocalDataId() {
    if (!isLocal()) {
      throw new IllegalStateException("Data location is not local");
    }
    return new IRLocalDataId(index);
  }

  public IRSessionId getSessionId() {
    if (!isRemote()) {
      throw new IllegalStateException("Data location is not remote");
    }
    return new IRSessionId(index);
  }

  public IRDataLocation getCell() {
    if (!isCell()) {
      throw new IllegalStateException("Data location is not a cell");
    }
    return cell.get();
  }

  public boolean isLocal() {
    return !remote && cell.isEmpty();
  }

  public boolean isRemote() {
    return remote;
  }

  public boolean isCell() {
    return cell.isPresent();
  }

  public IRSlotOffset getOffset() {
    return offset;
  }

  public IRDataLocation advance(IRSlotOffset newOffset) {
    return new IRDataLocation(index, remote, cell, offset.advance(newOffset));
  }

  public IRDataLocation replaceSessions(Function<IRSessionId, IRSessionId> replacer) {
    if (isRemote()) {
      return IRDataLocation.remote(replacer.apply(getSessionId()), offset);
    } else if (isCell()) {
      return IRDataLocation.cell(getCell().replaceSessions(replacer), offset);
    } else {
      return this;
    }
  }

  public IRDataLocation replaceLocalData(Function<IRLocalDataId, IRLocalDataId> replacer) {
    if (isLocal()) {
      return IRDataLocation.local(replacer.apply(getLocalDataId()), offset);
    } else if (isCell()) {
      return IRDataLocation.cell(getCell().replaceLocalData(replacer), offset);
    } else {
      return this;
    }
  }

  public IRDataLocation replaceSlots(Function<IRSlotTree, IRSlotTree> replacer) {
    return new IRDataLocation(index, remote, cell, offset.replaceSlots(replacer));
  }

  @Override
  public String toString() {
    StringBuilder b = new StringBuilder();
    if (remote) {
      b.append(new IRSessionId(index).toString());
    } else if (cell.isEmpty()) {
      b.append(new IRLocalDataId(index).toString());
    } else {
      b.append("c(");
      b.append(cell.get().toString());
      b.append(")");
    }
    if (!offset.isZero()) {
      b.append(".");
      b.append(offset.toString());
    }
    return b.toString();
  }

  @Override
  public boolean equals(Object obj) {
    if (this == obj) return true;
    if (obj == null || getClass() != obj.getClass()) return false;
    IRDataLocation other = (IRDataLocation) obj;
    return index == other.index && remote == other.remote && offset.equals(other.offset);
  }

  @Override
  public int hashCode() {
    int result = Integer.hashCode(index);
    result = 31 * result + Boolean.hashCode(remote);
    result = 31 * result + offset.hashCode();
    return result;
  }
}
