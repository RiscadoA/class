package pt.inescid.cllsj.compiler.ir.slot;

import java.util.Optional;

public class IRCellS extends IRSlot {
  // The value stored inside the cell, to be dropped when the cell is freed.
  // If empty, the cell is assumed to be affine, in which case a 'discard' operation
  // is performed (i.e., writing a '0' tag to it and continuing to it).
  private Optional<IRSlotTree> data;

  public static IRCellS affine() {
    return new IRCellS(Optional.empty());
  }

  public static IRCellS value(IRSlotTree data) {
    return new IRCellS(Optional.of(data));
  }

  private IRCellS(Optional<IRSlotTree> data) {
    this.data = data;
  }

  public boolean isAffine() {
    return data.isEmpty();
  }

  public boolean isValue() {
    return data.isPresent();
  }

  public IRSlotTree getValue() {
    if (data.isEmpty()) {
      throw new IllegalStateException("Cell is not value");
    }
    return data.get();
  }

  @Override
  public void accept(IRSlotVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public String toString() {
    if (data.isPresent()) {
      return "cell(" + data.get().toString() + ")";
    } else {
      return "cell(affine)";
    }
  }
}
