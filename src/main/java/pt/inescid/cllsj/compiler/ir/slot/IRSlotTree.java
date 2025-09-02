package pt.inescid.cllsj.compiler.ir.slot;

import java.util.List;
import java.util.Optional;

public abstract class IRSlotTree {
  public static final IRSlotTree LEAF = new Leaf();

  public static Unary of(IRSlot slot) {
    return of(slot, LEAF);
  }

  public static Unary of(IRSlot slot, IRSlotTree child) {
    return new Unary(slot, child);
  }

  public static IRSlotTree of(IRSlotSequence slots) {
    IRSlotTree result = LEAF;
    for (int i = slots.size() - 1; i >= 0; i--) {
      result = of(slots.get(i), result);
    }
    return result;
  }

  public static Tag tag(List<IRSlotTree> cases) {
    return new Tag(cases);
  }

  public abstract IRSlotCombinations combinations();

  protected abstract void toStringHelper(StringBuilder sb);

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    toStringHelper(sb);
    return sb.toString();
  }

  public boolean isLeaf() {
    return this instanceof Leaf;
  }

  public boolean isUnary() {
    return this instanceof Unary;
  }

  public boolean isTag() {
    return this instanceof Tag;
  }

  public abstract Optional<IRSlot> slot();

  public abstract IRSlotTree suffix(IRSlotTree other);

  public static class Leaf extends IRSlotTree {
    @Override
    public IRSlotCombinations combinations() {
      return IRSlotCombinations.EMPTY;
    }

    @Override
    public Optional<IRSlot> slot() {
      return Optional.empty();
    }

    @Override
    public IRSlotTree suffix(IRSlotTree other) {
      return other;
    }
    @Override
    protected void toStringHelper(StringBuilder sb) {
      if (sb.isEmpty()) {
        sb.append("[]");
      }
    }
  }

  public static class Unary extends IRSlotTree {
    private IRSlot slot;
    private IRSlotTree child;

    public Unary(IRSlot slot, IRSlotTree child) {
      this.slot = slot;
      this.child = child;
    }

    @Override
    public IRSlotCombinations combinations() {
      return child.combinations().prefix(slot);
    }

    @Override
    public Optional<IRSlot> slot() {
      return Optional.of(slot);
    }

    @Override
    public IRSlotTree suffix(IRSlotTree other) {
      return new Unary(slot, child.suffix(other));
    }

    @Override
    protected void toStringHelper(StringBuilder sb) {
      boolean started = false;
      if (sb.isEmpty()) {
        sb.append("[");
        started = true;
      }
      sb.append(slot);
      if (!child.isLeaf()) {
        sb.append("; ");
        child.toStringHelper(sb);
      }
      if (started) {
        sb.append("]");
      }
    }
  }

  public static class Tag extends IRSlotTree {
    private List<IRSlotTree> cases;

    public Tag(List<IRSlotTree> cases) {
      this.cases = cases;
    }

    @Override
    public IRSlotCombinations combinations() {
      IRSlotCombinations result = IRSlotCombinations.EMPTY;
      for (IRSlotTree caseTree : cases) {
        result = result.merge(caseTree.combinations());
      }
      return result.prefix(new IRTagS());
    }

    @Override
    public Optional<IRSlot> slot() {
      return Optional.of(new IRTagS());
    }

    @Override
    public IRSlotTree suffix(IRSlotTree other) {
      return new Tag(cases.stream().map(c -> c.suffix(other)).toList());
    }

    @Override
    protected void toStringHelper(StringBuilder sb) {
      sb.append("tag[");
      for (int i = 0; i < cases.size(); i++) {
        if (i > 0) {
          sb.append(" | ");
        }
        sb.append(cases.get(i));
      }
      sb.append("]");
    }
  }
}
