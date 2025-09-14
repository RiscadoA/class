package pt.inescid.cllsj.compiler.ir.slot;

import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import pt.inescid.cllsj.compiler.ir.IRValueRequisites;

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

  public static IRSlotTree isValue(
      IRValueRequisites requisites, IRSlotTree value, IRSlotTree notValue, IRSlotTree cont) {
    if (requisites.canBeValue()) {
      return new IsValue(requisites, value, notValue, cont);
    } else {
      return notValue;
    }
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

  public boolean isIsValue() {
    return this instanceof IsValue;
  }

  public abstract Optional<IRSlot> singleHead();

  public Set<IRSlot> head() {
    Optional<IRSlot> single = singleHead();
    if (single.isPresent()) {
      return Set.of(single.get());
    } else {
      return Set.of();
    }
  }

  public abstract IRSlotTree suffix(IRSlotTree other);

  public static class Leaf extends IRSlotTree {
    @Override
    public IRSlotCombinations combinations() {
      return IRSlotCombinations.EMPTY;
    }

    @Override
    public Optional<IRSlot> singleHead() {
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

    public IRSlotTree child() {
      return child;
    }

    @Override
    public IRSlotCombinations combinations() {
      return child.combinations().prefix(slot);
    }

    @Override
    public Optional<IRSlot> singleHead() {
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

    public List<IRSlotTree> cases() {
      return cases;
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
    public Optional<IRSlot> singleHead() {
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

  public static class IsValue extends IRSlotTree {
    private IRValueRequisites requisites;
    private IRSlotTree value;
    private IRSlotTree notValue;
    private IRSlotTree cont;

    public IsValue(
        IRValueRequisites requisites, IRSlotTree value, IRSlotTree notValue, IRSlotTree cont) {
      this.requisites = requisites;
      this.value = value;
      this.notValue = notValue;
      this.cont = cont;
    }

    public IRValueRequisites requisites() {
      return requisites;
    }

    public IRSlotTree value() {
      return value;
    }

    public IRSlotTree notValue() {
      return notValue;
    }

    public IRSlotTree cont() {
      return cont;
    }

    @Override
    public IRSlotCombinations combinations() {
      return value.combinations().merge(notValue.combinations()).suffix(cont.combinations());
    }

    @Override
    public Set<IRSlot> head() {
      Set<IRSlot> heads = new HashSet<>();
      heads.addAll(value.head());
      heads.addAll(notValue.head());
      if (value.isLeaf() || notValue.isLeaf()) {
        heads.addAll(cont.head());
      }
      return heads;
    }

    @Override
    public Optional<IRSlot> singleHead() {
      return Optional.empty();
    }

    @Override
    public IRSlotTree suffix(IRSlotTree other) {
      return new IsValue(requisites, value, notValue, cont.suffix(other));
    }

    @Override
    protected void toStringHelper(StringBuilder sb) {
      boolean started = false;
      if (sb.isEmpty() && !cont.isLeaf()) {
        sb.append("[");
        started = true;
      }
      sb.append("isValue<").append(requisites).append(">[");
      sb.append("yes: ").append(value);
      sb.append(" | no: ").append(notValue);
      sb.append("]");
      if (!cont.isLeaf()) {
        sb.append("; ").append(cont);
      }
      if (started && !cont.isLeaf()) {
        sb.append("]");
      }
    }
  }
}
