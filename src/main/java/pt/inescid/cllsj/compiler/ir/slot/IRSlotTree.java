package pt.inescid.cllsj.compiler.ir.slot;

import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.function.Function;
import pt.inescid.cllsj.compiler.ir.IRValueRequisites;
import pt.inescid.cllsj.compiler.ir.id.IRTypeId;

public abstract class IRSlotTree {
  public static final IRSlotTree LEAF = new Leaf();

  public static Unary of(IRSlot slot, IRSlotTree child) {
    return new Unary(slot, child);
  }

  public static IRSlotTree of(IRSlot... slots) {
    IRSlotTree result = LEAF;
    for (int i = slots.length - 1; i >= 0; i--) {
      result = of(slots[i], result);
    }
    return result;
  }

  public static Tag tag(List<IRSlotTree> cases) {
    return new Tag(cases);
  }

  public static IRSlotTree isValue(
      IRValueRequisites requisites, IRSlotTree value, IRSlotTree notValue) {
    if (requisites.mustBeValue()) {
      return value;
    } else if (requisites.canBeValue()) {
      return new IsValue(requisites, value, notValue);
    } else {
      return notValue;
    }
  }

  protected abstract void toStringHelper(StringBuilder sb);

  public abstract IRSlotTree replaceTypes(
      Function<IRTypeId, IRSlotTree> slotReplacer,
      Function<IRTypeId, IRValueRequisites> reqReplacer);

  public boolean isPolymorphic() {
    AtomicBoolean polymorphic = new AtomicBoolean(false);
    replaceTypes(
        typeId -> {
          polymorphic.set(true);
          return LEAF;
        },
        typeId -> {
          polymorphic.set(true);
          return IRValueRequisites.notValue();
        });
    return polymorphic.get();
  }

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

  @Override
  public boolean equals(Object obj) {
    return this.toString().equals(obj.toString());
  }

  @Override
  public int hashCode() {
    return this.toString().hashCode();
  }

  public static class Leaf extends IRSlotTree {
    @Override
    public Optional<IRSlot> singleHead() {
      return Optional.empty();
    }

    @Override
    public IRSlotTree suffix(IRSlotTree other) {
      return other;
    }

    @Override
    public IRSlotTree replaceTypes(
        Function<IRTypeId, IRSlotTree> slotReplacer,
        Function<IRTypeId, IRValueRequisites> reqReplacer) {
      return this;
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
    public Optional<IRSlot> singleHead() {
      return Optional.of(slot);
    }

    @Override
    public IRSlotTree suffix(IRSlotTree other) {
      return new Unary(slot, child.suffix(other));
    }

    @Override
    public IRSlotTree replaceTypes(
        Function<IRTypeId, IRSlotTree> slotReplacer,
        Function<IRTypeId, IRValueRequisites> reqReplacer) {
      return slot.replaceTypes(slotReplacer, reqReplacer)
          .suffix(child.replaceTypes(slotReplacer, reqReplacer));
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
    public Optional<IRSlot> singleHead() {
      return Optional.of(new IRTagS());
    }

    @Override
    public IRSlotTree suffix(IRSlotTree other) {
      return new Tag(cases.stream().map(c -> c.suffix(other)).toList());
    }

    @Override
    public IRSlotTree replaceTypes(
        Function<IRTypeId, IRSlotTree> slotReplacer,
        Function<IRTypeId, IRValueRequisites> reqReplacer) {
      return new Tag(cases.stream().map(c -> c.replaceTypes(slotReplacer, reqReplacer)).toList());
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

    public IsValue(IRValueRequisites requisites, IRSlotTree value, IRSlotTree notValue) {
      this.requisites = requisites;
      this.value = value;
      this.notValue = notValue;
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

    @Override
    public Set<IRSlot> head() {
      Set<IRSlot> heads = new HashSet<>();
      heads.addAll(value.head());
      heads.addAll(notValue.head());
      return heads;
    }

    @Override
    public Optional<IRSlot> singleHead() {
      return Optional.empty();
    }

    @Override
    public IRSlotTree suffix(IRSlotTree other) {
      return new IsValue(requisites, value.suffix(other), notValue.suffix(other));
    }

    @Override
    public IRSlotTree replaceTypes(
        Function<IRTypeId, IRSlotTree> slotReplacer,
        Function<IRTypeId, IRValueRequisites> reqReplacer) {
      return isValue(
          requisites.expandTypes(reqReplacer),
          value.replaceTypes(slotReplacer, reqReplacer),
          notValue.replaceTypes(slotReplacer, reqReplacer));
    }

    @Override
    protected void toStringHelper(StringBuilder sb) {
      sb.append("isValue<").append(requisites).append(">[");
      sb.append(value).append(" | ").append(notValue);
      sb.append("]");
    }
  }
}
