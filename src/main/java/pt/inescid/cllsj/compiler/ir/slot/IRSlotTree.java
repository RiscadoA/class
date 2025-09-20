package pt.inescid.cllsj.compiler.ir.slot;

import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.function.BiFunction;
import java.util.function.Function;
import pt.inescid.cllsj.compiler.ir.IRTypeFlagRequisites;
import pt.inescid.cllsj.compiler.ir.id.IRTypeFlag;
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

  public static IRSlotTree type(IRTypeFlagRequisites requisites, IRSlotTree set, IRSlotTree unset) {
    if (requisites.isGuaranteed()) {
      return set;
    } else if (requisites.isPossible()) {
      return new Type(requisites, set, unset);
    } else {
      return unset;
    }
  }

  protected abstract void toStringHelper(StringBuilder sb);

  public abstract IRSlotTree replaceTypes(
      Function<IRTypeId, IRSlotTree> slotReplacer,
      BiFunction<IRTypeId, IRTypeFlag, IRTypeFlagRequisites> reqReplacer);

  public boolean isPolymorphic() {
    AtomicBoolean polymorphic = new AtomicBoolean(false);
    replaceTypes(
        typeId -> {
          polymorphic.set(true);
          return LEAF;
        },
        (typeId, flag) -> {
          polymorphic.set(true);
          return IRTypeFlagRequisites.impossible();
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

  public boolean isType() {
    return this instanceof Type;
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
        BiFunction<IRTypeId, IRTypeFlag, IRTypeFlagRequisites> reqReplacer) {
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
        BiFunction<IRTypeId, IRTypeFlag, IRTypeFlagRequisites> reqReplacer) {
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
        BiFunction<IRTypeId, IRTypeFlag, IRTypeFlagRequisites> reqReplacer) {
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

  public static class Type extends IRSlotTree {
    private IRTypeFlagRequisites requisites;
    private IRSlotTree met;
    private IRSlotTree unmet;

    public Type(IRTypeFlagRequisites requisites, IRSlotTree met, IRSlotTree unmet) {
      this.requisites = requisites;
      this.met = met;
      this.unmet = unmet;
    }

    public IRTypeFlagRequisites requisites() {
      return requisites;
    }

    public IRSlotTree met() {
      return met;
    }

    public IRSlotTree unmet() {
      return unmet;
    }

    @Override
    public Set<IRSlot> head() {
      Set<IRSlot> heads = new HashSet<>();
      heads.addAll(met.head());
      heads.addAll(unmet.head());
      return heads;
    }

    @Override
    public Optional<IRSlot> singleHead() {
      return Optional.empty();
    }

    @Override
    public IRSlotTree suffix(IRSlotTree other) {
      return new Type(requisites, met.suffix(other), unmet.suffix(other));
    }

    @Override
    public IRSlotTree replaceTypes(
        Function<IRTypeId, IRSlotTree> slotReplacer,
        BiFunction<IRTypeId, IRTypeFlag, IRTypeFlagRequisites> reqReplacer) {
      return type(
          requisites.expandTypes(reqReplacer),
          met.replaceTypes(slotReplacer, reqReplacer),
          unmet.replaceTypes(slotReplacer, reqReplacer));
    }

    @Override
    protected void toStringHelper(StringBuilder sb) {
      sb.append("type<").append(requisites).append(">[");
      sb.append(met);
      sb.append(" | ");
      sb.append(unmet);
      sb.append("]");
    }
  }
}
