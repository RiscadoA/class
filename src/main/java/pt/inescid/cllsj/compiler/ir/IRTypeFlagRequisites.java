package pt.inescid.cllsj.compiler.ir;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.function.BiFunction;
import java.util.function.Function;
import pt.inescid.cllsj.compiler.ir.id.IRTypeFlag;
import pt.inescid.cllsj.compiler.ir.id.IRTypeId;

public class IRTypeFlagRequisites {
  private Optional<Map<IRTypeId, Set<IRTypeFlag>>> types;

  public static IRTypeFlagRequisites impossible() {
    return new IRTypeFlagRequisites(Optional.empty());
  }

  public static IRTypeFlagRequisites onlyIf(Map<IRTypeId, Set<IRTypeFlag>> types) {
    return new IRTypeFlagRequisites(Optional.of(types));
  }

  public static IRTypeFlagRequisites onlyIf(IRTypeId type, IRTypeFlag flag) {
    return onlyIf(Map.of(type, Set.of(flag)));
  }

  public static IRTypeFlagRequisites guaranteed() {
    return onlyIf(Map.of());
  }

  private IRTypeFlagRequisites(Optional<Map<IRTypeId, Set<IRTypeFlag>>> types) {
    this.types = types;
  }

  public boolean isPossible() {
    return types.isPresent();
  }

  public boolean isGuaranteed() {
    return types.isPresent() && types.get().values().stream().allMatch(s -> s.isEmpty());
  }

  public boolean isUncertain() {
    return isPossible() && !isGuaranteed();
  }

  public Map<IRTypeId, Set<IRTypeFlag>> flagsByType() {
    return types.orElseThrow();
  }

  public IRTypeFlagRequisites replaceTypes(Function<IRTypeId, IRTypeId> replacer) {
    return expandTypes((id, flag) -> IRTypeFlagRequisites.onlyIf(replacer.apply(id), flag));
  }

  public IRTypeFlagRequisites expandTypes(
      BiFunction<IRTypeId, IRTypeFlag, IRTypeFlagRequisites> replacer) {
    if (types.isPresent()) {
      Map<IRTypeId, Set<IRTypeFlag>> newFlags = new HashMap<>();
      for (IRTypeId t : types.get().keySet()) {
        for (IRTypeFlag f : types.get().get(t)) {
          IRTypeFlagRequisites expanded = replacer.apply(t, f);
          if (!expanded.isPossible()) {
            return IRTypeFlagRequisites.impossible();
          }
          for (IRTypeId nt : expanded.flagsByType().keySet()) {
            newFlags
                .computeIfAbsent(nt, k -> new HashSet<>())
                .addAll(expanded.flagsByType().get(nt));
          }
        }
      }
      return IRTypeFlagRequisites.onlyIf(newFlags);
    } else {
      return this;
    }
  }

  @Override
  public String toString() {
    if (isGuaranteed()) {
      return "yes";
    } else if (isPossible()) {
      return types.get().entrySet().stream()
          .flatMap(e -> e.getValue().stream().map(f -> e.getKey() + "." + f))
          .reduce((a, b) -> a + ", " + b)
          .orElseThrow();
    } else {
      return "no";
    }
  }

  @Override
  public boolean equals(Object obj) {
    return toString().equals(obj.toString());
  }

  @Override
  public int hashCode() {
    return toString().hashCode();
  }
}
