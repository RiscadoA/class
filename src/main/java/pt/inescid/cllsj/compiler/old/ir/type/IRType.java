package pt.inescid.cllsj.compiler.ir.old.type;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.BiFunction;

import pt.inescid.cllsj.compiler.old.ir.IRTypeVisitor;

public abstract class IRType {
  public static class ValueRequisites {
    private boolean canBeValue = false;
    private Map<Integer, Boolean> requiredTypePolarities;
    private List<Integer> typesWhichMustBeValues;

    public static ValueRequisites notValue() {
      return new ValueRequisites();
    }

    public static ValueRequisites value(
        Map<Integer, Boolean> requiredTypePolarities, List<Integer> typesWhichMustBeValues) {
      ValueRequisites requisites = new ValueRequisites();
      requisites.canBeValue = true;
      requisites.requiredTypePolarities = requiredTypePolarities;
      requisites.typesWhichMustBeValues = typesWhichMustBeValues;
      return requisites;
    }

    public boolean canBeValue() {
      return canBeValue;
    }

    public boolean mustBeValue() {
      return canBeValue && typesWhichMustBeValues.isEmpty() && requiredTypePolarities.isEmpty();
    }

    public Map<Integer, Boolean> getRequiredTypePolarities() {
      return requiredTypePolarities;
    }

    public List<Integer> getTypesWhichMustBeValues() {
      return typesWhichMustBeValues;
    }

    @Override
    public String toString() {
      if (canBeValue) {
        StringBuilder sb = new StringBuilder("value");
        if (!typesWhichMustBeValues.isEmpty()) {
          sb.append("(");
        }
        for (int i = 0; i < typesWhichMustBeValues.size(); i++) {
          if (i > 0) {
            sb.append(", ");
          }
          if (requiredTypePolarities.containsKey(i)) {
            if (requiredTypePolarities.get(i)) {
              sb.append("+");
            } else {
              sb.append("-");
            }
          }
          sb.append(typesWhichMustBeValues.get(i));
        }
        if (!typesWhichMustBeValues.isEmpty()) {
          sb.append(")");
        }
        return sb.toString();
      } else {
        return "notValue";
      }
    }

    public ValueRequisites merge(ValueRequisites other) {
      if (!canBeValue() || !other.canBeValue()) {
        return ValueRequisites.notValue();
      }

      Map<Integer, Boolean> requiredTypePolarities = new HashMap<>(this.requiredTypePolarities);
      for (Map.Entry<Integer, Boolean> e : other.requiredTypePolarities.entrySet()) {
        if (!requiredTypePolarities.containsKey(e.getKey())) {
          requiredTypePolarities.put(e.getKey(), e.getValue());
        } else if (!requiredTypePolarities.get(e.getKey()).equals(e.getValue())) {
          return ValueRequisites.notValue();
        }
      }

      List<Integer> typesWhichMustBeValues = new ArrayList<>(this.typesWhichMustBeValues);
      for (int t : other.typesWhichMustBeValues) {
        if (!typesWhichMustBeValues.contains(t)) {
          typesWhichMustBeValues.add(t);
        }
      }

      return ValueRequisites.value(requiredTypePolarities, typesWhichMustBeValues);
    }
  }

  public void accept(IRTypeVisitor visitor) {
    visitor.visit(this);
  }

  // Gets the requisites for the given type to be a value.
  public ValueRequisites valueRequisites() {
    return ValueRequisites.value(Map.of(), List.of());
  }

  // Returns the leftmost type ending. For tail types, this is the type itself.
  // For session types, it is the tail of the left hand side of the session.
  public IRType leftmostTail() {
    return this;
  }

  // Returns the rightmost type ending. For tail types, this is the type itself.
  // For session types, it is the tail of the right hand side of the session, unless the right hand
  // side
  // is a close, in which case the session type itself is returned.
  public IRType rightmostTail() {
    return this;
  }

  // Substitutes the variable with the given index with the given type.
  public abstract IRType substituteVar(
      int index, int offset, BiFunction<Integer, IRVarT, IRType> types);

  public abstract boolean equals(IRType other);

  @Override
  public boolean equals(Object obj) {
    if (obj instanceof IRType) {
      return ((IRType) obj).equals(this);
    } else {
      return false;
    }
  }
}
