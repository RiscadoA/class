package pt.inescid.cllsj.compiler.ir;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class IRValueRequisites {
  private boolean canBeValue = false;
  private Map<Integer, Boolean> requiredTypePolarities;
  private List<Integer> typesWhichMustBeValues;

  public static IRValueRequisites notValue() {
    return new IRValueRequisites();
  }

  public static IRValueRequisites value(
      Map<Integer, Boolean> requiredTypePolarities, List<Integer> typesWhichMustBeValues) {
    IRValueRequisites requisites = new IRValueRequisites();
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

  public IRValueRequisites clone() {
    IRValueRequisites clone = new IRValueRequisites();
    clone.canBeValue = this.canBeValue;
    if (canBeValue) {
      clone.requiredTypePolarities = new HashMap<>(this.requiredTypePolarities);
      clone.typesWhichMustBeValues = new ArrayList<>(this.typesWhichMustBeValues);
    }
    return clone;
  }
}
