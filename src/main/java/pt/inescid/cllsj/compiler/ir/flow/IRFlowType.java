package pt.inescid.cllsj.compiler.ir.flow;

import java.util.Optional;
import pt.inescid.cllsj.compiler.ir.type.IRType;

public class IRFlowType {
  private Optional<Boolean> isPositive = Optional.empty();
  private Optional<IRType> type = Optional.empty();

  public IRFlowType() {}

  public IRFlowType(boolean isPositive) {
    this.isPositive = Optional.of(isPositive);
  }

  public IRFlowType(IRType type, boolean isPositive) {
    this.type = Optional.of(type);
    this.isPositive = Optional.of(isPositive);
  }

  public Optional<IRType> getType() {
    return type;
  }

  public Optional<Boolean> isPositive() {
    return isPositive;
  }

  public IRFlowType clone() {
    return this;
  }

  public IRFlowType merge(IRFlowType other) {
    IRFlowType merged = this.clone();
    if (!other.isPositive().equals(other.isPositive())) {
      merged.isPositive = Optional.empty();
    }
    if (!other.type.equals(type)) {
      merged.type = Optional.empty();
    }
    return clone();
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("positive=");
    if (isPositive.isPresent()) {
      if (isPositive.get()) {
        sb.append("true");
      } else {
        sb.append("false");
      }
    } else {
      sb.append("?");
    }
    sb.append(", type=");
    if (type.isPresent()) {
      sb.append(type.get().toString());
    } else {
      sb.append("?");
    }
    return sb.toString();
  }
}
