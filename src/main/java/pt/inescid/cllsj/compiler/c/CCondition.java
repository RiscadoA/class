package pt.inescid.cllsj.compiler.c;

import java.util.Optional;

public class CCondition {
  private Optional<Boolean> certain;
  private Optional<String> expr;

  public static CCondition certainlyTrue() {
    return new CCondition(Optional.of(true), Optional.empty());
  }

  public static CCondition certainlyFalse() {
    return new CCondition(Optional.of(false), Optional.empty());
  }

  public static CCondition maybe(String expr) {
    return new CCondition(Optional.empty(), Optional.of(expr));
  }

  private CCondition(Optional<Boolean> certain, Optional<String> expr) {
    this.certain = certain;
    this.expr = expr;
  }

  public boolean isCertainlyTrue() {
    return certain.isPresent() && certain.get();
  }

  public boolean isCertainlyFalse() {
    return certain.isPresent() && !certain.get();
  }

  public String expression() {
    return expr.orElseThrow();
  }
}
