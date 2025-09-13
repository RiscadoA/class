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

  public String ternary(String ifTrue, String ifFalse) {
    if (isCertainlyTrue()) {
      return ifTrue;
    } else if (isCertainlyFalse()) {
      return ifFalse;
    } else {
      return "(" + expression() + " ? " + ifTrue + " : " + ifFalse + ")";
    }
  }

  public CSize ternary(CSize ifTrue, CSize ifFalse) {
    if (ifTrue.equals(ifFalse)) {
      return ifTrue;
    }

    if (isCertainlyTrue()) {
      return ifTrue;
    } else if (isCertainlyFalse()) {
      return ifFalse;
    } else {
      return CSize.expression(ternary(ifTrue.toString(), ifFalse.toString()));
    }
  }

  public CAlignment ternary(CAlignment ifTrue, CAlignment ifFalse) {
    if (ifTrue.equals(ifFalse)) {
      return ifTrue;
    }

    if (isCertainlyTrue()) {
      return ifTrue;
    } else if (isCertainlyFalse()) {
      return ifFalse;
    } else {
      return CAlignment.expression(ternary(ifTrue.toString(), ifFalse.toString()));
    }
  }

  public CLayout ternary(CLayout ifTrue, CLayout ifFalse) {
    return new CLayout(ternary(ifTrue.size, ifFalse.size), ternary(ifTrue.alignment, ifFalse.alignment));
  }

  @Override
  public String toString() {
    if (isCertainlyTrue()) {
      return "1";
    } else if (isCertainlyFalse()) {
      return "0";
    } else {
      return expression();
    }
  }
}
