package pt.inescid.cllsj.compiler.c;

import java.util.HashMap;
import java.util.Map;
import java.util.Optional;

public abstract class CAlignment {
  public static CAlignment one() {
    return constant(1);
  }

  public static CAlignment constant(int bytes) {
    return new CAlignmentConstant(bytes);
  }

  public static CAlignment expression(String expr) {
    return new CAlignmentExpression(expr);
  }

  public static CAlignment ternary(CCondition condition, CAlignment then, CAlignment otherwise) {
    return new CAlignmentTernary(condition, then, otherwise);
  }

  public CAlignment max(CAlignment other) {
    return new CAlignmentMax(this, other);
  }

  protected abstract CAlignment simplify(int max, Map<CCondition, Boolean> knownConditions);

  protected abstract String toExpression();

  public Optional<Integer> asConstant() {
    if (this instanceof CAlignmentConstant) {
      return Optional.of(((CAlignmentConstant) this).bytes);
    } else {
      return Optional.empty();
    }
  }

  @Override
  public String toString() {
    return simplify(0, Map.of()).toExpression();
  }

  @Override
  public boolean equals(Object obj) {
    return this.toString().equals(obj.toString());
  }

  private static class CAlignmentConstant extends CAlignment {
    public int bytes;

    public CAlignmentConstant(int bytes) {
      this.bytes = bytes;
    }

    @Override
    public String toExpression() {
      return Integer.toString(bytes);
    }

    @Override
    protected CAlignment simplify(int max, Map<CCondition, Boolean> knownConditions) {
      return constant(Math.max(bytes, max));
    }
  }

  private static class CAlignmentExpression extends CAlignment {
    private String expr;

    public CAlignmentExpression(String expr) {
      this.expr = expr;
    }

    @Override
    public String toExpression() {
      return expr;
    }

    @Override
    protected CAlignment simplify(int max, Map<CCondition, Boolean> knownConditions) {
      if (max <= 1) {
        return this;
      } else if (max >= 8) {
        return constant(max);
      } else {
        return this.max(constant(max));
      }
    }
  }

  private static class CAlignmentMax extends CAlignment {
    private CAlignment lhs;
    private CAlignment rhs;

    public CAlignmentMax(CAlignment lhs, CAlignment rhs) {
      this.lhs = lhs;
      this.rhs = rhs;
    }

    @Override
    public String toExpression() {
      return "MAX(" + lhs + ", " + rhs + ")";
    }

    @Override
    protected CAlignment simplify(int max, Map<CCondition, Boolean> knownConditions) {
      CAlignment lhs = this.lhs.simplify(max, knownConditions);
      CAlignment rhs = this.rhs.simplify(0, knownConditions);

      if (lhs instanceof CAlignmentConstant) {
        return this.rhs.simplify(((CAlignmentConstant) lhs).bytes, knownConditions);
      } else if (rhs instanceof CAlignmentConstant) {
        return this.lhs.simplify(((CAlignmentConstant) rhs).bytes, knownConditions);
      } else {
        return new CAlignmentMax(lhs, rhs);
      }
    }
  }

  private static class CAlignmentTernary extends CAlignment {
    private CCondition condition;
    private CAlignment then;
    private CAlignment otherwise;

    public CAlignmentTernary(CCondition condition, CAlignment then, CAlignment otherwise) {
      this.condition = condition;
      this.then = then;
      this.otherwise = otherwise;
    }

    @Override
    public String toExpression() {
      return "((" + condition + ") ? (" + then + ") : (" + otherwise + "))";
    }

    @Override
    protected CAlignment simplify(int max, Map<CCondition, Boolean> knownConditions) {
      if (knownConditions.containsKey(condition)) {
        if (knownConditions.get(condition)) {
          return then.simplify(max, knownConditions);
        } else {
          return otherwise.simplify(max, knownConditions);
        }
      }

      Map<CCondition, Boolean> newKnownConditions = new HashMap<>(knownConditions);
      newKnownConditions.put(condition, true);
      CAlignment then = this.then.simplify(max, newKnownConditions);
      newKnownConditions.put(condition, false);
      CAlignment otherwise = this.otherwise.simplify(max, newKnownConditions);
      if (then.equals(otherwise)) {
        return then;
      } else {
        return new CAlignmentTernary(condition, then, otherwise);
      }
    }
  }
}
