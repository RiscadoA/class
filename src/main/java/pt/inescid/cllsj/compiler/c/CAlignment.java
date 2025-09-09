package pt.inescid.cllsj.compiler.c;

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

  public static CAlignment ternary(String condition, CAlignment then, CAlignment otherwise) {
    return expression("((" + condition + ") ? (" + then + ") : (" + otherwise + "))");
  }

  public CAlignment max(CAlignment other) {
    return new CAlignmentMax(this, other);
  }

  protected abstract CAlignment simplify(int max);

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
    return simplify(0).toExpression();
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
    protected CAlignment simplify(int max) {
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
    protected CAlignment simplify(int max) {
      if (max <= 1) {
        return this;
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
    protected CAlignment simplify(int max) {
      CAlignment lhs = this.lhs.simplify(max);
      CAlignment rhs = this.rhs.simplify(0);

      if (lhs instanceof CAlignmentConstant) {
        return rhs.simplify(((CAlignmentConstant) lhs).bytes);
      } else if (rhs instanceof CAlignmentConstant) {
        return lhs.simplify(((CAlignmentConstant) rhs).bytes);
      } else {
        return new CAlignmentMax(lhs, rhs);
      }
    }
  }
}
