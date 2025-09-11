package pt.inescid.cllsj.compiler.c;

import java.util.Optional;

public abstract class CSize {
  public static CSize zero() {
    return constant(0);
  }

  public static CSize constant(int bytes) {
    return new CSizeConstant(bytes);
  }

  public static CSize expression(String expr) {
    return new CSizeExpression(expr);
  }

  public static CSize sizeOf(String cType) {
    return expression("sizeof(" + cType + ")");
  }

  public static CSize ternary(String condition, CSize then, CSize otherwise) {
    return expression("((" + condition + ") ? (" + then + ") : (" + otherwise + "))");
  }

  public String advancePointer(String pointer) {
    String offset = toString();
    if (offset.equals("0")) {
      return pointer;
    } else {
      return pointer + " + " + offset;
    }
  }

  public String retreatPointer(String pointer) {
    String offset = toString();
    if (offset.equals("0")) {
      return pointer;
    } else {
      return pointer + " - (" + offset + ")";
    }
  }

  public CSize add(CSize other) {
    return new CSizeAdd(this, other);
  }

  public CSize subtract(CSize other) {
    return new CSizeSubtract(this, other);
  }

  public CSize multiply(int constant) {
    return multiply(constant(constant));
  }

  public CSize multiply(CSize other) {
    return new CSizeMultiply(this, other);
  }

  public CSize max(CSize other) {
    return new CSizeMax(this, other);
  }

  public CSize align(CAlignment alignment) {
    return new CSizeAlign(this, alignment);
  }

  public CSize padding(CAlignment alignment) {
    return new CSizePadding(this, alignment);
  }

  public Optional<Integer> asConstant() {
    if (this instanceof CSizeConstant) {
      return Optional.of(((CSizeConstant) this).bytes);
    } else {
      return Optional.empty();
    }
  }

  protected CSize addRemainder(int remainder) {
    if (remainder > 0) {
      return this.add(constant(remainder));
    }
    if (remainder < 0) {
      return this.subtract(constant(-remainder));
    } else {
      return this;
    }
  }

  protected abstract CSize simplify(int remainder);

  protected abstract String toExpression();

  @Override
  public String toString() {
    return simplify(0).toExpression();
  }

  private static class CSizeConstant extends CSize {
    public int bytes;

    public CSizeConstant(int bytes) {
      this.bytes = bytes;
    }

    @Override
    public String toExpression() {
      return Integer.toString(bytes);
    }

    @Override
    protected CSize simplify(int remainder) {
      return constant(bytes + remainder);
    }
  }

  private static class CSizeExpression extends CSize {
    private String expr;

    public CSizeExpression(String expr) {
      this.expr = expr;
    }

    @Override
    public String toExpression() {
      return expr;
    }

    @Override
    protected CSize simplify(int remainder) {
      return addRemainder(remainder);
    }
  }

  private static class CSizeAdd extends CSize {
    public CSize lhs;
    public CSize rhs;

    public CSizeAdd(CSize lhs, CSize rhs) {
      this.lhs = lhs;
      this.rhs = rhs;
    }

    @Override
    protected CSize simplify(int remainder) {
      CSize lhs = this.lhs.simplify(0);
      CSize rhs = this.rhs.simplify(remainder);

      if (lhs instanceof CSizeConstant) {
        return rhs.simplify(((CSizeConstant) lhs).bytes);
      }

      if (rhs instanceof CSizeConstant) {
        return lhs.simplify(((CSizeConstant) rhs).bytes);
      }

      return new CSizeAdd(lhs, rhs);
    }

    @Override
    public String toExpression() {
      return lhs + " + " + rhs;
    }
  }

  private static class CSizeSubtract extends CSize {
    public CSize lhs;
    public CSize rhs;

    public CSizeSubtract(CSize lhs, CSize rhs) {
      this.lhs = lhs;
      this.rhs = rhs;
    }

    @Override
    protected CSize simplify(int remainder) {
      CSize lhs = this.lhs.simplify(remainder > 0 ? remainder : 0);
      CSize rhs = this.rhs.simplify(remainder < 0 ? -remainder : 0);

      if (rhs instanceof CSizeConstant) {
        return lhs.simplify(-((CSizeConstant) rhs).bytes);
      }

      if (lhs.equals(rhs)) {
        return zero();
      } else {
        return new CSizeSubtract(lhs, rhs);
      }
    }

    @Override
    public String toExpression() {
      return lhs + " - (" + rhs + ")";
    }
  }

  private static class CSizeMultiply extends CSize {
    public CSize lhs;
    public CSize rhs;

    public CSizeMultiply(CSize lhs, CSize rhs) {
      this.lhs = lhs;
      this.rhs = rhs;
    }

    @Override
    protected CSize simplify(int remainder) {
      CSize lhs = this.lhs.simplify(0);
      CSize rhs = this.rhs.simplify(0);

      if (lhs instanceof CSizeConstant) {
        int bytes = ((CSizeConstant) lhs).bytes;
        if (bytes == 1) {
          return rhs.simplify(remainder);
        } else if (bytes == 0) {
          return constant(remainder);
        }
      }

      if (rhs instanceof CSizeConstant) {
        int bytes = ((CSizeConstant) rhs).bytes;
        if (bytes == 1) {
          return lhs.simplify(remainder);
        } else if (bytes == 0) {
          return constant(remainder);
        }
      }

      if (lhs instanceof CSizeConstant && rhs instanceof CSizeConstant) {
        int lhsBytes = ((CSizeConstant) lhs).bytes;
        int rhsBytes = ((CSizeConstant) rhs).bytes;
        return constant(lhsBytes * rhsBytes + remainder);
      }

      return lhs.multiply(rhs).addRemainder(remainder);
    }

    @Override
    public String toExpression() {
      return "(" + lhs + ") * (" + rhs + ")";
    }
  }

  private static class CSizeMax extends CSize {
    public CSize lhs;
    public CSize rhs;

    public CSizeMax(CSize lhs, CSize rhs) {
      this.lhs = lhs;
      this.rhs = rhs;
    }

    @Override
    protected CSize simplify(int remainder) {
      CSize lhs = this.lhs.simplify(remainder);
      CSize rhs = this.rhs.simplify(remainder);

      if (lhs instanceof CSizeConstant && rhs instanceof CSizeConstant) {
        return constant(Math.max(((CSizeConstant) lhs).bytes, ((CSizeConstant) rhs).bytes));
      }

      if (lhs.asConstant().isPresent() && lhs.asConstant().get() == 0) {
        return rhs;
      }

      if (rhs.asConstant().isPresent() && rhs.asConstant().get() == 0) {
        return lhs;
      }

      return lhs.max(rhs);
    }

    @Override
    public String toExpression() {
      return "MAX(" + lhs + ", " + rhs + ")";
    }
  }

  private static class CSizeAlign extends CSize {
    private CSize offset;
    private CAlignment alignment;

    public CSizeAlign(CSize offset, CAlignment alignment) {
      this.offset = offset;
      this.alignment = alignment;
    }

    @Override
    public String toExpression() {
      return "ALIGN(" + offset + ", " + alignment + ")";
    }

    @Override
    protected CSize simplify(int remainder) {
      CSize offset = this.offset.simplify(0);
      CAlignment alignment = this.alignment.simplify(0);

      if (alignment.asConstant().isPresent()) {
        if (alignment.asConstant().get() == 1) {
          return offset.simplify(remainder);
        }

        if (remainder % alignment.asConstant().get() == 0) {
          offset = offset.simplify(remainder);
          remainder = 0;
        }

        if (offset instanceof CSizeConstant) {
          int offsetBytes = ((CSizeConstant) offset).bytes;
          int alignmentBytes = alignment.asConstant().get();

          return constant(
                  offsetBytes
                      + ((alignmentBytes - (offsetBytes % alignmentBytes)) % alignmentBytes))
              .addRemainder(remainder);
        } else if (offset instanceof CSizeAlign) {
          CSizeAlign inner = (CSizeAlign) offset;
          return inner.offset.align(alignment.max(inner.alignment).simplify(0)).addRemainder(remainder);
        }
      }

      if (offset.asConstant().isPresent() && offset.asConstant().get() == 0) {
        return constant(remainder);
      }

      return offset.align(alignment).addRemainder(remainder);
    }
  }

  private static class CSizePadding extends CSize {
    private CSize offset;
    private CAlignment alignment;

    public CSizePadding(CSize offset, CAlignment alignment) {
      this.offset = offset;
      this.alignment = alignment;
    }

    @Override
    public String toExpression() {
      return "PADDING(" + offset + ", " + alignment + ")";
    }

    @Override
    protected CSize simplify(int remainder) {
      CSize offset = this.offset.simplify(0);
      CAlignment alignment = this.alignment.simplify(0);

      if (offset instanceof CSizeConstant && alignment.asConstant().isPresent()) {
        int offsetBytes = ((CSizeConstant) offset).bytes;
        int alignmentBytes = alignment.asConstant().get();

        return constant((alignmentBytes - (offsetBytes % alignmentBytes)) % alignmentBytes)
            .addRemainder(remainder);
      }

      return offset.padding(alignment).addRemainder(remainder);
    }
  }

  @Override
  public boolean equals(Object obj) {
    return toString().equals(obj.toString());
  }
}
