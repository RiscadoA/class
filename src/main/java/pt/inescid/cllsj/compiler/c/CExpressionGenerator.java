package pt.inescid.cllsj.compiler.c;

import java.util.function.Function;
import pt.inescid.cllsj.compiler.ir.expression.IRClone;
import pt.inescid.cllsj.compiler.ir.expression.IRExpression;
import pt.inescid.cllsj.compiler.ir.expression.IRExpressionVisitor;
import pt.inescid.cllsj.compiler.ir.expression.IRMove;
import pt.inescid.cllsj.compiler.ir.expression.arithmetic.IRAdd;
import pt.inescid.cllsj.compiler.ir.expression.arithmetic.IRDivide;
import pt.inescid.cllsj.compiler.ir.expression.arithmetic.IRModulo;
import pt.inescid.cllsj.compiler.ir.expression.arithmetic.IRMultiply;
import pt.inescid.cllsj.compiler.ir.expression.arithmetic.IRSubtract;
import pt.inescid.cllsj.compiler.ir.expression.bool.IRAnd;
import pt.inescid.cllsj.compiler.ir.expression.bool.IREqual;
import pt.inescid.cllsj.compiler.ir.expression.bool.IRGreaterThan;
import pt.inescid.cllsj.compiler.ir.expression.bool.IRLessThan;
import pt.inescid.cllsj.compiler.ir.expression.bool.IRNot;
import pt.inescid.cllsj.compiler.ir.expression.bool.IROr;
import pt.inescid.cllsj.compiler.ir.expression.literal.IRBoolLiteral;
import pt.inescid.cllsj.compiler.ir.expression.literal.IRIntLiteral;
import pt.inescid.cllsj.compiler.ir.expression.literal.IRStringLiteral;
import pt.inescid.cllsj.compiler.ir.slot.IRBoolS;
import pt.inescid.cllsj.compiler.ir.slot.IRIntS;
import pt.inescid.cllsj.compiler.ir.slot.IRStringS;

public class CExpressionGenerator extends IRExpressionVisitor {
  private Function<IRMove, String> moveGenerator;
  private Function<IRClone, String> cloneGenerator;
  private StringBuilder code = new StringBuilder("");

  public static String generate(
      IRExpression expression,
      Function<IRMove, String> moveGenerator,
      Function<IRClone, String> cloneGenerator) {
    CExpressionGenerator gen = new CExpressionGenerator();
    gen.moveGenerator = moveGenerator;
    gen.cloneGenerator = cloneGenerator;
    expression.accept(gen);
    return gen.code.toString();
  }

  public static String generateToString(
      IRExpression expr,
      Function<IRMove, String> moveGenerator,
      Function<IRClone, String> cloneGenerator) {
    String result = generate(expr, moveGenerator, cloneGenerator);

    if (expr.getSlot() instanceof IRIntS) {
      return "string_from_int(" + result + ")";
    } else if (expr.getSlot() instanceof IRBoolS) {
      return "string_clone(" + result + " ? \"true\" : \"false\")";
    } else if (expr.getSlot() instanceof IRStringS) {
      return result;
    } else {
      throw new UnsupportedOperationException(
          "Unsupported expression slot: " + expr.getSlot().getClass().getName());
    }
  }

  private String recurseToString(IRExpression expr) {
    return generateToString(expr, moveGenerator, cloneGenerator);
  }

  private void binary(String op, IRExpression lhs, IRExpression rhs) {
    code.append("(");
    lhs.accept(this);
    code.append(" ").append(op).append(" ");
    rhs.accept(this);
    code.append(")");
  }

  @Override
  public void visit(IRIntLiteral lit) {
    code.append(lit.getValue());
  }

  @Override
  public void visit(IRBoolLiteral lit) {
    code.append(lit.getValue() ? "1" : "0");
  }

  @Override
  public void visit(IRStringLiteral lit) {
    code.append("string_clone(\"").append(lit.getValue()).append("\")");
  }

  @Override
  public void visit(IRMove move) {
    code.append(moveGenerator.apply(move));
  }

  @Override
  public void visit(IRClone clone) {
    code.append(cloneGenerator.apply(clone));
  }

  @Override
  public void visit(IRAdd add) {
    if (add.getSlot() instanceof IRStringS) {
      code.append("string_concat(");
      code.append(recurseToString(add.getLhs()));
      code.append(", ");
      code.append(recurseToString(add.getRhs()));
      code.append(")");
    } else {
      binary("+", add.getLhs(), add.getRhs());
    }
  }

  @Override
  public void visit(IRSubtract sub) {
    binary("-", sub.getLhs(), sub.getRhs());
  }

  @Override
  public void visit(IRMultiply mul) {
    binary("*", mul.getLhs(), mul.getRhs());
  }

  @Override
  public void visit(IRDivide div) {
    binary("/", div.getLhs(), div.getRhs());
  }

  @Override
  public void visit(IRModulo mod) {
    binary("%", mod.getLhs(), mod.getRhs());
  }

  @Override
  public void visit(IREqual eq) {
    if (eq.getLhs().getSlot() instanceof IRStringS || eq.getRhs().getSlot() instanceof IRStringS) {
      code.append("string_equal(");
      code.append(recurseToString(eq.getLhs()));
      code.append(", ");
      code.append(recurseToString(eq.getRhs()));
      code.append(")");
    } else {
      binary("==", eq.getLhs(), eq.getRhs());
    }
  }

  @Override
  public void visit(IRLessThan lt) {
    binary("<", lt.getLhs(), lt.getRhs());
  }

  @Override
  public void visit(IRGreaterThan gt) {
    binary(">", gt.getLhs(), gt.getRhs());
  }

  @Override
  public void visit(IRAnd and) {
    binary("&&", and.getLhs(), and.getRhs());
  }

  @Override
  public void visit(IROr or) {
    binary("||", or.getLhs(), or.getRhs());
  }

  @Override
  public void visit(IRNot not) {
    code.append("(!");
    not.getInner().accept(this);
    code.append(")");
  }
}
