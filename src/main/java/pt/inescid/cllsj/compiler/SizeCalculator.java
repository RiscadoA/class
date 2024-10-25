package pt.inescid.cllsj.compiler;

import pt.inescid.cllsj.ast.ASTTypeVisitor;
import pt.inescid.cllsj.ast.types.ASTBotT;
import pt.inescid.cllsj.ast.types.ASTCaseT;
import pt.inescid.cllsj.ast.types.ASTOneT;
import pt.inescid.cllsj.ast.types.ASTRecvT;
import pt.inescid.cllsj.ast.types.ASTSendT;
import pt.inescid.cllsj.ast.types.ASTType;

public class SizeCalculator extends ASTTypeVisitor {
  private int size = -1;

  public static int calculate(ASTType type) {
    SizeCalculator calculator = new SizeCalculator();
    type.accept(calculator);
    return calculator.size;
  }

  @Override
  public void visit(ASTType type) {
    throw new UnsupportedOperationException(
        "Cannot estimate value size of type " + type.getClass().getSimpleName());
  }

  @Override
  public void visit(ASTBotT type) {
    size = 0;
  }

  @Override
  public void visit(ASTCaseT type) {
    int caseSize = 0;
    for (ASTType caseType : type.getcases().values()) {
      caseSize = Integer.max(caseSize, calculate(caseType));
    }
    size = 1 + caseSize; // 1 for the label
  }

  @Override
  public void visit(ASTOneT type) {
    size = 0;
  }

  @Override
  public void visit(ASTRecvT type) {
    size = 8; // 8 bytes for the pointer to the received value
    size += calculate(type.getrhs());
  }

  @Override
  public void visit(ASTSendT type) {
    size = 8; // 8 bytes for the pointer to the sent value
    size += calculate(type.getrhs());
  }
}
