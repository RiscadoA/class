package pt.inescid.cllsj.compiler;

import java.util.Map;
import pt.inescid.cllsj.ast.ASTTypeVisitor;
import pt.inescid.cllsj.ast.types.ASTBangT;
import pt.inescid.cllsj.ast.types.ASTBotT;
import pt.inescid.cllsj.ast.types.ASTCaseT;
import pt.inescid.cllsj.ast.types.ASTCoRecT;
import pt.inescid.cllsj.ast.types.ASTIdT;
import pt.inescid.cllsj.ast.types.ASTNotT;
import pt.inescid.cllsj.ast.types.ASTOfferT;
import pt.inescid.cllsj.ast.types.ASTOneT;
import pt.inescid.cllsj.ast.types.ASTRecT;
import pt.inescid.cllsj.ast.types.ASTRecvT;
import pt.inescid.cllsj.ast.types.ASTSendT;
import pt.inescid.cllsj.ast.types.ASTType;
import pt.inescid.cllsj.ast.types.ASTWhyT;

public class SizeCalculator extends ASTTypeVisitor {
  private String size = "";
  private Map<String, String> vars;

  public static String calculate(ASTType type, Map<String, String> vars) {
    SizeCalculator calculator = new SizeCalculator(vars);
    type.accept(calculator);
    return calculator.size;
  }

  private SizeCalculator(Map<String, String> vars) {
    this.vars = vars;
  }

  @Override
  public void visit(ASTType type) {
    throw new UnsupportedOperationException(
        "Cannot estimate value size of type " + type.getClass().getSimpleName());
  }

  @Override
  public void visit(ASTBangT type) {
    size = "sizeof(int) + sizeof(struct environment*) + sizeof(void*)";
  }

  @Override
  public void visit(ASTBotT type) {
    size = "0";
  }

  @Override
  public void visit(ASTCaseT type) {
    this.visitBranching(type.getcases());
  }

  @Override
  public void visit(ASTCoRecT type) {
    assert !vars.containsKey(type.getid()) : "Type variable " + type.getid() + " already defined";
    vars.put(type.getid(), "0");
    type.getin().accept(this);
    vars.remove(type.getid());
  }

  @Override
  public void visit(ASTIdT type) {
    size = vars.get(type.getid());
    if (size == null)
      throw new IllegalArgumentException("Type variable " + type.getid() + " not defined");
  }

  @Override
  public void visit(ASTNotT type) {
    type.getin().accept(this);
  }

  @Override
  public void visit(ASTOfferT type) {
    this.visitBranching(type.getcases());
  }

  @Override
  public void visit(ASTOneT type) {
    size = "0";
  }

  @Override
  public void visit(ASTRecT type) {
    assert !vars.containsKey(type.getid()) : "Type variable " + type.getid() + " already defined";
    vars.put(type.getid(), "0");
    type.getin().accept(this);
    vars.remove(type.getid());
  }

  @Override
  public void visit(ASTRecvT type) {
    size = "sizeof(struct record*) + ";
    size += calculate(type.getrhs());
  }

  @Override
  public void visit(ASTSendT type) {
    size = "sizeof(struct record*) + ";
    size += calculate(type.getrhs());
  }

  @Override
  public void visit(ASTWhyT type) {
    size = "sizeof(int) + sizeof(struct environment*) + sizeof(void*)";
  }

  private void visitBranching(Map<String, ASTType> branches) {
    size = "sizeof(unsigned char)"; // for the label
    for (Map.Entry<String, ASTType> branch : branches.entrySet()) {
      // We're wasting memory doing this - we should do a max instead.
      // Since in C we can't have complex expressions such as a max in a constant expression, we'll
      // just add all the sizes.
      size += " + (/*" + branch.getKey() + "*/ " + calculate(branch.getValue(), this.vars) + ")";
    }
  }
}
