package pt.inescid.cllsj.compiler.ir;

import java.util.Set;
import pt.inescid.cllsj.ast.ASTTypeVisitor;
import pt.inescid.cllsj.ast.types.*;

public class IRUsesTypeVar extends ASTTypeVisitor {
  private boolean usesTypeVar = false;
  private String varName;

  public static boolean check(ASTType type, Set<String> varNames) {
    for (String varName : varNames) {
      if (check(type, varName)) {
        return true;
      }
    }
    return false;
  }

  public static boolean check(ASTType type, String varName) {
    IRUsesTypeVar v = new IRUsesTypeVar();
    v.varName = varName;
    type.accept(v);
    return v.usesTypeVar;
  }

  @Override
  public void visit(ASTBangT type) {
    type.getin().accept(this);
  }

  @Override
  public void visit(ASTBotT type) {}

  @Override
  public void visit(ASTCaseT type) {
    for (ASTType t : type.getcases().values()) {
      t.accept(this);
    }
  }

  @Override
  public void visit(ASTCoRecT type) {
    if (!type.getid().equals(varName)) {
      type.getin().accept(this);
    }
  }

  @Override
  public void visit(ASTIdT type) {
    if (type.getid().equals(varName)) {
      usesTypeVar = true;
    }
  }

  @Override
  public void visit(ASTNotT type) {
    type.getin().accept(this);
  }

  @Override
  public void visit(ASTOfferT type) {
    for (ASTType t : type.getcases().values()) {
      t.accept(this);
    }
  }

  @Override
  public void visit(ASTOneT type) {}

  @Override
  public void visit(ASTRecT type) {
    if (!type.getid().equals(varName)) {
      type.getin().accept(this);
    }
  }

  @Override
  public void visit(ASTRecvT type) {
    type.getlhs().accept(this);
    type.getrhs().accept(this);
  }

  @Override
  public void visit(ASTSendT type) {
    type.getlhs().accept(this);
    type.getrhs().accept(this);
  }

  @Override
  public void visit(ASTWhyT type) {
    type.getin().accept(this);
  }

  @Override
  public void visit(ASTintT type) {}

  @Override
  public void visit(ASTCointT type) {}

  @Override
  public void visit(ASTLintT type) {}

  @Override
  public void visit(ASTLCointT type) {}

  @Override
  public void visit(ASTLboolT type) {}

  @Override
  public void visit(ASTCoLboolT type) {}

  @Override
  public void visit(ASTLstringT type) {}

  @Override
  public void visit(ASTCoLstringT type) {}

  @Override
  public void visit(ASTSendTT type) {
    if (!type.getid().equals(varName)) {
      type.getrhs().accept(this);
    }
  }

  @Override
  public void visit(ASTRecvTT type) {
    if (!type.getid().equals(varName)) {
      type.getrhs().accept(this);
    }
  }

  @Override
  public void visit(ASTAffineT type) {
    type.getin().accept(this);
  }

  @Override
  public void visit(ASTCoAffineT type) {
    type.getin().accept(this);
  }

  @Override
  public void visit(ASTCellT type) {
    type.getin().accept(this);
  }

  @Override
  public void visit(ASTUsageT type) {
    type.getin().accept(this);
  }

  @Override
  public void visit(ASTCellLT type) {
    type.getin().accept(this);
  }

  @Override
  public void visit(ASTUsageLT type) {
    type.getin().accept(this);
  }
}
