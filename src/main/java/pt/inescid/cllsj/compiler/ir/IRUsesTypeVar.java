package pt.inescid.cllsj.compiler.ir;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import pt.inescid.cllsj.Env;
import pt.inescid.cllsj.EnvEntry;
import pt.inescid.cllsj.TypeEntry;
import pt.inescid.cllsj.ast.ASTTypeVisitor;
import pt.inescid.cllsj.ast.types.*;

public class IRUsesTypeVar extends ASTTypeVisitor {
  private Env<EnvEntry> ep;
  private boolean usesTypeVar = false;
  private String varName;
  private List<ASTType> visitedRecursive = new ArrayList<>();

  public static boolean check(Env<EnvEntry> ep, ASTType type, Set<String> varNames) {
    for (String varName : varNames) {
      if (check(ep, type, varName)) {
        return true;
      }
    }
    return false;
  }

  public static boolean check(Env<EnvEntry> ep, ASTType type, String varName) {
    IRUsesTypeVar v = new IRUsesTypeVar();
    v.ep = ep;
    v.varName = varName;
    type.accept(v);
    return v.usesTypeVar;
  }

  private void recurse(Env<EnvEntry> ep, ASTType type) {
    Env<EnvEntry> oldEp = this.ep;
    this.ep = ep;
    type.accept(this);
    this.ep = oldEp;
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
      // Mark the recursive type as visited to prevent infinite loops
      if (visitedRecursive.contains(type)) {
        return;
      }
      visitedRecursive.addLast(type);

      type.getin().accept(this);

      visitedRecursive.removeLast();
    }
  }

  @Override
  public void visit(ASTIdT idType) {
    ASTType type = idType.unfoldTypeCatch(ep);
    if (!(type instanceof ASTIdT)) {
      type.accept(this);
      return;
    }
    idType = (ASTIdT) type;

    if (idType.getid().equals(varName)) {
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
      // Mark the recursive type as visited to prevent infinite loops
      if (visitedRecursive.contains(type)) {
        return;
      }
      visitedRecursive.addLast(type);

      type.getin().accept(this);

      visitedRecursive.removeLast();
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
      recurse(ep.assoc(type.getid(), new TypeEntry(new ASTIdT(type.getid()))), type.getrhs());
    }
  }

  @Override
  public void visit(ASTRecvTT type) {
    if (!type.getid().equals(varName)) {
      recurse(ep.assoc(type.getid(), new TypeEntry(new ASTIdT(type.getid()))), type.getrhs());
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
