package pt.inescid.cllsj.compiler.ir;

import java.util.Set;
import pt.inescid.cllsj.ast.ASTTypeVisitor;
import pt.inescid.cllsj.ast.types.*;
import pt.inescid.cllsj.compiler.Compiler;

public class IRPolyEndPointCounter extends ASTTypeVisitor {
  private Compiler compiler;
  private IREnvironment env;
  private int count = 0;
  private Set<String> polyNames;

  public static int count(
      Compiler compiler, IREnvironment env, ASTType type, Set<String> polyNames) {
    IRPolyEndPointCounter counter = new IRPolyEndPointCounter();
    counter.compiler = compiler;
    counter.env = env;
    counter.polyNames = polyNames;
    counter.recurse(type);
    return counter.count;
  }

  private void recurse(ASTType type) {
    if (IRUsesTypeVar.check(env.getEp(), type, polyNames)) {
      type.accept(this);
    } else {
      count += 1;
    }
  }

  @Override
  public void visit(ASTBangT type) {
    if (IRValueChecker.check(compiler, env, type.getin(), true)) {
      type.getin().accept(this);
    } else {
      count += 1;
    }
  }

  @Override
  public void visit(ASTBotT type) {
    count += 1;
  }

  @Override
  public void visit(ASTCaseT type) {
    int oldCount = count;
    int maxCount = 0;
    for (int i = 0; i < type.getcases().size(); ++i) {
      count = 0;
      recurse(type.getCaseType(type.getLabel(i)));
      maxCount = Math.max(maxCount, count);
    }
    count = oldCount + maxCount;
  }

  @Override
  public void visit(ASTCoRecT type) {
    count += 1;
  }

  @Override
  public void visit(ASTIdT idType) {
    ASTType type = idType.unfoldTypeCatch(env.getEp());
    if (!(type instanceof ASTIdT)) {
      type.accept(this);
      return;
    }

    count += 1;
  }

  @Override
  public void visit(ASTNotT type) {
    recurse(type.getin());
  }

  @Override
  public void visit(ASTOfferT type) {
    int oldCount = count;
    int maxCount = 0;
    for (int i = 0; i < type.getcases().size(); ++i) {
      count = 0;
      recurse(type.getCaseType(type.getLabel(i)));
      maxCount = Math.max(maxCount, count);
    }
    count = oldCount + maxCount;
  }

  @Override
  public void visit(ASTOneT type) {
    count += 1;
  }

  @Override
  public void visit(ASTRecT type) {
    count += 1;
  }

  @Override
  public void visit(ASTRecvT type) {
    recurse(type.getlhs());
    recurse(type.getrhs());
  }

  @Override
  public void visit(ASTSendT type) {
    recurse(type.getlhs());
    recurse(type.getrhs());
  }

  @Override
  public void visit(ASTWhyT type) {
    if (IRValueChecker.check(compiler, env, type.getin(), false)) {
      type.getin().accept(this);
    } else {
      count += 1;
    }
  }

  @Override
  public void visit(ASTintT type) {
    count += 1;
  }

  @Override
  public void visit(ASTCointT type) {
    count += 1;
  }

  @Override
  public void visit(ASTLintT type) {
    count += 1;
  }

  @Override
  public void visit(ASTLCointT type) {
    count += 1;
  }

  @Override
  public void visit(ASTLboolT type) {
    count += 1;
  }

  @Override
  public void visit(ASTCoLboolT type) {
    count += 1;
  }

  @Override
  public void visit(ASTLstringT type) {
    count += 1;
  }

  @Override
  public void visit(ASTCoLstringT type) {
    count += 1;
  }

  @Override
  public void visit(ASTSendTT type) {
    count += 1;
  }

  @Override
  public void visit(ASTRecvTT type) {
    count += 1;
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
    // TODO Auto-generated method stub
    throw new UnsupportedOperationException("Unimplemented method 'visit'");
  }

  @Override
  public void visit(ASTUsageT type) {
    // TODO Auto-generated method stub
    throw new UnsupportedOperationException("Unimplemented method 'visit'");
  }

  @Override
  public void visit(ASTCellLT type) {
    // TODO Auto-generated method stub
    throw new UnsupportedOperationException("Unimplemented method 'visit'");
  }

  @Override
  public void visit(ASTUsageLT type) {
    // TODO Auto-generated method stub
    throw new UnsupportedOperationException("Unimplemented method 'visit'");
  }
}
