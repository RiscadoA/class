package pt.inescid.cllsj.compiler.ir;

import java.util.Set;
import pt.inescid.cllsj.ast.ASTTypeVisitor;
import pt.inescid.cllsj.ast.types.*;

public class IRPolyEndPointCounter extends ASTTypeVisitor {
  private int count = 0;
  private Set<String> polyNames;

  public static int count(ASTType type, Set<String> polyNames) {
    IRPolyEndPointCounter counter = new IRPolyEndPointCounter();
    counter.polyNames = polyNames;
    counter.recurse(type);
    return counter.count;
  }

  private void recurse(ASTType type) {
    if (IRUsesTypeVar.check(type, polyNames)) {
      type.accept(this);
    } else {
      count += 1;
    }
  }

  @Override
  public void visit(ASTBangT type) {
    // TODO Auto-generated method stub
    throw new UnsupportedOperationException("Unimplemented method 'visit'");
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
    recurse(type.getin());
  }

  @Override
  public void visit(ASTIdT type) {
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
    recurse(type.getin());
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
    // TODO Auto-generated method stub
    throw new UnsupportedOperationException("Unimplemented method 'visit'");
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
    // TODO Auto-generated method stub
    throw new UnsupportedOperationException("Unimplemented method 'visit'");
  }

  @Override
  public void visit(ASTCoAffineT type) {
    // TODO Auto-generated method stub
    throw new UnsupportedOperationException("Unimplemented method 'visit'");
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
