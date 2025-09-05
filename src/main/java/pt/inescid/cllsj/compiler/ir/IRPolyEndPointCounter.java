package pt.inescid.cllsj.compiler.ir;

import pt.inescid.cllsj.ast.ASTTypeVisitor;
import pt.inescid.cllsj.ast.types.*;

public class IRPolyEndPointCounter extends ASTTypeVisitor {
  private int count = 0;

  public static int count(ASTType type) {
    IRPolyEndPointCounter counter = new IRPolyEndPointCounter();
    type.accept(counter);
    return counter.count;
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
      type.getCaseType(type.getLabel(i)).accept(this);
      maxCount = Math.max(maxCount, count);
    }
    count = oldCount + maxCount;
  }

  @Override
  public void visit(ASTCoRecT type) {
    type.getin().accept(this);
  }

  @Override
  public void visit(ASTIdT type) {
    count += 1;
  }

  @Override
  public void visit(ASTNotT type) {
    type.getin().accept(this);
  }

  @Override
  public void visit(ASTOfferT type) {
    int oldCount = count;
    int maxCount = 0;
    for (int i = 0; i < type.getcases().size(); ++i) {
      count = 0;
      type.getCaseType(type.getLabel(i)).accept(this);
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
    type.getin().accept(this);
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
    // TODO Auto-generated method stub
    throw new UnsupportedOperationException("Unimplemented method 'visit'");
  }

  @Override
  public void visit(ASTRecvTT type) {
    // TODO Auto-generated method stub
    throw new UnsupportedOperationException("Unimplemented method 'visit'");
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
