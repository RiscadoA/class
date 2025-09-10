package pt.inescid.cllsj.compiler.ir;

import pt.inescid.cllsj.ast.ASTTypeVisitor;
import pt.inescid.cllsj.ast.types.*;
import pt.inescid.cllsj.compiler.Compiler;

public class IRContinuationChecker extends ASTTypeVisitor {
  private IREnvironment env;
  private boolean mayHaveContinuation = false;
  private boolean isDual = false;

  public static boolean cannotHaveContinuation(Compiler compiler, IREnvironment env, ASTType type) {
    return !mayHaveContinuation(env, type);
  }

  public static boolean mayHaveContinuation(IREnvironment env, ASTType type) {
    IRContinuationChecker visitor = new IRContinuationChecker();
    visitor.env = env;
    type.accept(visitor);
    return visitor.mayHaveContinuation;
  }

  private void foundPolarity(boolean polarity) {
    if (this.isDual ^ polarity) {
      mayHaveContinuation = true;
    }
  }

  private void recurse(ASTType type) {
    boolean oldDual = isDual;
    type.accept(this);
    isDual = oldDual;
  }

  @Override
  public void visit(ASTBangT type) {
    foundPolarity(true);
  }

  @Override
  public void visit(ASTBotT type) {
    foundPolarity(false);
  }

  @Override
  public void visit(ASTCaseT type) {
    foundPolarity(true);
    for (ASTType c : type.getcases().values()) {
      recurse(c);
    }
  }

  @Override
  public void visit(ASTCoRecT type) {
    mayHaveContinuation = true;
  }

  @Override
  public void visit(ASTIdT type) {
    // Unfold the type to check if its definition is known
    ASTType unfolded;
    try {
      unfolded = type.unfoldType(env.getEp());
    } catch (Exception e) {
      throw new IllegalArgumentException("Error unfolding type: " + e.getMessage());
    }
    if (!(unfolded instanceof ASTIdT)) {
      // We have a type definition, just recurse on the unfolded type
      unfolded.accept(this);
      return;
    }

    // We still have a type identifier, thus this is a polymorphic session and not a value
    mayHaveContinuation = true;
  }

  @Override
  public void visit(ASTNotT type) {
    isDual = !isDual;
    recurse(type.getin());
  }

  @Override
  public void visit(ASTOfferT type) {
    foundPolarity(false);
    for (ASTType c : type.getcases().values()) {
      recurse(c);
    }
  }

  @Override
  public void visit(ASTOneT type) {
    foundPolarity(true);
  }

  @Override
  public void visit(ASTRecT type) {
    mayHaveContinuation = true;
  }

  @Override
  public void visit(ASTRecvT type) {
    foundPolarity(false);
    recurse(type.getrhs());
  }

  @Override
  public void visit(ASTSendT type) {
    foundPolarity(true);
    recurse(type.getrhs());
  }

  @Override
  public void visit(ASTWhyT type) {
    foundPolarity(false);
  }

  @Override
  public void visit(ASTintT type) {
    foundPolarity(true);
  }

  @Override
  public void visit(ASTCointT type) {
    foundPolarity(false);
  }

  @Override
  public void visit(ASTLintT type) {
    foundPolarity(true);
  }

  @Override
  public void visit(ASTLCointT type) {
    foundPolarity(false);
  }

  @Override
  public void visit(ASTLboolT type) {
    foundPolarity(true);
  }

  @Override
  public void visit(ASTCoLboolT type) {
    foundPolarity(false);
  }

  @Override
  public void visit(ASTLstringT type) {
    foundPolarity(true);
  }

  @Override
  public void visit(ASTCoLstringT type) {
    foundPolarity(false);
  }

  @Override
  public void visit(ASTSendTT type) {
    foundPolarity(true);
  }

  @Override
  public void visit(ASTRecvTT type) {
    foundPolarity(false);
  }

  @Override
  public void visit(ASTAffineT type) {
    throw new UnsupportedOperationException("Affine types should no longer exist at this stage");
  }

  @Override
  public void visit(ASTCoAffineT type) {
    throw new UnsupportedOperationException("Affine types should no longer exist at this stage");
  }

  @Override
  public void visit(ASTCellT type) {
    mayHaveContinuation = true;
  }

  @Override
  public void visit(ASTUsageT type) {
    mayHaveContinuation = true;
  }

  @Override
  public void visit(ASTCellLT type) {
    mayHaveContinuation = true;
  }

  @Override
  public void visit(ASTUsageLT type) {
    mayHaveContinuation = true;
  }
}
