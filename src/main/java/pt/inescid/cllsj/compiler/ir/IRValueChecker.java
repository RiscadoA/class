package pt.inescid.cllsj.compiler.ir;

import java.util.Optional;
import pt.inescid.cllsj.Env;
import pt.inescid.cllsj.EnvEntry;
import pt.inescid.cllsj.ast.ASTTypeVisitor;
import pt.inescid.cllsj.ast.types.*;
import pt.inescid.cllsj.compiler.Compiler;

public class IRValueChecker extends ASTTypeVisitor {
  private Compiler compiler;
  private Env<EnvEntry> ep;
  private IREnvironment env;
  private boolean isValue = true;
  private Optional<Boolean> polarity;

  public static boolean check(
      Compiler compiler, Env<EnvEntry> ep, IREnvironment env, ASTType type, boolean requiredPolarity) {
    return check(compiler, ep, env, type, Optional.of(requiredPolarity));
  }

  public static boolean check(
      Compiler compiler, Env<EnvEntry> ep, IREnvironment env, ASTType type, Optional<Boolean> requiredPolarity) {
    IRValueChecker visitor = new IRValueChecker();
    visitor.compiler = compiler;
    visitor.ep = ep;
    visitor.env = env;
    visitor.polarity = requiredPolarity;
    type.accept(visitor);
    return visitor.isValue;
  }

  private void expectPolarity(boolean polarity) {
    if (this.polarity.isPresent() && this.polarity.get() != polarity) {
      isValue = false;
    }
    this.polarity = Optional.of(polarity);
  }

  private void recurse(ASTType type) {
    if (isValue) {
      Optional<Boolean> savedPolarity = polarity;
      type.accept(this);
      polarity = savedPolarity;
    }
  }

  @Override
  public void visit(ASTBangT type) {
    expectPolarity(true);
  }

  @Override
  public void visit(ASTBotT type) {
    expectPolarity(false);
  }

  @Override
  public void visit(ASTCaseT type) {
    expectPolarity(true);
    for (ASTType c : type.getcases().values()) {
      recurse(c);
    }
  }

  @Override
  public void visit(ASTCoRecT type) {
    isValue = false;
  }

  @Override
  public void visit(ASTIdT type) {
    // Unfold the type to check if its definition is known
    ASTType unfolded;
    try {
      unfolded = type.unfoldType(ep);
    } catch (Exception e) {
      throw new IllegalArgumentException("Error unfolding type: " + e.getMessage());
    }
    if (!(unfolded instanceof ASTIdT)) {
      // We have a type definition, just recurse on the unfolded type
      unfolded.accept(this);
      return;
    }

    // We still have a type identifier
    type = (ASTIdT) unfolded;

    // Use the environment to check if the type is a value type
    IREnvironment.Type envType = env.getType(type.getid());
    if (!envType.isValue()) {
      isValue = false;
    } else {
      expectPolarity(envType.isPositive());
    }
  }

  @Override
  public void visit(ASTNotT type) {
    if (polarity.isPresent()) {
      polarity = Optional.of(!polarity.get());
    }
    recurse(type.getin());
  }

  @Override
  public void visit(ASTOfferT type) {
    expectPolarity(false);
    for (ASTType c : type.getcases().values()) {
      recurse(c);
    }
  }

  @Override
  public void visit(ASTOneT type) {
    expectPolarity(true);
  }

  @Override
  public void visit(ASTRecT type) {
    isValue = false;
  }

  @Override
  public void visit(ASTRecvT type) {
    if (!compiler.optimizeSendValue.get()) {
      isValue = false;
    } else {
      expectPolarity(false);
      recurse(type.getlhs());
      recurse(type.getrhs());
    }
  }

  @Override
  public void visit(ASTSendT type) {
    if (!compiler.optimizeSendValue.get()) {
      isValue = false;
    } else {
      expectPolarity(true);
      recurse(type.getlhs());
      recurse(type.getrhs());
    }
  }

  @Override
  public void visit(ASTWhyT type) {
    expectPolarity(false);
  }

  @Override
  public void visit(ASTintT type) {
    expectPolarity(true);
  }

  @Override
  public void visit(ASTCointT type) {
    expectPolarity(false);
  }

  @Override
  public void visit(ASTLintT type) {
    expectPolarity(true);
  }

  @Override
  public void visit(ASTLCointT type) {
    expectPolarity(false);
  }

  @Override
  public void visit(ASTLboolT type) {
    expectPolarity(true);
  }

  @Override
  public void visit(ASTCoLboolT type) {
    expectPolarity(false);
  }

  @Override
  public void visit(ASTLstringT type) {
    expectPolarity(true);
  }

  @Override
  public void visit(ASTCoLstringT type) {
    expectPolarity(false);
  }

  @Override
  public void visit(ASTSendTT type) {
    expectPolarity(true);
  }

  @Override
  public void visit(ASTRecvTT type) {
    expectPolarity(false);
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
    isValue = false;
  }

  @Override
  public void visit(ASTUsageT type) {
    isValue = false;
  }

  @Override
  public void visit(ASTCellLT type) {
    isValue = false;
  }

  @Override
  public void visit(ASTUsageLT type) {
    isValue = false;
  }
}
