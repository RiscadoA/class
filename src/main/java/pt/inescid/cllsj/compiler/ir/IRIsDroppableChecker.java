package pt.inescid.cllsj.compiler.ir;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import pt.inescid.cllsj.ast.ASTTypeVisitor;
import pt.inescid.cllsj.ast.types.ASTAffineT;
import pt.inescid.cllsj.ast.types.ASTBangT;
import pt.inescid.cllsj.ast.types.ASTBotT;
import pt.inescid.cllsj.ast.types.ASTCaseT;
import pt.inescid.cllsj.ast.types.ASTCellLT;
import pt.inescid.cllsj.ast.types.ASTCellT;
import pt.inescid.cllsj.ast.types.ASTCoAffineT;
import pt.inescid.cllsj.ast.types.ASTCoLboolT;
import pt.inescid.cllsj.ast.types.ASTCoLstringT;
import pt.inescid.cllsj.ast.types.ASTCoRecT;
import pt.inescid.cllsj.ast.types.ASTCointT;
import pt.inescid.cllsj.ast.types.ASTIdT;
import pt.inescid.cllsj.ast.types.ASTLCointT;
import pt.inescid.cllsj.ast.types.ASTLboolT;
import pt.inescid.cllsj.ast.types.ASTLintT;
import pt.inescid.cllsj.ast.types.ASTLstringT;
import pt.inescid.cllsj.ast.types.ASTNotT;
import pt.inescid.cllsj.ast.types.ASTOfferT;
import pt.inescid.cllsj.ast.types.ASTOneT;
import pt.inescid.cllsj.ast.types.ASTRecT;
import pt.inescid.cllsj.ast.types.ASTRecvT;
import pt.inescid.cllsj.ast.types.ASTRecvTT;
import pt.inescid.cllsj.ast.types.ASTSendT;
import pt.inescid.cllsj.ast.types.ASTSendTT;
import pt.inescid.cllsj.ast.types.ASTType;
import pt.inescid.cllsj.ast.types.ASTUsageLT;
import pt.inescid.cllsj.ast.types.ASTUsageT;
import pt.inescid.cllsj.ast.types.ASTWhyT;
import pt.inescid.cllsj.ast.types.ASTintT;
import pt.inescid.cllsj.compiler.Compiler;
import pt.inescid.cllsj.compiler.ir.id.IRTypeFlag;
import pt.inescid.cllsj.compiler.ir.id.IRTypeId;

public class IRIsDroppableChecker extends ASTTypeVisitor {
  private Compiler compiler;
  private IREnvironment env;
  private boolean polarity;

  private Optional<Map<IRTypeId, Set<IRTypeFlag>>> types = Optional.of(new HashMap<>());

  public static IRTypeFlagRequisites check(
      Compiler compiler, IREnvironment env, ASTType type, boolean expectedPolarity) {
    IRIsDroppableChecker checker = new IRIsDroppableChecker();
    checker.compiler = compiler;
    checker.env = env;
    checker.polarity = expectedPolarity;
    type.accept(checker);
    if (checker.types.isEmpty()) {
      return IRTypeFlagRequisites.impossible();
    } else {
      return IRTypeFlagRequisites.onlyIf(checker.types.get());
    }
  }

  private void expectPolarity(boolean polarity) {
    if (this.polarity != polarity) {
      types = Optional.empty();
    }
    this.polarity = polarity;
  }

  private void recurse(ASTType type) {
    if (types.isPresent()) {
      boolean savedPolarity = polarity;
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
    types = Optional.empty();
  }

  @Override
  public void visit(ASTIdT type) {
    // Unfold the type to check if its definition is known
    ASTType unfolded = type.unfoldTypeCatch(env.getEp());
    if (!(unfolded instanceof ASTIdT)) {
      // We have a type definition, just recurse on the unfolded type
      unfolded.accept(this);
      return;
    }
    type = (ASTIdT) unfolded;

    // We still have a type identifier, thus this is a polymorphic session and not a value
    IREnvironment.Type typeEntry = env.getType(type.getid());
    expectPolarity(typeEntry.isPositive());
    types.ifPresent(
        s ->
            s.computeIfAbsent(typeEntry.getId(), k -> new HashSet<>())
                .add(IRTypeFlag.IS_DROPPABLE));
  }

  @Override
  public void visit(ASTNotT type) {
    polarity = !polarity;
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
    types = Optional.empty();
  }

  @Override
  public void visit(ASTRecvT type) {
    if (!compiler.optimizeSendValue.get()) {
      types = Optional.empty();
    } else {
      expectPolarity(false);
      recurse(type.getlhs());
      recurse(type.getrhs());
    }
  }

  @Override
  public void visit(ASTSendT type) {
    if (!compiler.optimizeSendValue.get()) {
      types = Optional.empty();
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
    types = Optional.empty();
  }

  @Override
  public void visit(ASTRecvTT type) {
    types = Optional.empty();
  }

  @Override
  public void visit(ASTAffineT type) {
    if (compiler.optimizeAffineValue.get()) {
      expectPolarity(true);
      recurse(type.getin());
    } else {
      types = Optional.empty();
    }
  }

  @Override
  public void visit(ASTCoAffineT type) {
    if (compiler.optimizeAffineValue.get()) {
      expectPolarity(false);
      recurse(type.getin());
    } else {
      types = Optional.empty();
    }
  }

  @Override
  public void visit(ASTCellT type) {
    expectPolarity(true);
  }

  @Override
  public void visit(ASTUsageT type) {
    expectPolarity(false);
  }

  @Override
  public void visit(ASTCellLT type) {
    types = Optional.empty();
  }

  @Override
  public void visit(ASTUsageLT type) {
    types = Optional.empty();
  }
}
