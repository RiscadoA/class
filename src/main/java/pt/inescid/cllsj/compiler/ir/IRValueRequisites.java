package pt.inescid.cllsj.compiler.ir;

import java.util.HashSet;
import java.util.Optional;
import java.util.Set;
import java.util.function.Function;
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
import pt.inescid.cllsj.compiler.ir.id.IRTypeId;

public class IRValueRequisites {
  private Optional<Set<IRTypeId>> types;

  public static IRValueRequisites check(
      Compiler compiler, IREnvironment env, ASTType type, boolean expectedPolarity) {
    Visitor visitor = new Visitor();
    visitor.compiler = compiler;
    visitor.env = env;
    visitor.polarity = expectedPolarity;
    type.accept(visitor);
    return new IRValueRequisites(visitor.types);
  }

  public static IRValueRequisites notValue() {
    return new IRValueRequisites(Optional.empty());
  }

  public static IRValueRequisites valueIf(Set<IRTypeId> types) {
    return new IRValueRequisites(Optional.of(types));
  }

  public static IRValueRequisites valueIf(IRTypeId type) {
    return valueIf(Set.of(type));
  }

  public static IRValueRequisites value() {
    return valueIf(Set.of());
  }

  private IRValueRequisites(Optional<Set<IRTypeId>> types) {
    this.types = types;
  }

  public boolean canBeValue() {
    return types.isPresent();
  }

  public boolean mustBeValue() {
    return types.isPresent() && types.get().isEmpty();
  }

  public Set<IRTypeId> typesWhichMustBeValue() {
    return types.orElseThrow();
  }

  public IRValueRequisites replaceTypes(Function<IRTypeId, IRTypeId> replacer) {
    return expandTypes(id -> IRValueRequisites.valueIf(replacer.apply(id)));
  }

  public IRValueRequisites expandTypes(Function<IRTypeId, IRValueRequisites> replacer) {
    if (types.isPresent()) {
      Set<IRTypeId> newTypes = new HashSet<>();
      for (IRTypeId t : types.get()) {
        IRValueRequisites expanded = replacer.apply(t);
        if (!expanded.canBeValue()) {
          return IRValueRequisites.notValue();
        }
        newTypes.addAll(expanded.types.get());
      }
      return IRValueRequisites.valueIf(newTypes);
    } else {
      return this;
    }
  }

  @Override
  public String toString() {
    if (mustBeValue()) {
      return "yes";
    } else if (canBeValue()) {
      return "if("
          + types.get().stream().map(Object::toString).reduce((a, b) -> a + ", " + b).orElse("")
          + ")";
    } else {
      return "no";
    }
  }

  private static class Visitor extends ASTTypeVisitor {
    private Compiler compiler;
    private IREnvironment env;
    private boolean polarity;

    private Optional<Set<IRTypeId>> types = Optional.of(new HashSet<>());

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
      types.ifPresent(s -> s.add(typeEntry.getId()));
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
      types = Optional.empty();
    }

    @Override
    public void visit(ASTCoAffineT type) {
      types = Optional.empty();
    }

    @Override
    public void visit(ASTCellT type) {
      types = Optional.empty();
    }

    @Override
    public void visit(ASTUsageT type) {
      types = Optional.empty();
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
}
