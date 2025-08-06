package pt.inescid.cllsj.compiler;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import pt.inescid.cllsj.Env;
import pt.inescid.cllsj.EnvEntry;
import pt.inescid.cllsj.ast.ASTTypeVisitor;
import pt.inescid.cllsj.ast.types.*;
import pt.inescid.cllsj.compiler.ir.IRValueRequisites;

public class ASTTypeIsValue extends ASTTypeVisitor {
  private boolean canBeValue = true;
  private Map<Integer, Boolean> requiredTypePolarities = new HashMap<>();
  private List<Integer> typesWhichMustBeValues = new ArrayList<>();
  private Env<EnvEntry> ep;
  private Map<String, Integer> typeMap = new HashMap<>();
  private boolean isDual = false;

  public static IRValueRequisites check(
      Env<EnvEntry> ep, Map<String, Integer> typeMap, ASTType type, boolean isDual) {
    ASTTypeIsValue visitor = new ASTTypeIsValue(ep, typeMap, isDual);
    type.accept(visitor);

    if (visitor.canBeValue) {
      return IRValueRequisites.value(
          visitor.requiredTypePolarities, visitor.typesWhichMustBeValues);
    } else {
      return IRValueRequisites.notValue();
    }
  }

  private ASTTypeIsValue(Env<EnvEntry> ep, Map<String, Integer> typeMap, boolean isDual) {
    this.ep = ep;
    this.typeMap = typeMap;
    this.isDual = isDual;
  }

  @Override
  public void visit(ASTType type) {
    throw new UnsupportedOperationException("Type not supported by IntoIRType: " + type);
  }

  @Override
  public void visit(ASTOneT type) {
    if (isDual) {
      canBeValue = false;
    }
  }

  @Override
  public void visit(ASTBotT type) {
    if (!isDual) {
      canBeValue = false;
    }
  }

  @Override
  public void visit(ASTSendT type) {
    if (isDual) {
      canBeValue = false;
    } else {
      type.getrhs().accept(this);
    }
  }

  @Override
  public void visit(ASTRecvT type) {
    if (!isDual) {
      canBeValue = false;
    } else {
      type.getrhs().accept(this);
    }
  }

  @Override
  public void visit(ASTCaseT type) {
    if (isDual) {
      canBeValue = false;
    } else {
      for (Map.Entry<String, ASTType> entry : type.getcases().entrySet()) {
        entry.getValue().accept(this);
      }
    }
  }

  @Override
  public void visit(ASTOfferT type) {
    if (!isDual) {
      canBeValue = false;
    } else {
      for (Map.Entry<String, ASTType> entry : type.getcases().entrySet()) {
        entry.getValue().accept(this);
      }
    }
  }

  @Override
  public void visit(ASTRecT type) {
    canBeValue = false;
  }

  @Override
  public void visit(ASTCoRecT type) {
    canBeValue = false;
  }

  @Override
  public void visit(ASTIdT type) {
    ASTType unfolded;
    try {
      unfolded = type.unfoldType(ep);
    } catch (Exception e) {
      throw new IllegalArgumentException("Error unfolding type: " + e.getMessage());
    }

    if (unfolded instanceof ASTIdT) {
      type = (ASTIdT) unfolded;
      int typeId = typeMap.get(type.getid());

      boolean requiredPolarity = !isDual;
      if (requiredTypePolarities.containsKey(typeId)
          && requiredTypePolarities.get(typeId) != requiredPolarity) {
        canBeValue = false;
      } else {
        requiredTypePolarities.put(typeId, requiredPolarity);
        typesWhichMustBeValues.add(typeId);
      }
    } else {
      unfolded.accept(this);
    }
  }

  @Override
  public void visit(ASTBangT type) {
    if (isDual) {
      canBeValue = false;
    }
  }

  @Override
  public void visit(ASTWhyT type) {
    if (!isDual) {
      canBeValue = false;
    }
  }

  @Override
  public void visit(ASTintT type) {
    if (isDual) {
      canBeValue = false;
    }
  }

  @Override
  public void visit(ASTCointT type) {
    if (!isDual) {
      canBeValue = false;
    }
  }

  @Override
  public void visit(ASTLintT type) {
    if (isDual) {
      canBeValue = false;
    }
  }

  @Override
  public void visit(ASTLCointT type) {
    if (!isDual) {
      canBeValue = false;
    }
  }

  @Override
  public void visit(ASTLboolT type) {
    if (isDual) {
      canBeValue = false;
    }
  }

  @Override
  public void visit(ASTCoLboolT type) {
    if (!isDual) {
      canBeValue = false;
    }
  }

  @Override
  public void visit(ASTLstringT type) {
    if (isDual) {
      canBeValue = false;
    }
  }

  @Override
  public void visit(ASTCoLstringT type) {
    if (!isDual) {
      canBeValue = false;
    }
  }

  @Override
  public void visit(ASTNotT type) {
    isDual = !isDual;
    type.getin().accept(this);
    isDual = !isDual;
  }

  @Override
  public void visit(ASTSendTT type) {
    if (isDual) {
      canBeValue = false;
    }
  }

  @Override
  public void visit(ASTRecvTT type) {
    if (!isDual) {
      canBeValue = false;
    }
  }

  @Override
  public void visit(ASTAffineT type) {
    canBeValue = false;
  }

  @Override
  public void visit(ASTCoAffineT type) {
    canBeValue = false;
  }

  @Override
  public void visit(ASTCellT type) {
    canBeValue = false;
  }

  @Override
  public void visit(ASTUsageT type) {
    canBeValue = false;
  }

  @Override
  public void visit(ASTCellLT type) {
    canBeValue = false;
  }

  @Override
  public void visit(ASTUsageLT type) {
    canBeValue = false;
  }
}
