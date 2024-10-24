package pt.inescid.cllsj;

import java.util.*;
import pt.inescid.cllsj.ast.nodes.ASTType;

public class SessionFieldAffine extends SessionField {

  HashMap<String, ASTType> usageSet;
  HashMap<String, ASTType> coaffineSet;

  public SessionFieldAffine(HashMap<String, ASTType> _usageSet) {
    usageSet = _usageSet;
  }

  public HashMap<String, ASTType> getusageSet() {
    return usageSet;
  }
}
