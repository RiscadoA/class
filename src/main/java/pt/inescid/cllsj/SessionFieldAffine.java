package pt.inescid.cllsj;

import java.util.*;

public class SessionFieldAffine extends SessionField {

  HashMap<String, ASTType> usageSet;
  HashMap<String, ASTType> coaffineSet;

  SessionFieldAffine(HashMap<String, ASTType> _usageSet) {
    usageSet = _usageSet;
  }

  HashMap<String, ASTType> getusageSet() {
    return usageSet;
  }
}
