package pt.inescid.cllsj;

import java.util.*;
import java.util.concurrent.*;
import pt.inescid.cllsj.ast.nodes.ASTId;
import pt.inescid.cllsj.ast.nodes.ASTNode;

public class SAM {

  static void tracedef(ASTNode n, boolean t) {
    String msg = t ? ("SAM START THREAD ") : ("SAM STOP THREAD ");
    System.out.print(msg + " " + n + " ");
    if (n instanceof ASTId) {
      System.out.print(((ASTId) n).getId());
    }
    System.out.println();
  }

  public static void SAMloop(ASTNode code, Env<SessionField> frame, Env<EnvEntry> ep)
      throws Exception {

    if (CLLSj.trace && code != null) {
      tracedef(code, true);
    }
    SAMCont cont = new SAMCont(code, frame, ep);
    while (cont.code != null) {
      cont.code.samL(cont.frame, cont.epnm, cont);
    }
    if (CLLSj.trace) tracedef(code, false);
  }
}
