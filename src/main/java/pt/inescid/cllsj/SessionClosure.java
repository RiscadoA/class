package pt.inescid.cllsj;

import pt.inescid.cllsj.ast.nodes.ASTNode;

public class SessionClosure extends SessionField {

  String id;
  boolean pol;
  ASTNode body;
  Env<SessionField> env;
  Env<EnvEntry> ep;
  int sessionsize;

  public SessionClosure() {}

  public SessionClosure(
      String _id,
      int _size,
      boolean _pol,
      ASTNode _body,
      Env<SessionField> _env,
      Env<EnvEntry> _ep) {
    id = _id;
    body = _body;
    env = _env;
    ep = _ep;
    sessionsize = _size;
    if (_size > 30) { // silly safe guard
      System.out.println("SIZE OVF=" + _size);
      _size = _size / 0;
    }
    pol = _pol;
  }

  public String getId() {
    return id;
  }

  public void setId(String _id) {
    id = _id;
  }

  public ASTNode getBody() {
    return body;
  }

  public void setBody(ASTNode _body) {
    body = _body;
  }

  public void setEnv(Env<SessionField> _env) {
    env = _env;
  }

  public Env<SessionField> getEnv() {
    return env;
  }

  public void setEnvP(Env<EnvEntry> _ep) {
    ep = _ep;
  }

  public Env<EnvEntry> getEnvP() {
    return ep;
  }

  public int getSize() {
    return sessionsize;
  }

  public boolean getPol() {
    return pol;
  }
}
