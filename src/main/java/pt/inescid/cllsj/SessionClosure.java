package pt.inescid.cllsj;

public class SessionClosure extends SessionField {

  String id;
  boolean pol;
  ASTNode body;
  Env<SessionField> env;
  Env<EnvEntry> ep;
  int sessionsize;

  SessionClosure() {}

  SessionClosure(
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

  String getId() {
    return id;
  }

  void setId(String _id) {
    id = _id;
  }

  ASTNode getBody() {
    return body;
  }

  void setBody(ASTNode _body) {
    body = _body;
  }

  void setEnv(Env<SessionField> _env) {
    env = _env;
  }

  Env<SessionField> getEnv() {
    return env;
  }

  void setEnvP(Env<EnvEntry> _ep) {
    ep = _ep;
  }

  Env<EnvEntry> getEnvP() {
    return ep;
  }

  int getSize() {
    return sessionsize;
  }

  boolean getPol() {
    return pol;
  }
}
