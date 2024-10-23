package pt.ulisboa.tecnico.cllsj;

public class SessionValue extends SessionField {

  Value v;
  SessionClosure cont;

  SessionValue() {
    cont = new SessionClosure();
  }

  Value getValue() {
    return v;
  }

  void setValue(Value _v) {
    v = _v;
  }

  ASTNode getCont() {
    return cont.getBody();
  }

  void setCont(ASTNode _body) {
    cont.setBody(_body);
  }

  void setFrame(Env<SessionField> _env) {
    cont.setEnv(_env);
  }

  Env<SessionField> getFrame() {
    return cont.getEnv();
  }

  void setFrameP(Env<EnvEntry> _env) {
    cont.setEnvP(_env);
  }

  Env<EnvEntry> getFrameP() {
    return cont.getEnvP();
  }
}
