package pt.inescid.cllsj;

import pt.inescid.cllsj.ast.nodes.ASTNode;

public class SessionValue extends SessionField {

  Value v;
  SessionClosure cont;

  public SessionValue() {
    cont = new SessionClosure();
  }

  public Value getValue() {
    return v;
  }

  public void setValue(Value _v) {
    v = _v;
  }

  public ASTNode getCont() {
    return cont.getBody();
  }

  public void setCont(ASTNode _body) {
    cont.setBody(_body);
  }

  public void setFrame(Env<SessionField> _env) {
    cont.setEnv(_env);
  }

  public Env<SessionField> getFrame() {
    return cont.getEnv();
  }

  public void setFrameP(Env<EnvEntry> _env) {
    cont.setEnvP(_env);
  }

  public Env<EnvEntry> getFrameP() {
    return cont.getEnvP();
  }
}
