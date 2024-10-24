package pt.inescid.cllsj;

import java.util.logging.*;

interface Cell {

  String getId();

  LinSession take(String _chi) throws Exception;

  void put(String _chi, ASTNode _rhs, Env<EnvEntry> _ep, Env<LinSession> _ed, Env<Server> _eg)
      throws Exception;

  void free() throws Exception;

  void setCell(
      String _chi,
      ASTNode _rhs,
      Env<EnvEntry> _ep,
      Env<LinSession> _ed,
      Env<Server> _eg,
      Logger logger,
      boolean _linear);

  void setEmptyCell(Logger logger);

  LinSession read(String _chi) throws Exception;

  void write(String _chi, ASTNode _rhs, Env<EnvEntry> _ep, Env<Server> _eg) throws Exception;

  void lock() throws Exception;

  void unlock() throws Exception;

  void incUsages(int n);
}
