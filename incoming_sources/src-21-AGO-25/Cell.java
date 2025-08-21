import java.util.logging.*;


interface Cell {

    String getId();
    LinSession take(String _chi) throws Exception;
    void put(String _chi, ASTNode _rhs, Env<EnvEntry> _ep, Env<Session> _ed,  Env<Server> _eg) throws Exception;
    void free() throws Exception;
    void setCell(String _chi, ASTNode _rhs, Env<EnvEntry> _ep, Env<Session> _ed,
                 Env<Server> _eg, Logger logger, boolean _linear,boolean _state);
    void setEmptyCell(Logger logger);
    LinSession read(String _chi) throws Exception;
    void write(String _chi, ASTNode _rhs, Env<EnvEntry> _ep, Env<Server> _eg) throws Exception;
    void lock() throws Exception;
    void unlock() throws Exception;
    void incUsages(int n);

}
