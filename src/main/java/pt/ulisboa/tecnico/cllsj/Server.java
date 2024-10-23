package pt.ulisboa.tecnico.cllsj;

import java.util.HashMap;
import java.util.logging.*;


class Server extends Session {

    public final String chi;

    private final Logger logger;

    private final ASTType type;

    public final ASTNode rhs;

    private final Env<EnvEntry> ep;

    private final Env<Server> ge;

    Server(){
       chi     = "FUC";
        type    = null;
        rhs     = null;
        ep      = null;
        ge      = null;
        logger = null;
    }

    Server(String _chi, ASTType _type, ASTNode _rhs, Env<EnvEntry> _ep, Env<Server> _ge, Logger _logger){
        chi = _chi;
        type = _type;
        rhs = _rhs;
        ep  = _ep;
        ge = _ge;
        logger = _logger;
    }

    LinSession call(String _chi) throws Exception{
        LinSession session = new LinSession(_chi);

        CLLSj.threadPool.submit(
                new Runnable(){
                    public void run(){ try{
                        rhs.runproc(ep, new Env<LinSession>(chi, session, null), ge, logger);
                    }catch (Exception e) { e.printStackTrace(System.out);} }
                });
        return session;
    }
}
