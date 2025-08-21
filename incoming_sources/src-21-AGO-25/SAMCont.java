import java.util.*;
    
public class SAMCont  {

    public ASTNode code;
    public Env<SessionField> frame;
    public Env<EnvEntry> epnm;

    static SAMCont Null = new SAMCont(null, null, null);
    
    public SAMCont(ASTNode _code,  Env<SessionField> _frame, Env<EnvEntry> _epnm)
    {
        code = _code;
        frame = _frame;
        epnm = _epnm;
    }	

    
};;

	
