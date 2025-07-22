import java.util.*;
    
public class SAMState  {

    public ASTNode code;
    public Env<SessionField> frame;
    public Env<EnvEntry> epnm;
    
    public SAMState(ASTNode _code,  Env<SessionField> _frame, Env<EnvEntry> _epnm)
    {
	code = _code;
	frame = _frame;
	epnm = _epnm;
    }	
    
};;

	
