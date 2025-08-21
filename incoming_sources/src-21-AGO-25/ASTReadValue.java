import java.util.*;
import java.util.logging.*;
import java.util.function.*;

public class ASTReadValue extends ASTNode {

    String chr;
    SessionRecord cback;
    ASTNode rhs;

    ASTReadValue(String ch, SessionRecord cb, ASTNode cont){
        cback = cb;
        chr = ch;
        rhs = cont;
    }

    public  void ASTInsertPipe(Function<ASTNode,ASTNode> f, ASTNode from) throws Exception
    {
    }

    public  void ASTInsertUse(String _ch,ASTType t, ASTNode here, Boolean disCont) throws Exception
    {
    }

    public void ASTInsertWhyNot(String ch, ASTType _t, ASTNode here) throws Exception
    {
    }

    public void ASTInsertCall(String ch, String cho, ASTType t, ASTNode here) throws Exception{
    }

    public ASTNode ASTweakeningOnLeaf(String _ch, ASTType typ, boolean exp) throws Exception
    {
        return null;
    }

    public void ASTupdCont(ASTNode newCont, ASTNode caller) throws Exception{
    }
  
    public void typecheck(Env<ASTType> ed, Env<ASTType> eg, Env<EnvEntry> ep) throws Exception {
    }

    public Set<String> fn(Set<String> s) {
        s = rhs.fn(s);
        return s;
    }

    public Set<String> fnLinear(Set<String> s) {
        s = rhs.fnLinear(s);
        return s;
    }
    
    public ASTNode subst(Env<ASTType> e) {
	    return this;
	}

    public void subs(String x, String y){// implements x/y (substitutes y by x)
        rhs.subs(x,y);
    }

    public void runproc(Env<EnvEntry> ep, Env<Session> ed,
	Env<Server> eg, Logger logger) throws Exception {
    }

    public void samL(Env<SessionField> frame, Env<EnvEntry> ep, SAMCont p_cont) throws Exception  {
        Value v = (Value) cback.readSlot(0);
        rhs.samL(frame.assoc(chr,v),ep,p_cont);
    }
    
}
