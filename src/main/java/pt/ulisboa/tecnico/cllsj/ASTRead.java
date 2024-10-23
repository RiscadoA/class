package pt.ulisboa.tecnico.cllsj;

import java.util.*;
import java.util.logging.*;
 import java.util.function.*;

public class ASTRead extends ASTNode {
    String chr;
    String chi;
    ASTType type;
    ASTNode rhs;

    boolean lockedUsage;

    public ASTRead(String _chr, String _chi, ASTType _type, ASTNode _rhs) {
        chr = _chr;
        chi = _chi;
        type = _type;
        rhs = _rhs;
    }

    public  void ASTInsertPipe(Function<ASTNode,ASTNode> f, ASTNode from) throws Exception
    {
	if (from==rhs) { ASTNode nnode = f.apply(from);
	    rhs.setanc(nnode);
	    rhs = nnode;
	    nnode.setanc(this);
	} else {
	    throw new Exception("ASTInsertPipe: call not expected");   
	}
    }
    public  void ASTInsertUse(String _ch, ASTType t, ASTNode here, Boolean disCont) throws Exception
    {
        if(_ch.equals(chr)) {
            ASTUse pushUse = new ASTUse(_ch, here);
	    pushUse.setrhs(t);
            pushUse.eg = eg;
            here.setanc(pushUse);
            pushUse.setanc(this);
            rhs = pushUse;
        }else
            anc.ASTInsertUse(_ch,t,  this, disCont);
    }

    public void ASTInsertCall(String ch, String cho, ASTType t, ASTNode here) throws Exception{
        ASTNode pushCall = new ASTCall(ch, cho, t,here);
        pushCall.eg = eg;
        here.setanc(pushCall);
        pushCall.setanc(this);
        rhs = pushCall;
    }

    public void ASTInsertWhyNot(String _ch, ASTType _t, ASTNode here) throws Exception
    {
        anc.ASTInsertWhyNot(_ch, _t, this);
    }

    public void ASTupdCont(ASTNode newCont, ASTNode caller) throws Exception{
        rhs = newCont;
    }

    public ASTNode ASTweakeningOnLeaf(String _ch, ASTType t, boolean exp) throws Exception
    {
	if(_ch.equals(chi)) {
	    if (!_ch.equals(chr)) { // may place here before READ
		return this.ASTweakeningHere(_ch,exp);
	    }
	    throw new TypeError("Line " + lineno + " :" +"for " + _ch + " type pending after READ on "+chr);
	}
	rhs = rhs.ASTweakeningOnLeaf(_ch,t,exp);
	return this;
    }

    public void typecheck(Env<ASTType> ed, Env<ASTType> eg, Env<EnvEntry> ep) throws Exception {
        this.eg = eg;
        this.inferUses(chr,ed,ep);

        ASTType ty = ed.find(chr);
        ASTType typee = type.unfoldType(ep);
        ty = ty.unfoldType(ep);
        if (ty instanceof ASTUsageBLT) {
            lockedUsage = true;
            ASTUsageBLT tyr = (ASTUsageBLT) ty;
            if (typee.equalst(new ASTWhyT(tyr.getin()), ep, true, new Trail())) {
                ed = ed.assoc(chi, typee); // must be ?-type
                ed.upd(chr, new ASTUsageBLT(tyr.getin().unfoldType(ep)));
                rhs.typecheck(ed, eg, ep);
                rhs.linclose(ed, ep);
                rhs.linclose(chi, ed, ep);
            } else throw new TypeError("Line " + lineno + " :" +"RD " + chi
                    + " type mismatch: found=" + tyr.getin().toStr(ep) + " declared=" + type.toStr(ep));
        } else if (ty instanceof ASTUsageBT) {
            lockedUsage = false;
            ASTUsageBT tyr = (ASTUsageBT) ty;
            if (typee.equalst(new ASTWhyT(tyr.getin()), ep, true, new Trail())) {
                ed = ed.assoc(chi, typee); // must be ?-type
                ed.upd(chr, new ASTUsageBT(tyr.getin().unfoldType(ep)));
                ep = ASTNode.propagateRVar(ep, chr, chi);
                rhs.typecheck(ed, eg, ep);
                rhs.linclose(ed, ep);
                rhs.linclose(chi, ed, ep);
            } else throw new TypeError("Line " + lineno + " :" +"RD " + chi
                    + " type mismatch: found=" + tyr.getin().toStr(ep) + " declared=" + type.toStr(ep));
        } else throw new TypeError("Line " + lineno + " :" +"READ: " + chr + " is not of USAGE! type.");
    }

    public Set<String> fn(Set<String> s) {
        Set<String> ss;
        s.add(chr);
        ss = rhs.fn(s);
        ss.remove(chi);
        return ss;
    }

    public Set<String> fnLinear(Set<String> s) {
        Set<String> ss;
        s.add(chr);
        ss = rhs.fnLinear(s);
        ss.remove(chi);
        return ss;
    }

    public ASTNode subst(Env<ASTType> e) {
        ASTRead p = new ASTRead(chr, chi, type.subst(e), rhs.subst(e));
	p.rhs.setanc(p);
	return p;
    }

    public void subs(String x, String y){// implements x/y (substitutes y by x)
        if(y == chr) {
            chr = x;
            rhs.subs(x,y);
        }
        else if(x == chi){//we rename the bound name chi to fresh to avoid capturing name x
            String fresh = ASTNode.gensym();
            rhs.subs(fresh, chi);
            chi = fresh;
            rhs.subs(x,y);
        }
        else if(y != chi)
            rhs.subs(x,y);
    }

    public void runproc(Env<EnvEntry> ep, Env<LinSession> ed, Env<Server> eg, Logger logger) throws Exception {

        Cell cell = (Cell) ed.find(chr);
        LinSession session;
        //Thread.sleep(10);
        if(lockedUsage){
            //System.out.println("The cell " + cell.getId() + " is locked. Trying to read it.");
            session = cell.read(chi);
        }
        else {
            cell.lock();
            session = cell.read(chi);
            cell.unlock();
        }
        logger.info("READ cell " + cell.getId() + " on session " + chi);
        rhs.runproc(ep, ed.assoc(chi, session), eg, logger);
    }

}
