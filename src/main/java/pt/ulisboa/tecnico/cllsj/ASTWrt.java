package pt.ulisboa.tecnico.cllsj;

import java.util.*;
import java.util.logging.*;
import java.util.function.*;

public class ASTWrt extends ASTNode {
    String chs;
    String cho;
    ASTType type;
    ASTNode lhs;
    ASTNode rhs;

    boolean lockedUsage;

    public ASTWrt(String _chs,String _cho,ASTType _type,ASTNode _lhs,ASTNode _rhs) {
        chs = _chs;
        cho = _cho;
        type = _type;
        lhs = _lhs;
        rhs = _rhs;
    }

    public  void ASTInsertPipe(Function<ASTNode,ASTNode> f, ASTNode from) throws Exception
    {
	if (from==lhs) { ASTNode nnode = f.apply(from);
	    lhs.setanc(nnode);
	    lhs = nnode;
	    nnode.setanc(this);
	} else if (from==rhs) { ASTNode nnode = f.apply(from);
	    rhs.setanc(nnode);
	    rhs = nnode;
	    nnode.setanc(this);
	} else
	    {
		throw new Exception("ASTInsertPipe: call not expected");   
	    }
    }
    public void ASTInsertUse(String _ch,  ASTType t, ASTNode here, Boolean disCont) throws Exception
    {
	anc.ASTInsertUse(_ch, t, this, disCont);
    }

    public  void ASTInsertCall(String ch, String cho, ASTType t, ASTNode here) throws Exception
    {
        ASTNode pushCall = new ASTCall(ch, cho, t, here);
        pushCall.eg = eg;

        here.setanc(pushCall);
        pushCall.setanc(this);
        if (lhs==here) lhs = pushCall;
        else rhs = pushCall;
    }

    public void ASTInsertWhyNot(String ch, ASTType _t, ASTNode here) throws Exception
    {
        anc.ASTInsertWhyNot(ch, _t, this);
    }

    public ASTNode ASTweakeningOnLeaf(String ch, ASTType typ, boolean exp) throws Exception
    {
	    Set<String> s = lhs.fn(new HashSet<String>());
	    if ( !ch.equals(cho) && s.contains(ch)  ) {
		lhs = lhs.ASTweakeningOnLeaf(ch,typ,exp);
		return this;
	    };
	    rhs = rhs.ASTweakeningOnLeaf(ch,typ,exp);
	    return this;
    }

    public void ASTupdCont(ASTNode newCont, ASTNode caller) throws Exception{
        if(caller == lhs)
            lhs = newCont;
        else
            rhs = newCont;
    }

    /*
    public void setBackPtrs()
    {
	lhs.setBackPtrs();
	rhs.setBackPtrs();
        lhs.setanc(this);
        rhs.setanc(this);
    }
    */
    
    public void typecheck(Env<ASTType> ed, Env<ASTType> eg, Env<EnvEntry> ep) throws Exception {
        this.eg = eg;

        this.inferUses(chs,ed,ep);
        ASTType ty = ed.find(chs);
        ty = ty.unfoldType(ep);
        ASTType typee = type.unfoldType(ep);

        if (ty instanceof ASTUsageBLT) {
            lockedUsage = true;
            ASTUsageBLT tys = (ASTUsageBLT)ty;
            ed.upd(chs,null);

            if(typee.equalst( new ASTBangT(tys.getin().dual(ep)),ep, true, new Trail())) {

                Set<String> s = lhs.fn(new HashSet<String>());
                s.remove(cho);
                Iterator<String> it = s.iterator();
                while(it.hasNext()){
                    String id = it.next();
                    //System.out.println("DEBUG !: " + id + " is a free name!");
                    ASTType tyId = null;
                    try {
                        tyId = ed.find(id);
                    } catch (Exception e){}
                    if(tyId != null){
                        //System.out.println("DEBUG !: " + id + " is a in the linear context");
                        tyId = tyId.unfoldType(ep);
                        if(tyId instanceof ASTWhyT){
                            //System.out.println("DEBUG !: " + id + " is of type ?");
                            ASTWhyT t = (ASTWhyT) tyId;
                            tyId = t.getin();
                            this.getanc().ASTInsertWhyNot(id,tyId, this);
                            ed.upd(id, null);
                            //System.out.println("DEBUG ! infer ? for " + id);
                            //eg.crawl();
                        } else
                            throw new TypeError("Line " + lineno + " :" +"Lineno " + lineno + " :" +"!: "+id+" is not of ?type.");
                    }
                }


                Env<ASTType> el = new Env<ASTType>().assoc(cho, type.unfoldType(ep));

                Env<ASTType> eglhs = eg.assoc("$DUMMY",new ASTBotT());

                lhs.typecheck(el,eglhs,ep);
                lhs.linclose(el,ep);
                lhs.linclose(cho,el,ep);
                ed.upd(chs, new ASTUsageBLT(tys.getin().unfoldType(ep)));

                Env<ASTType> egrhs = eg.assoc("$DUMMY",new ASTBotT());

                rhs.typecheck(ed,egrhs,ep);
                rhs.linclose(ed,ep);
            }
            else throw new TypeError("Line " + lineno + " :" +"Lineno " + lineno + " :" +"WRT "+cho
                    +" type mismatch: found="+tys.getin().toStr(ep)+" declared="+typee.toStr(ep));
        } else
        if (ty instanceof ASTUsageBT) {
            lockedUsage = false;
            ASTUsageBT tys = (ASTUsageBT)ty;
            ed.upd(chs,null);

            if(typee.equalst( new ASTBangT(tys.getin().dual(ep)),ep, true, new Trail())) {

                Set<String> s = lhs.fn(new HashSet<String>());
                s.remove(cho);
                Iterator<String> it = s.iterator();
                while(it.hasNext()){
                    String id = it.next();
                    //System.out.println("DEBUG !: " + id + " is a free name!");
                    ASTType tyId = null;
                    try {
                        tyId = ed.find(id);
                    } catch (Exception e){}
                    if(tyId != null){
                        //System.out.println("DEBUG !: " + id + " is a in the linear context");
                        tyId = tyId.unfoldType(ep);
                        if(tyId instanceof ASTWhyT){
                            //System.out.println("DEBUG !: " + id + " is of type ?");
                            ASTWhyT t = (ASTWhyT) tyId;
                            tyId = t.getin();
                            this.getanc().ASTInsertWhyNot(id,tyId, this);
                            ed.upd(id, null);
                            //System.out.println("DEBUG ! infer ? for " + id);
                            //eg.crawl();
                        } else
                            throw new TypeError("Line " + lineno + " :" +"Lineno " + lineno + " :" +"!: "+id+" is not of ?type.");
                    }
                }


                Env<ASTType> el = new Env<ASTType>().assoc(cho, type.unfoldType(ep));

                Env<ASTType> eglhs = eg.assoc("$DUMMY",new ASTBotT());

                lhs.typecheck(el,eglhs,ep);
                lhs.linclose(el,ep);
                lhs.linclose(cho,el,ep);
                ed.upd(chs, new ASTUsageBT(tys.getin().unfoldType(ep)));

                Env<ASTType> egrhs = eg.assoc("$DUMMY",new ASTBotT());

                rhs.typecheck(ed,egrhs,ep);
                rhs.linclose(ed,ep);
            }
            else throw new TypeError("Line " + lineno + " :" +"Lineno " + lineno + " :" +"WRT "+cho
                    +" type mismatch: found="+tys.getin().toStr(ep)+" declared="+typee.toStr(ep));
        } else
            throw new TypeError("Line " + lineno + " :" +"Lineno " + lineno + " :" +"WRT: "+chs+" is not of USAGE! type.");
    }

    public Set<String> fn(Set<String> s) {
        Set<String> ss;
        s.add(chs);
        ss = lhs.fn(s);
        ss.remove(cho);
        ss = rhs.fn(s);
        return ss;
    }

    public Set<String> fnLinear(Set<String> s) {
        Set<String> ss;
        s.add(chs);
        ss = lhs.fnLinear(s);
        ss.remove(cho);
        ss = rhs.fnLinear(s);
        return ss;
    }

    public ASTNode subst(Env<ASTType> e) {
        ASTWrt p= new ASTWrt(chs,cho,type.subst(e),lhs.subst(e),rhs.subst(e));
	p.lhs.setanc(p);
	p.rhs.setanc(p);
	return p;
    }

    public void subs(String x, String y){// implements x/y (substitutes y by x)
        if(y == chs) {
            chs = x;
            rhs.subs(x,y);
        }
        else if(x == cho){//we rename the bound name chi to fresh to avoid capturing name x
            String fresh = ASTNode.gensym();
            lhs.subs(fresh, cho);
            cho = fresh;
            lhs.subs(x,y);
            rhs.subs(x,y);
        }
        else if(y != cho) {
            lhs.subs(x,y);
            rhs.subs(x, y);
        }
        else
            rhs.subs(x,y);
    }

    public void runproc(Env<EnvEntry> ep, Env<LinSession> ed, Env<Server> eg, Logger logger) throws Exception{

        Cell cell = (Cell) ed.find(chs);
        //Thread.sleep(10);
        if(lockedUsage){
            cell.write(cho, lhs, ep, eg);
        }else {
            cell.lock();
            cell.write(cho, lhs, ep, eg);
            cell.unlock();
        }
        logger.info("WRITE on cell " + cell.getId());
        rhs.runproc(ep, ed, eg, logger);
    }

}
