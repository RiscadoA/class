package pt.ulisboa.tecnico.cllsj;

import java.util.*;
import java.util.logging.*;
import java.util.function.*;

// unfolded

public class ASTEmptyCell extends ASTNode {

    String ch;

    public ASTEmptyCell(String _ch) {
        ch = _ch;
    }

    public void ASTInsertPipe(Function<ASTNode,ASTNode> f, ASTNode from) throws Exception
    {	throw new Exception("ASTInsertPipe: call not expected");
    }

    public void ASTInsertUse(String ch, ASTType t, ASTNode here, Boolean disCont) throws Exception
    {
	 throw new Exception ("Unexpected call editASTInsertUse"); // never called
    }
    public void ASTInsertWhyNot(String _ch, ASTType _t, ASTNode here) throws Exception
    {
        throw new Exception ("Unexpected call: ASTInsertWhyNot"); // never called
    }
    public void ASTupdCont(ASTNode newCont, ASTNode caller) throws Exception {
        throw new Exception ("Unexpected call.");
    }

    public void ASTInsertCall(String ch, String cho, ASTType t, ASTNode here) throws Exception{
        throw new Exception ("Unexpected call: ASTInsertCall"); // never called
    }

    public ASTNode ASTweakeningOnLeaf(String _ch, ASTType t, boolean exp) throws Exception
    {
	return this.ASTweakeningTerm(_ch,exp);
    }

    public void typecheck(Env<ASTType> ed, Env<ASTType> eg, Env<EnvEntry> ep) throws Exception {
        this.inferUses(ch,ed,ep);

        ASTType ty;
        ty = ed.find(ch);
        ty = ty.unfoldType(ep);
        if (ty instanceof ASTCellLT) {
            ed.upd(ch,null);
        } else throw new TypeError("Line " + lineno + " :" +"EMPTY CELL: "+ch+" is not of CellL type.");
    }

    public Set<String> fn(Set<String> s) {
        s.add(ch);
        return s;
    }

    public Set<String> fnLinear(Set<String> s) {
        s.add(ch);
        return s;
    }

    public ASTNode subst(Env<ASTType> e) {
	    return this;
    }

    public void subs(String x, String y){// implements x/y (substitutes y by x)
        if(y == ch)
            ch = x;
    }

    public void runproc(Env<EnvEntry> ep, Env<LinSession> ed, Env<Server> eg, Logger logger) throws Exception{
        Cell cell = (Cell) ed.find(ch);
        cell.setEmptyCell(logger);
    }

}
