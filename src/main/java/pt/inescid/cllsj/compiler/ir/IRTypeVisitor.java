package pt.inescid.cllsj.compiler.ir;

import pt.inescid.cllsj.compiler.ir.type.IRClose;
import pt.inescid.cllsj.compiler.ir.type.IRRec;
import pt.inescid.cllsj.compiler.ir.type.IRSession;
import pt.inescid.cllsj.compiler.ir.type.IRTag;
import pt.inescid.cllsj.compiler.ir.type.IRType;
import pt.inescid.cllsj.compiler.ir.type.IRVar;

public abstract class IRTypeVisitor {
    // Catch all for types which do not have their own visit method
    public abstract void visit(IRType type);

    public void visit(IRClose type) {
        visit((IRType) type);
    }

    public void visit(IRTag type) {
        visit((IRType) type);
    }

    public void visit(IRSession type) {
        visit((IRType) type);
    }

    public void visit(IRRec type) {
        visit((IRType) type);
    }

    public void visit(IRVar type) {
        visit((IRType) type);
    }
}
