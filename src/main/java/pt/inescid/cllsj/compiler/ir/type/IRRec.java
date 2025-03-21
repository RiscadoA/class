package pt.inescid.cllsj.compiler.ir.type;

import pt.inescid.cllsj.compiler.ir.IRTypeVisitor;

public class IRRec extends IRType {
    private IRType inner;

    public IRRec(IRType inner) {
        this.inner = inner;
    }

    public IRType getInner() {
        return inner;
    }

    @Override
    public void accept(IRTypeVisitor visitor) {
        visitor.visit(this);
    }

    @Override
    public String toString() {
        return "rec " + inner.toString();
    }
}
