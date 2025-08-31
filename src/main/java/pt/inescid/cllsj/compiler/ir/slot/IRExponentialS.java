package pt.inescid.cllsj.compiler.ir.slot;

import pt.inescid.cllsj.compiler.ir.IRSlotVisitor;

public class IRExponentialS extends IRSlot {
    @Override
    public void accept(IRSlotVisitor visitor) {
        visitor.visit(this);
    }

    @Override
    public String toString() {
        return "exponential";
    }
}
