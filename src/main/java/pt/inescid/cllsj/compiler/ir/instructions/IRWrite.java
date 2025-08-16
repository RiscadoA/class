package pt.inescid.cllsj.compiler.ir.instructions;

import pt.inescid.cllsj.compiler.ir.IRInstructionVisitor;

public abstract class IRWrite extends IRInstruction {
    private int record; // Record into which the value is written.
    private int slot; // Slot in the record where the value is stored.
    
    public IRWrite(int record, int slot) {
        this.record = record;
        this.slot = slot;
    }
    
    public int getRecord() {
        return record;
    }
    
    public int getSlot() {
        return slot;
    }
    
    @Override
    public void accept(IRInstructionVisitor visitor) {
        visitor.visit(this);
    }
}
