package pt.inescid.cllsj.compiler.ir.slot;

import java.util.List;

public class IRSlotSequence {
    private List<IRSlot> slots;

    public IRSlotSequence(List<IRSlot> slots) {
        this.slots = slots;
    }

    public List<IRSlot> getSlots() {
        return slots;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        if (!slots.isEmpty()) {
            sb.append(slots.getFirst().toString());
        }
        for (int i = 1; i < slots.size(); i++) {
            sb.append("; ").append(slots.get(i).toString());
        }
        return sb.toString();        
    }
}
