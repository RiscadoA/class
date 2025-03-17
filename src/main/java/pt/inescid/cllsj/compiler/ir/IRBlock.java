package pt.inescid.cllsj.compiler.ir;

import java.util.List;

import pt.inescid.cllsj.compiler.ir.instructions.IRInstruction;

public class IRBlock {
    private List<IRInstruction> instructions;

    public IRBlock(List<IRInstruction> instructions) {
        this.instructions = instructions;
    }

    public List<IRInstruction> getInstructions() {
        return instructions;
    }

    public void accept(IRVisitor visitor) {
        for (IRInstruction instruction : instructions) {
            instruction.accept(visitor);
        }
    }

    @Override
    public String toString() {
        String result = "";
        for (IRInstruction instruction : instructions) {
            result += instruction.toString() + "\n";
        }
        return result;
    }
}
