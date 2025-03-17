package pt.inescid.cllsj.compiler.ir.instructions;

import java.util.Map;

import pt.inescid.cllsj.compiler.ir.IRVisitor;

public class IRCall extends IRInstruction {
    private String processName;
    private int environmentSize; // Number of records required for the process being called.
    private Map<Integer, Integer> arguments; // Maps records in the local environment to records in the environment of the process being called.

    public IRCall(String processName, int environmentSize, Map<Integer, Integer> arguments) {
        this.processName = processName;
        this.environmentSize = environmentSize;
        this.arguments = arguments;
    }

    public String getProcessName() {
        return processName;
    }

    public int getEnvironmentSize() {
        return environmentSize;
    }

    public Map<Integer, Integer> getArguments() {
        return arguments;
    }

    @Override
    public void accept(IRVisitor visitor) {
        visitor.visit(this);
    }

    @Override
    public String toString() {
        String str = "call(" + processName + ", " + environmentSize;
        for (Map.Entry<Integer, Integer> entry : this.arguments.entrySet()) {
            str += ", " + entry.getKey() + " -> " + entry.getValue();
        }
        return str + ")";
    }
}
