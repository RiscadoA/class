package pt.inescid.cllsj.compiler.ir;

import java.util.Map;

public class IRProgram {
    private Map<String, IRProcess> processes;

    public IRProgram(Map<String, IRProcess> processes) {
        this.processes = processes;
    }

    public Map<String, IRProcess> getProcesses() {
        return processes;
    }

    @Override
    public String toString() {
        String result = "";
        for (Map.Entry<String, IRProcess> entry : processes.entrySet()) {
            result += entry.getKey() + ":\n";
            result += entry.getValue().toString().indent(4);
        }
        return result;
    }
}
