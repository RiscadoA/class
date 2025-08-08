package pt.inescid.cllsj.compiler.ir;

import java.util.HashMap;
import java.util.Map;

public class IRProgram {
  private Map<String, IRProcess> processes;

  public IRProgram() {
    this.processes = new HashMap<>();
  }

  public void addProcess(String name, IRProcess process) {
    processes.put(name, process);
  }

  public Map<String, IRProcess> getProcesses() {
    return processes;
  }

  @Override
  public String toString() {
    String result = "";
    for (Map.Entry<String, IRProcess> process : processes.entrySet()) {
      if (!result.isEmpty()) {
        result += "\n";
      }
      result += process.getKey() + "[env]:\n";
      for (int i = 0; i < process.getValue().getTypeVariableCount(); i++) {
        String pol = process.getValue().isTypeVariablePositive(i) ? "positive" : "negative";
        result += "    type " + i + ": " + pol + "\n";
      }
      for (int i = 0; i < process.getValue().getRecordCount(); i++) {
        result += "    ";
        if (i < process.getValue().getRecordArgumentCount()) {
          result += "arg ";
        }
        result += "record " + i + ": " + process.getValue().getRecordType(i).toString() + "\n";
      }
      for (int i = 0; i < process.getValue().getExponentialCount(); i++) {
        result += "    ";
        if (i < process.getValue().getExponentialArgumentCount()) {
          result += "arg ";
        }
        result +=
            "exponential "
                + i
                + ": "
                + process.getValue().getExponentialType(i).toString()
                + "\n";
      }
      result += "    endPoints: " + process.getValue().getEndPoints() + "\n";
      result += process.getKey() + ":\n";
      result += process.getValue().toString() + "\n";
    }
    return result;
  }
}
