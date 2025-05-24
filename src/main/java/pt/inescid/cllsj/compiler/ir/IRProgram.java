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
      String records = "";
      for (int i = 0; i < process.getValue().getRecordCount(); i++) {
        if (!records.isEmpty()) {
          records += ", ";
        }
        records += i + ": " + process.getValue().getRecordType(i).toString();
      }

      String exponentials = "";
      for (int i = 0; i < process.getValue().getExponentialCount(); i++) {
        if (exponentials.isEmpty()) {
          exponentials += "; ";
        } else {
          exponentials += ", ";
        }
        exponentials += i + ": " + process.getValue().getExponentialType(i).toString();
      }

      if (!result.isEmpty()) {
        result += "\n";
      }
      result += process.getKey() + "(" + records + exponentials + "):\n";
      result += process.getValue().toString() + "\n";
    }
    return result;
  }
}
