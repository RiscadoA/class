package pt.inescid.cllsj.compiler.old.ir;

import java.util.HashMap;
import java.util.Map;
import java.util.function.BiConsumer;

public class IRProgramOld {
  private Map<String, IRProcessOld> processes;

  public IRProgramOld() {
    this.processes = new HashMap<>();
  }

  public void addProcess(String name, IRProcessOld process) {
    processes.put(name, process);
  }

  public Map<String, IRProcessOld> getProcesses() {
    return processes;
  }

  @Override
  public String toString() {
    String result = "";
    for (Map.Entry<String, IRProcessOld> process : processes.entrySet()) {
      if (!result.isEmpty()) {
        result += "\n";
      }
      result += process.getKey() + "[env]:\n";
      for (int i = 0; i < process.getValue().getTypeVariableCount(); i++) {
        String pol = process.getValue().isTypeVariablePositive(i) ? "positive" : "negative";
        result += "    type " + i + ": " + pol + "\n";
      }
      result += "    arg records: " + process.getValue().getRecordArgumentCount() + "\n";
      result += "    arg exponentials: " + process.getValue().getExponentialArgumentCount() + "\n";
      result += "    records: " + process.getValue().getRecordCount() + "\n";
      result += "    exponentials: " + process.getValue().getExponentialCount() + "\n";
      result += "    endPoints: " + process.getValue().getEndPoints() + "\n";
      result += "    inlineable: " + process.getValue().isInlineable() + "\n";
      result += "    recursive: " + process.getValue().isRecursive() + "\n";
      result += process.getKey() + ":\n";
      result += process.getValue().toString() + "\n";
    }
    return result;
  }

  public void forEachProcess(BiConsumer<String, IRProcessOld> consumer) {
    processes.forEach(consumer);
  }
}
