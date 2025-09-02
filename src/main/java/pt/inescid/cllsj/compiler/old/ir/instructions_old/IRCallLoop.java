package pt.inescid.cllsj.compiler.ir.old.instructions_old;

import java.util.List;
import java.util.Optional;
import java.util.function.Function;
import pt.inescid.cllsj.compiler.ir.old.type.IRVarT;
import pt.inescid.cllsj.compiler.old.ir.IRInstructionVisitorOld;

public class IRCallLoop extends IRInstruction {
  public static class LinearArgument {
    private int sourceRecord;
    private int targetRecord;

    public LinearArgument(int sourceRecord, int targetRecord) {
      this.sourceRecord = sourceRecord;
      this.targetRecord = targetRecord;
    }

    public int getSourceRecord() {
      return sourceRecord;
    }

    public int getTargetRecord() {
      return targetRecord;
    }
  }

  public static class ExponentialArgument {
    private int sourceExponential;
    private int targetExponential;

    public ExponentialArgument(int sourceExponential, int targetExponential) {
      this.sourceExponential = sourceExponential;
      this.targetExponential = targetExponential;
    }

    public int getSourceExponential() {
      return sourceExponential;
    }

    public int getTargetExponential() {
      return targetExponential;
    }
  }

  private String entryLabel;
  private List<LinearArgument> linearArguments;
  private List<ExponentialArgument> exponentialArguments;

  public IRCallLoop(
      String entryLabel,
      List<LinearArgument> linearArguments,
      List<ExponentialArgument> exponentialArguments) {
    this.entryLabel = entryLabel;
    this.linearArguments = linearArguments;
    this.exponentialArguments = exponentialArguments;
  }

  public String getEntryLabel() {
    return entryLabel;
  }

  public List<LinearArgument> getLinearArguments() {
    return linearArguments;
  }

  public List<ExponentialArgument> getExponentialArguments() {
    return exponentialArguments;
  }

  @Override
  public void accept(IRInstructionVisitorOld visitor) {
    visitor.visit(this);
  }

  @Override
  public String toString() {
    String str = "callLoop(" + entryLabel;
    for (LinearArgument arg : this.linearArguments) {
      str += ", L" + arg.getTargetRecord() + " <- " + arg.getSourceRecord();
    }
    for (ExponentialArgument arg : this.exponentialArguments) {
      str += ", E" + arg.getTargetExponential() + " <- " + arg.getSourceExponential();
    }
    return str + ")";
  }

  public static boolean canGetFromCallProcess(IRCallProcess call) {
    return fromCallProcess("", call, r -> r, e -> e).isPresent();
  }

  public static Optional<IRCallLoop> fromCallProcess(
      String entryLabel,
      IRCallProcess call,
      Function<Integer, Integer> recordRename,
      Function<Integer, Integer> exponentialRename) {
    for (IRCallProcess.TypeArgument arg : call.getTypeArguments()) {
      if (!(arg.getSourceType() instanceof IRVarT)) {
        return Optional.empty();
      }

      IRVarT sourceType = (IRVarT) arg.getSourceType();
      if (sourceType.getType() != arg.getTargetType()) {
        return Optional.empty();
      }
    }

    return Optional.of(
        new IRCallLoop(
            entryLabel,
            call.getLinearArguments().stream()
                .map(
                    arg ->
                        new LinearArgument(
                            arg.getSourceRecord(), recordRename.apply(arg.getTargetRecord())))
                .toList(),
            call.getExponentialArguments().stream()
                .map(
                    arg ->
                        new ExponentialArgument(
                            arg.getSourceExponential(),
                            exponentialRename.apply(arg.getTargetExponential())))
                .toList()));
  }

  @Override
  public IRInstruction clone() {
    return new IRCallLoop(
        entryLabel,
        linearArguments.stream()
            .map(arg -> new LinearArgument(arg.sourceRecord, arg.targetRecord))
            .toList(),
        exponentialArguments.stream()
            .map(arg -> new ExponentialArgument(arg.sourceExponential, arg.targetExponential))
            .toList());
  }

  @Override
  public void renameRecords(Function<Integer, Integer> renamer) {
    for (LinearArgument arg : linearArguments) {
      arg.sourceRecord = renamer.apply(arg.sourceRecord);
      arg.targetRecord = renamer.apply(arg.targetRecord);
    }
  }

  @Override
  public void renameExponentials(Function<Integer, Integer> renamer) {
    for (ExponentialArgument arg : exponentialArguments) {
      arg.sourceExponential = renamer.apply(arg.sourceExponential);
      arg.targetExponential = renamer.apply(arg.targetExponential);
    }
  }

  @Override
  public void renameLabels(Function<String, String> renamer) {
    this.entryLabel = renamer.apply(this.entryLabel);
  }
}
