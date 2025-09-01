package pt.inescid.cllsj.compiler.ir.instruction;

import java.util.List;
import java.util.function.Function;
import pt.inescid.cllsj.compiler.ir.id.IRDataLocation;
import pt.inescid.cllsj.compiler.ir.id.IRLocalDataId;
import pt.inescid.cllsj.compiler.ir.id.IRProcessId;
import pt.inescid.cllsj.compiler.ir.id.IRSessionId;
import pt.inescid.cllsj.compiler.ir.slot.IRSlotSequence;

public class IRCallProcess extends IRInstruction {
  public static class SessionArgument {
    // Which session in the source process to bind from
    private IRSessionId sourceSessionId;

    // Which session in the target process to bind to
    private IRSessionId targetSessionId;

    // How much to offset the session's remote value in the target process
    private IRSlotSequence valueOffset;

    public SessionArgument(
        IRSessionId sourceSessionId, IRSessionId targetSessionId, IRSlotSequence valueOffset) {
      this.sourceSessionId = sourceSessionId;
      this.targetSessionId = targetSessionId;
      this.valueOffset = valueOffset;
    }

    public IRSessionId getSourceSessionId() {
      return sourceSessionId;
    }

    public IRSessionId getTargetSessionId() {
      return targetSessionId;
    }

    public IRSlotSequence getValueOffset() {
      return valueOffset;
    }

    public void replaceSessions(Function<IRSessionId, IRSessionId> replacer) {
      sourceSessionId = replacer.apply(sourceSessionId);
      targetSessionId = replacer.apply(targetSessionId);
    }

    public SessionArgument clone() {
      return new SessionArgument(sourceSessionId, targetSessionId, valueOffset);
    }
  }
  ;

  public static class TypeArgument {
    public TypeArgument clone() {
      return new TypeArgument();
    }
  }
  ;

  public static class DataArgument {
    // Where to get the data from in the source process
    private IRDataLocation sourceLocation;

    // Where to put the moved data in the target process
    private IRLocalDataId targetDataId;

    // Slots to move from the source to the target
    private IRSlotSequence slots;

    public DataArgument(
        IRDataLocation sourceLocation, IRLocalDataId targetDataId, IRSlotSequence slots) {
      this.sourceLocation = sourceLocation;
      this.targetDataId = targetDataId;
      this.slots = slots;
    }

    public IRDataLocation getSourceLocation() {
      return sourceLocation;
    }

    public IRLocalDataId getTargetDataId() {
      return targetDataId;
    }

    public IRSlotSequence getSlots() {
      return slots;
    }

    public DataArgument clone() {
      return new DataArgument(sourceLocation, targetDataId, slots);
    }
  }
  ;

  private IRProcessId processId;
  private List<SessionArgument> sessionArguments;
  private List<TypeArgument> typeArguments;
  private List<DataArgument> dataArguments;

  public IRCallProcess(
      IRProcessId processId,
      List<SessionArgument> sessionArguments,
      List<TypeArgument> typeArguments,
      List<DataArgument> dataArguments) {
    this.processId = processId;
    this.sessionArguments = sessionArguments;
    this.typeArguments = typeArguments;
    this.dataArguments = dataArguments;
  }

  public IRProcessId getProcessId() {
    return processId;
  }

  public List<SessionArgument> getSessionArguments() {
    return sessionArguments;
  }

  public List<TypeArgument> getTypeArguments() {
    return typeArguments;
  }

  public List<DataArgument> getDataArguments() {
    return dataArguments;
  }

  @Override
  public void accept(IRInstructionVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public void replaceSessions(Function<IRSessionId, IRSessionId> replacer) {
    for (SessionArgument arg : sessionArguments) {
      arg.replaceSessions(replacer);
    }
  }

  @Override
  public void replaceDataLocations(Function<IRDataLocation, IRDataLocation> replacer) {
    for (DataArgument arg : dataArguments) {
      arg.sourceLocation = replacer.apply(arg.sourceLocation);
    }
  }

  @Override
  public IRInstruction clone() {
    return new IRCallProcess(
        processId,
        sessionArguments.stream().map(SessionArgument::clone).toList(),
        typeArguments.stream().map(TypeArgument::clone).toList(),
        dataArguments.stream().map(DataArgument::clone).toList());
  }
}
