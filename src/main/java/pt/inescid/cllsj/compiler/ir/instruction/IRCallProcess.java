package pt.inescid.cllsj.compiler.ir.instruction;

import java.util.List;
import java.util.Optional;
import java.util.function.Function;
import pt.inescid.cllsj.compiler.ir.id.IRDataLocation;
import pt.inescid.cllsj.compiler.ir.id.IRLocalDataId;
import pt.inescid.cllsj.compiler.ir.id.IRProcessId;
import pt.inescid.cllsj.compiler.ir.id.IRSessionId;
import pt.inescid.cllsj.compiler.ir.id.IRTypeId;
import pt.inescid.cllsj.compiler.ir.slot.IRSlotOffset;
import pt.inescid.cllsj.compiler.ir.slot.IRSlotTree;

public class IRCallProcess extends IRInstruction {
  public static class TypeArgument {
    private Optional<IRDataLocation> sourceLocation;
    private Optional<IRSlotTree> sourceTree;
    private IRTypeId targetType;

    public TypeArgument(IRDataLocation sourceLocation, IRTypeId targetType) {
      this.sourceLocation = Optional.of(sourceLocation);
      this.sourceTree = Optional.empty();
      this.targetType = targetType;
    }

    public TypeArgument(IRSlotTree sourceTree, IRTypeId targetType) {
      this.sourceTree = Optional.of(sourceTree);
      this.targetType = targetType;
    }

    public boolean isFromLocation() {
      return sourceLocation.isPresent();
    }

    public IRDataLocation getSourceLocation() {
      return sourceLocation.orElseThrow();
    }

    public IRSlotTree getSourceTree() {
      return sourceTree.orElseThrow();
    }

    public IRTypeId getTargetType() {
      return targetType;
    }

    public TypeArgument clone() {
      if (sourceLocation.isPresent()) {
        return new TypeArgument(sourceLocation.get(), targetType);
      } else {
        return new TypeArgument(sourceTree.get(), targetType);
      }
    }

    @Override
    public String toString() {
      return targetType + " <- " + (sourceLocation.isPresent() ? sourceLocation.get() : sourceTree.get());
    }
  }

  public static class SessionArgument {
    // Which session in the source process to bind from
    private IRSessionId sourceSessionId;

    // Which session in the target process to bind to
    private IRSessionId targetSessionId;

    // How much to offset the session's remote data in the target process
    private IRSlotOffset dataOffset;

    public SessionArgument(
        IRSessionId sourceSessionId, IRSessionId targetSessionId, IRSlotOffset dataOffset) {
      this.sourceSessionId = sourceSessionId;
      this.targetSessionId = targetSessionId;
      this.dataOffset = dataOffset;
    }

    public IRSessionId getSourceSessionId() {
      return sourceSessionId;
    }

    public IRSessionId getTargetSessionId() {
      return targetSessionId;
    }

    public IRSlotOffset getDataOffset() {
      return dataOffset;
    }

    public void replaceSessions(Function<IRSessionId, IRSessionId> replacer) {
      sourceSessionId = replacer.apply(sourceSessionId);
      targetSessionId = replacer.apply(targetSessionId);
    }

    public SessionArgument clone() {
      return new SessionArgument(sourceSessionId, targetSessionId, dataOffset);
    }

    @Override
    public String toString() {
      return targetSessionId + " <- " + sourceSessionId + dataOffset;
    }
  }

  public static class DataArgument {
    // Where to get the data from in the source process
    private IRDataLocation sourceLocation;

    // Where to put the moved data in the target process
    private IRLocalDataId targetDataId;

    // Slots to move from the source to the target
    private IRSlotTree slots;

    // Whether the data should be cloned (otherwise moved)
    private boolean clone;

    public DataArgument(
        IRDataLocation sourceLocation,
        IRLocalDataId targetDataId,
        IRSlotTree slots,
        boolean clone) {
      this.sourceLocation = sourceLocation;
      this.targetDataId = targetDataId;
      this.slots = slots;
      this.clone = clone;
    }

    public IRDataLocation getSourceLocation() {
      return sourceLocation;
    }

    public IRLocalDataId getTargetDataId() {
      return targetDataId;
    }

    public IRSlotTree getSlots() {
      return slots;
    }

    public boolean isClone() {
      return clone;
    }

    public DataArgument clone() {
      return new DataArgument(sourceLocation, targetDataId, slots, clone);
    }

    @Override
    public String toString() {
      StringBuilder sb = new StringBuilder();
      sb.append(targetDataId);
      sb.append(clone ? " =" : " <-");
      sb.append(slots);
      sb.append(" ").append(sourceLocation);
      return sb.toString();
    }
  }

  protected IRProcessId processId;
  protected List<TypeArgument> typeArguments;
  protected List<SessionArgument> sessionArguments;
  protected List<DataArgument> dataArguments;
  protected boolean isEndPoint;

  public IRCallProcess(
      IRProcessId processId,
      List<TypeArgument> typeArguments,
      List<SessionArgument> sessionArguments,
      List<DataArgument> dataArguments,
      boolean isEndPoint) {
    this.processId = processId;
    this.typeArguments = typeArguments;
    this.sessionArguments = sessionArguments;
    this.dataArguments = dataArguments;
    this.isEndPoint = isEndPoint;
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

  public boolean isEndPoint() {
    return isEndPoint;
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
        typeArguments.stream().map(TypeArgument::clone).toList(),
        sessionArguments.stream().map(SessionArgument::clone).toList(),
        dataArguments.stream().map(DataArgument::clone).toList(),
        isEndPoint);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("callProcess(").append(processId);
    if (isEndPoint) {
      sb.append(", end point");
    }
    for (TypeArgument arg : typeArguments) {
      sb.append(", ").append(arg.toString());
    }
    for (SessionArgument arg : sessionArguments) {
      sb.append(", ").append(arg.toString());
    }
    for (DataArgument arg : dataArguments) {
      sb.append(", ").append(arg.toString());
    }
    sb.append(")");
    return sb.toString();
  }
}
