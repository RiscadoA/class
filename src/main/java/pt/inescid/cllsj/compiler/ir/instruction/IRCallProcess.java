package pt.inescid.cllsj.compiler.ir.instruction;

import java.util.List;
import java.util.Optional;
import java.util.function.Function;
import pt.inescid.cllsj.compiler.ir.IRValueRequisites;
import pt.inescid.cllsj.compiler.ir.id.IRDataLocation;
import pt.inescid.cllsj.compiler.ir.id.IRLocalDataId;
import pt.inescid.cllsj.compiler.ir.id.IRProcessId;
import pt.inescid.cllsj.compiler.ir.id.IRSessionId;
import pt.inescid.cllsj.compiler.ir.id.IRTypeId;
import pt.inescid.cllsj.compiler.ir.slot.IRSlotDynamicOffset;
import pt.inescid.cllsj.compiler.ir.slot.IRSlotTree;

public class IRCallProcess extends IRInstruction {
  public static class TypeArgument {
    private Optional<IRDataLocation> sourceLocation;
    private Optional<IRSlotTree> sourceTree;
    private Optional<IRValueRequisites> sourceIsValue;
    private IRTypeId targetType;

    public TypeArgument(IRDataLocation sourceLocation, IRTypeId targetType) {
      this.sourceLocation = Optional.of(sourceLocation);
      this.sourceTree = Optional.empty();
      this.sourceIsValue = Optional.empty();
      this.targetType = targetType;
    }

    public TypeArgument(
        IRSlotTree sourceTree, IRValueRequisites sourceIsValue, IRTypeId targetType) {
      this.sourceLocation = Optional.empty();
      this.sourceTree = Optional.of(sourceTree);
      this.sourceIsValue = Optional.of(sourceIsValue);
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

    public IRValueRequisites getSourceIsValue() {
      return sourceIsValue.orElseThrow();
    }

    public IRTypeId getTargetType() {
      return targetType;
    }

    public TypeArgument clone() {
      if (sourceLocation.isPresent()) {
        return new TypeArgument(sourceLocation.get(), targetType);
      } else {
        return new TypeArgument(sourceTree.get(), sourceIsValue.get(), targetType);
      }
    }

    @Override
    public String toString() {
      if (sourceLocation.isPresent()) {
        return targetType + " <- " + sourceLocation.get();
      } else {
        return targetType + " <- " + sourceTree.get() + " (value=" + sourceIsValue.get() + ")";
      }
    }
  }

  public static class SessionArgument {
    // Data location to fetch the session from in the source process
    private Optional<IRDataLocation> sourceSessionLocation;

    // Which session in the source process to bind from
    private Optional<IRSessionId> sourceSessionId;

    // Which session in the target process to bind to
    private IRSessionId targetSessionId;

    // How much to offset the session's remote data in the target process
    private IRSlotDynamicOffset dataOffset;

    public SessionArgument(
        IRSessionId sourceSessionId, IRSessionId targetSessionId, IRSlotDynamicOffset dataOffset) {
      this.sourceSessionLocation = Optional.empty();
      this.sourceSessionId = Optional.of(sourceSessionId);
      this.targetSessionId = targetSessionId;
      this.dataOffset = dataOffset;
    }

    public SessionArgument(
        IRDataLocation sourceSessionLocation,
        IRSessionId targetSessionId,
        IRSlotDynamicOffset dataOffset) {
      this.sourceSessionLocation = Optional.of(sourceSessionLocation);
      this.sourceSessionId = Optional.empty();
      this.targetSessionId = targetSessionId;
      this.dataOffset = dataOffset;
    }

    public boolean isFromLocation() {
      return sourceSessionLocation.isPresent();
    }

    public IRDataLocation getSourceSessionLocation() {
      return sourceSessionLocation.orElseThrow();
    }

    public IRSessionId getSourceSessionId() {
      return sourceSessionId.orElseThrow();
    }

    public IRSessionId getTargetSessionId() {
      return targetSessionId;
    }

    public IRSlotDynamicOffset getDataOffset() {
      return dataOffset;
    }

    public void replaceDataLocations(Function<IRDataLocation, IRDataLocation> replacer) {
      sourceSessionLocation = sourceSessionLocation.map(replacer);
    }

    public void replaceSessions(Function<IRSessionId, IRSessionId> replacer) {
      sourceSessionId = sourceSessionId.map(replacer);
      targetSessionId = replacer.apply(targetSessionId);
    }

    public SessionArgument clone() {
      if (sourceSessionLocation.isPresent()) {
        return new SessionArgument(sourceSessionLocation.get(), targetSessionId, dataOffset);
      } else {
        return new SessionArgument(sourceSessionId.get(), targetSessionId, dataOffset);
      }
    }

    @Override
    public String toString() {
      if (sourceSessionLocation.isPresent()) {
        return targetSessionId + " <- " + sourceSessionLocation.get() + dataOffset;
      } else {
        return targetSessionId + " <- " + sourceSessionId.get() + dataOffset;
      }
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
    for (TypeArgument arg : typeArguments) {
      arg.sourceLocation = arg.sourceLocation.map(replacer);
    }
    for (DataArgument arg : dataArguments) {
      arg.sourceLocation = replacer.apply(arg.sourceLocation);
    }
    for (SessionArgument arg : sessionArguments) {
      arg.replaceDataLocations(replacer);
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
