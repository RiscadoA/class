package pt.inescid.cllsj.compiler.ir.instruction;

import java.util.Optional;
import java.util.function.Function;
import pt.inescid.cllsj.compiler.ir.id.IRDataLocation;
import pt.inescid.cllsj.compiler.ir.id.IRSessionId;
import pt.inescid.cllsj.compiler.ir.slot.IRSlotOffset;
import pt.inescid.cllsj.compiler.ir.slot.IRSlotTree;

public class IRBindSession extends IRInstruction {
  private IRSessionId targetSessionId;
  private Optional<IRDataLocation> sourceLocation;
  private Optional<IRSessionId> sourceSessionId;
  private IRSlotOffset remoteDataOffset;
  private IRDataLocation localData;

  public IRBindSession(
      IRSessionId targetSessionId,
      IRDataLocation sourceLocation,
      IRSlotOffset remoteDataOffset,
      IRDataLocation localData) {
    this.targetSessionId = targetSessionId;
    this.sourceLocation = Optional.of(sourceLocation);
    this.sourceSessionId = Optional.empty();
    this.remoteDataOffset = remoteDataOffset;
    this.localData = localData;
  }

  public IRBindSession(
      IRSessionId targetSessionId,
      IRSessionId sourceSessionId,
      IRSlotOffset remoteDataOffset,
      IRDataLocation localData) {
    this.targetSessionId = targetSessionId;
    this.sourceLocation = Optional.empty();
    this.sourceSessionId = Optional.of(sourceSessionId);
    this.remoteDataOffset = remoteDataOffset;
    this.localData = localData;
  }

  public IRSessionId getTargetSessionId() {
    return targetSessionId;
  }

  public boolean isFromLocation() {
    return sourceLocation.isPresent();
  }

  public IRDataLocation getSourceLocation() {
    if (!sourceLocation.isPresent()) {
      throw new IllegalStateException("Is not from a location");
    }
    return sourceLocation.get();
  }

  public IRSessionId getSourceSessionId() {
    if (!sourceSessionId.isPresent()) {
      throw new IllegalStateException("Is not from a session ID");
    }
    return sourceSessionId.get();
  }

  public IRSlotOffset getRemoteDataOffset() {
    return remoteDataOffset;
  }

  public IRDataLocation getLocalData() {
    return localData;
  }

  @Override
  public IRInstruction clone() {
    if (isFromLocation()) {
      return new IRBindSession(targetSessionId, getSourceLocation(), remoteDataOffset, localData);
    } else {
      return new IRBindSession(targetSessionId, getSourceSessionId(), remoteDataOffset, localData);
    }
  }

  @Override
  public void accept(IRInstructionVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("bindSession(");
    sb.append(targetSessionId);
    sb.append(", ");
    if (isFromLocation()) {
      sb.append(getSourceLocation());
    } else {
      sb.append(getSourceSessionId());
    }
    sb.append(".");
    sb.append(remoteDataOffset);
    sb.append(", ");
    sb.append(localData);
    sb.append(")");
    return sb.toString();
  }

  @Override
  public void replaceSessions(Function<IRSessionId, IRSessionId> replacer) {
    super.replaceSessions(replacer);
    sourceSessionId = sourceSessionId.map(replacer);
    targetSessionId = replacer.apply(targetSessionId);
  }

  @Override
  public void replaceDataLocations(Function<IRDataLocation, IRDataLocation> replacer) {
    super.replaceDataLocations(replacer);
    sourceLocation = sourceLocation.map(replacer);
    localData = replacer.apply(localData);
  }

  @Override
  public void replaceSlots(Function<IRSlotTree, IRSlotTree> replacer) {
    super.replaceSlots(replacer);
    remoteDataOffset = remoteDataOffset.replaceSlots(replacer);
  }
}
