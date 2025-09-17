package pt.inescid.cllsj.compiler.ir.instruction;

import java.util.List;
import java.util.function.Function;
import pt.inescid.cllsj.compiler.ir.IRValueRequisites;
import pt.inescid.cllsj.compiler.ir.id.IRDataLocation;
import pt.inescid.cllsj.compiler.ir.id.IRLocalDataId;
import pt.inescid.cllsj.compiler.ir.id.IRProcessId;
import pt.inescid.cllsj.compiler.ir.id.IRTypeId;
import pt.inescid.cllsj.compiler.ir.slot.IRSlotTree;

public class IRWriteExponential extends IRWrite {
  public static class TypeArgument {
    private IRSlotTree sourceTree;
    private IRValueRequisites sourceIsValue;
    private IRTypeId targetType;

    public TypeArgument(
        IRSlotTree sourceTree, IRValueRequisites sourceIsValue, IRTypeId targetType) {
      this.sourceTree = sourceTree;
      this.sourceIsValue = sourceIsValue;
      this.targetType = targetType;
    }

    public IRSlotTree getSourceTree() {
      return sourceTree;
    }

    public IRValueRequisites getSourceIsValue() {
      return sourceIsValue;
    }

    public IRTypeId getTargetType() {
      return targetType;
    }

    public TypeArgument clone() {
      return new TypeArgument(sourceTree, sourceIsValue, targetType);
    }

    @Override
    public String toString() {
      return targetType + " <- " + sourceTree + " (value=" + sourceIsValue + ")";
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

  private IRProcessId processId;
  private List<TypeArgument> typeArguments;
  private List<DataArgument> dataArguments;

  public IRWriteExponential(
      IRDataLocation location,
      IRProcessId processId,
      List<TypeArgument> typeArguments,
      List<DataArgument> dataArguments) {
    super(location);
    this.processId = processId;
    this.typeArguments = typeArguments;
    this.dataArguments = dataArguments;
  }

  public IRProcessId getProcessId() {
    return processId;
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
  public void replaceLocalData(Function<IRLocalDataId, IRLocalDataId> replacer) {
    super.replaceLocalData(replacer);
  }

  @Override
  public void replaceSlots(Function<IRSlotTree, IRSlotTree> replacer) {
    super.replaceSlots(replacer);
    for (DataArgument arg : dataArguments) {
      arg.slots = replacer.apply(arg.slots);
    }
    for (TypeArgument arg : typeArguments) {
      arg.sourceTree = replacer.apply(arg.sourceTree);
    }
  }

  @Override
  public void replaceType(
      Function<IRTypeId, IRSlotTree> slotReplacer,
      Function<IRTypeId, IRValueRequisites> reqReplacer) {
    super.replaceType(slotReplacer, reqReplacer);
    for (TypeArgument arg : typeArguments) {
      arg.sourceIsValue = reqReplacer.apply(arg.targetType);
    }
  }

  @Override
  public void replaceDataLocations(Function<IRDataLocation, IRDataLocation> replacer) {
    super.replaceDataLocations(replacer);
    for (DataArgument arg : dataArguments) {
      arg.sourceLocation = replacer.apply(arg.sourceLocation);
    }
  }

  @Override
  public void replaceProcesses(Function<IRProcessId, IRProcessId> replacer) {
    super.replaceProcesses(replacer);
    processId = replacer.apply(processId);
  }

  @Override
  public IRInstruction clone() {
    return new IRWriteExponential(
        location,
        processId,
        typeArguments.stream().map(TypeArgument::clone).toList(),
        dataArguments.stream().map(DataArgument::clone).toList());
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("writeExponential(");
    sb.append(location).append(", ");
    sb.append(processId);
    for (TypeArgument arg : typeArguments) {
      sb.append(", ").append(arg.toString());
    }
    for (DataArgument arg : dataArguments) {
      sb.append(", ").append(arg.toString());
    }
    sb.append(")");
    return sb.toString();
  }
}
