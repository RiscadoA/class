package pt.inescid.cllsj.compiler.ir.instruction;

import java.util.List;
import java.util.function.Function;
import pt.inescid.cllsj.compiler.ir.id.IRDataLocation;
import pt.inescid.cllsj.compiler.ir.id.IRLocalDataId;
import pt.inescid.cllsj.compiler.ir.id.IRProcessId;
import pt.inescid.cllsj.compiler.ir.slot.IRSlotSequence;

public class IRWriteExponentialFromProcess extends IRWrite {
  public static class TypeArgument {
    public TypeArgument clone() {
      return new TypeArgument();
    }
  }

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

  private IRProcessId processId;
  private List<TypeArgument> typeArguments;
  private List<DataArgument> dataArguments;

  public IRWriteExponentialFromProcess(
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
  public void replaceDataLocations(Function<IRDataLocation, IRDataLocation> replacer) {
    super.replaceDataLocations(replacer);
    for (DataArgument arg : dataArguments) {
      arg.sourceLocation = replacer.apply(arg.sourceLocation);
    }
  }

  @Override
  public IRInstruction clone() {
    return new IRWriteExponentialFromProcess(
        location,
        processId,
        typeArguments.stream().map(TypeArgument::clone).toList(),
        dataArguments.stream().map(DataArgument::clone).toList());
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("writeExponentialFromProcess(").append(processId);
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
