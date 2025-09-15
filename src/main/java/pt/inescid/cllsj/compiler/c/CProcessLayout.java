package pt.inescid.cllsj.compiler.c;

import java.util.HashMap;
import java.util.Map;
import java.util.function.Function;
import pt.inescid.cllsj.compiler.Compiler;
import pt.inescid.cllsj.compiler.ir.IRValueRequisites;
import pt.inescid.cllsj.compiler.ir.id.IRDropId;
import pt.inescid.cllsj.compiler.ir.id.IRLocalDataId;
import pt.inescid.cllsj.compiler.ir.id.IRSessionId;
import pt.inescid.cllsj.compiler.ir.id.IRTypeId;
import pt.inescid.cllsj.compiler.ir.instruction.IRProcess;

// Stores the offsets of the various sections in a process layout
// A process layout is as follows:
// - end point counter (optional integer)
// - drop bits (optional byte[])
// - type section (struct type[])
// - session section (struct session[])
// - data section (arbitrary data based on local data slot combinations)
public class CProcessLayout {
  private Compiler compiler;
  private IRProcess process;

  private CAlignment alignment;
  private CSize endPointsOffset;
  private CSize dropByteStart;
  private CSize dropByteCount;
  private Map<IRDropId, CSizeBits> dropBitOffsets = new HashMap<>();
  private Map<IRTypeId, CSize> typeOffsets = new HashMap<>();
  private Map<IRSessionId, CSize> sessionOffsets = new HashMap<>();
  private CSize sessionsEnd;

  public static CProcessLayout compute(Compiler compiler, IRProcess process) {
    CProcessLayout result = new CProcessLayout();
    result.compiler = compiler;
    result.process = process;

    result.alignment = compiler.arch.pointerAlignment;

    result.endPointsOffset = CSize.zero();
    CSize endPointsEnd;
    if (compiler.optimizeSingleEndpoint.get() && process.getEndPoints() == 1) {
      endPointsEnd = result.endPointsOffset;
    } else {
      endPointsEnd = result.endPointsOffset.add(compiler.arch.intSize);
    }

    result.dropByteStart = endPointsEnd;
    result.dropByteCount = CSize.zero();
    CSizeBits dropBitNext = CSizeBits.of(result.dropByteStart, 0);
    for (int i = 0; i < process.getDropOnEnd().size(); ++i) {
      IRDropId dropId = new IRDropId(i);
      IRProcess.DropOnEnd drop = process.getDropOnEnd(dropId);
      if (drop.isAlways()) {
        continue; // No need to store a bit if it will always be dropped anyway
      }

      if (dropBitNext.getBits() == 0) {
        result.dropByteCount = result.dropByteCount.add(CSize.constant(1));
      }
      result.dropBitOffsets.put(dropId, dropBitNext);
      if (dropBitNext.getBits() >= 7) {
        dropBitNext = CSizeBits.of(dropBitNext.getSize().add(CSize.constant(1)));
      } else {
        dropBitNext = CSizeBits.of(dropBitNext.getSize(), dropBitNext.getBits() + 1);
      }
    }
    CSize dropBitEnds = result.dropByteStart.add(result.dropByteCount);

    CSize typesStart = dropBitEnds.align(compiler.arch.typeAlignment());
    CSize typesEnd = typesStart.add(compiler.arch.typeSize().multiply(process.getTypeCount()));
    for (int i = 0; i < process.getTypeCount(); ++i) {
      result.typeOffsets.put(new IRTypeId(i), typesStart.add(compiler.arch.typeSize().multiply(i)));
    }

    CSize sessionsStart = typesEnd.align(compiler.arch.sessionAlignment());
    result.sessionsEnd =
        sessionsStart.add(compiler.arch.sessionSize().multiply(process.getSessionCount()));
    for (int i = 0; i < process.getSessionCount(); ++i) {
      result.sessionOffsets.put(
          new IRSessionId(i), sessionsStart.add(compiler.arch.sessionSize().multiply(i)));
    }

    return result;
  }

  public CAlignment alignment() {
    return alignment;
  }

  public CSize endPointsOffset() {
    return endPointsOffset;
  }

  public CSize dropByteStart() {
    return dropByteStart;
  }

  public CSize dropByteCount() {
    return dropByteCount;
  }

  public CSizeBits dropBitOffset(IRDropId dropId) {
    return dropBitOffsets.get(dropId);
  }

  public CSize typeOffset(IRTypeId id) {
    return typeOffsets.get(id);
  }

  public CSize sessionOffset(IRSessionId id) {
    return sessionOffsets.get(id);
  }

  public CSize dataOffset(
      Function<IRTypeId, CLayout> typeLayout,
      Function<IRValueRequisites, CCondition> isValue,
      IRLocalDataId id) {
    CSize dataEnd = sessionsEnd;
    for (int i = 0; i < id.getIndex(); ++i) {
      CLayout layout =
          CLayout.computeMaximum(
              process.getLocalData(new IRLocalDataId(i)), compiler.arch, typeLayout, isValue);
      dataEnd = dataEnd.align(layout.alignment);
      dataEnd = dataEnd.add(layout.size);
    }

    CLayout layout =
        CLayout.computeMaximum(process.getLocalData(id), compiler.arch, typeLayout, isValue);
    return dataEnd.align(layout.alignment);
  }

  public CSize size(
      Function<IRTypeId, CLayout> typeLayoutProvider,
      Function<IRValueRequisites, CCondition> isValue) {
    CSize dataEnd = sessionsEnd;
    for (int i = 0; i < process.getLocalDataCount(); ++i) {
      CLayout layout =
          CLayout.computeMaximum(
              process.getLocalData(new IRLocalDataId(i)),
              compiler.arch,
              typeLayoutProvider,
              isValue);
      dataEnd = dataEnd.align(layout.alignment);
      dataEnd = dataEnd.add(layout.size);
    }
    return dataEnd;
  }
}
