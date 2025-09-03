package pt.inescid.cllsj.compiler.c;

import java.util.HashMap;
import java.util.Map;
import java.util.function.Function;
import pt.inescid.cllsj.compiler.Compiler;
import pt.inescid.cllsj.compiler.ir.id.IRLocalDataId;
import pt.inescid.cllsj.compiler.ir.id.IRSessionId;
import pt.inescid.cllsj.compiler.ir.id.IRTypeId;
import pt.inescid.cllsj.compiler.ir.instruction.IRProcess;

// Stores the offsets of the various sections in a process layout
// A process layout is as follows:
// - end point counter (optional integer)
// - type section (struct type[])
// - session section (struct session[])
// - data section (arbitrary data based on local data slot combinations)
public class CProcessLayout {
  private Compiler compiler;
  private IRProcess process;

  private CAlignment alignment;
  private CSize endPointsOffset;
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

    CSize typesStart = endPointsEnd.align(compiler.arch.typeAlignment());
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

  public CSize typeOffset(IRTypeId id) {
    return typeOffsets.get(id);
  }

  public CSize sessionOffset(IRSessionId id) {
    return sessionOffsets.get(id);
  }

  public CSize dataOffset(Function<IRTypeId, CLayout> typeLayoutProvider, IRLocalDataId id) {
    CSize dataEnd = sessionsEnd;
    for (int i = 0; i < id.getIndex(); ++i) {
      CLayout layout =
          CLayout.compute(
              process.getLocalData(new IRLocalDataId(i)), compiler.arch, typeLayoutProvider);
      dataEnd = dataEnd.align(layout.alignment);
      dataEnd = dataEnd.add(layout.size);
    }

    CLayout layout = CLayout.compute(process.getLocalData(id), compiler.arch, typeLayoutProvider);
    return dataEnd.align(layout.alignment);
  }

  public CSize size(Function<IRTypeId, CLayout> typeLayoutProvider) {
    CSize dataEnd = sessionsEnd;
    for (int i = 0; i < process.getLocalDataCount(); ++i) {
      CLayout layout =
          CLayout.compute(
              process.getLocalData(new IRLocalDataId(i)), compiler.arch, typeLayoutProvider);
      dataEnd = dataEnd.align(layout.alignment);
      dataEnd = dataEnd.add(layout.size);
    }
    return dataEnd;
  }
}
