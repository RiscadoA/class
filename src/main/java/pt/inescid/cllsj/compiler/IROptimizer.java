package pt.inescid.cllsj.compiler;

import java.util.HashMap;
import java.util.Map;
import pt.inescid.cllsj.compiler.ir.IRBlock;
import pt.inescid.cllsj.compiler.ir.IRProcess;
import pt.inescid.cllsj.compiler.ir.IRProgram;
import pt.inescid.cllsj.compiler.ir.flow.IRFlow;
import pt.inescid.cllsj.compiler.ir.instructions.IRFlip;
import pt.inescid.cllsj.compiler.ir.instructions.IRFlipForward;
import pt.inescid.cllsj.compiler.ir.instructions.IRForward;
import pt.inescid.cllsj.compiler.ir.instructions.IRInstruction;

public class IROptimizer {
  private Map<String, IRFlow> processFlows = new HashMap<>();

  public void analyze(IRProgram program) {
    for (Map.Entry<String, IRProcess> e : program.getProcesses().entrySet()) {
      processFlows.put(e.getKey(), IRAnalyzer.analyze(e.getValue()));
    }
  }

  public void optimizeFlipForward(IRProgram ir) {
    for (IRProcess process : ir.getProcesses().values()) {
      optimizeFlipForward(process.getEntry());
      for (IRBlock block : process.getBlocks()) {
        optimizeFlipForward(block);
      }
    }
  }

  private void optimizeFlipForward(IRBlock ir) {
    int size = ir.getInstructions().size();
    if (size < 2) {
      return; // Not what we're looking for
    }
    IRInstruction maybeFlip = ir.getInstructions().get(size - 2);
    IRInstruction maybeForward = ir.getInstructions().get(size - 1);
    if (!(maybeFlip instanceof IRFlip && maybeForward instanceof IRForward)) {
      return; // Not what we're looking for
    }
    IRFlip flip = (IRFlip) maybeFlip;
    IRForward forward = (IRForward) maybeForward;
    if (flip.getRecord() != forward.getNegRecord()) {
      return; // Not what we're looking for
    }
    int x = forward.getNegRecord();
    int y = forward.getPosRecord();

    // We've found a block of the form:
    //
    //     flip(x)
    //     forward(-x, +y)
    //
    // Our goal is to avoid the flip entirely by executing the forward earlier
    // The tradeoff here is that we might need to allocate more memory for the merged record
    // What we gain is that we deallocate unnecessary memory earlier and avoid a costly flip.

    ir.getInstructions().removeLast();
    ir.getInstructions().removeLast();

    ir.add(new IRFlipForward(x, y));
  }
}
