package pt.inescid.cllsj.compiler.ir;

import pt.inescid.cllsj.Env;
import pt.inescid.cllsj.EnvEntry;
import pt.inescid.cllsj.ast.nodes.ASTProcDef;
import pt.inescid.cllsj.ast.nodes.ASTProgram;
import pt.inescid.cllsj.compiler.Compiler;
import pt.inescid.cllsj.compiler.ir.id.IRProcessId;
import pt.inescid.cllsj.compiler.ir.instruction.IRBlock;
import pt.inescid.cllsj.compiler.ir.instruction.IRProcess;
import pt.inescid.cllsj.compiler.ir.instruction.IRProgram;

public class IRGenerator {
  private Compiler compiler;

  private IRProgram program = new IRProgram();
  private IRProcess process;
  private IRBlock block;

  private Env<EnvEntry> ep;

  public static IRProgram generate(Compiler compiler, Env<EnvEntry> ep, ASTProgram ast) {
    IRGenerator gen = new IRGenerator();
    gen.compiler = compiler;
    gen.ep = ep;

    for (ASTProcDef procDef : ast.getProcDefs()) {
      IRProcessId id = new IRProcessId(procDef.getId());
      // gen.program.add(new IRProcess(id, )
    }

    return gen.program;
  }
}
