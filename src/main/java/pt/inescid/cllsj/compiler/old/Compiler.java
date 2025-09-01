package pt.inescid.cllsj.compiler.old;

import java.io.FileInputStream;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import pt.inescid.cllsj.CLLSj;
import pt.inescid.cllsj.Env;
import pt.inescid.cllsj.EnvEntry;
import pt.inescid.cllsj.ast.nodes.ASTDList;
import pt.inescid.cllsj.ast.nodes.ASTInclude;
import pt.inescid.cllsj.ast.nodes.ASTPList;
import pt.inescid.cllsj.ast.nodes.ASTProgram;
import pt.inescid.cllsj.ast.nodes.ASTProgramWithIncludes;
import pt.inescid.cllsj.compiler.ast.ASTPrinter;
import pt.inescid.cllsj.compiler.ast.ASTSessionRenamer;
import pt.inescid.cllsj.compiler.ir.old.IRProgramOld;

public class Compiler {
  public String entryProcess = "main";
  public boolean trace = false;
  public boolean debug = false;
  public boolean profile = false;
  public boolean onlyIR = false;
  public boolean onlyAST = false;
  public boolean onlyAnalyze = false;
  public boolean disableConcurrency = false;
  public int customAllocatorSizeDivisor = 32;
  public int customAllocatorLevels = 8;
  public int inliningComplexity = 20;
  public boolean optimizeIRWithAnalysis = true;
  public boolean optimizePrimitiveExponentials = true;
  public boolean optimizeExponentialExpressionToForward = true;
  public boolean optimizeSendForward = true;
  public boolean optimizeTailCalls = true;
  public boolean optimizeFlipForward = true;
  public boolean optimizeSendValue = true;
  public boolean optimizeKnownJumps = true;
  public boolean optimizeKnownSlots = true;
  public boolean optimizeKnownEndPoints = true;
  public boolean optimizeSingleEndpoint = true;
  public boolean inlineRecursiveProcesses = true;

  public int compile(String path) {
    ASTProgram ast;
    try {
      ast = Compiler.parse(Path.of(path));
    } catch (Exception e) {
      System.err.println("Parsing error: " + e.getMessage());
      e.printStackTrace();
      return 1;
    }

    if (ast == null) {
      System.err.println("Parsing error: AST is null");
      return 1;
    }

    try {
      ASTSessionRenamer.execute(ast);
    } catch (Exception e) {
      System.err.println("Renaming error: " + e.getMessage());
      e.printStackTrace();
      return 1;
    }

    Env<EnvEntry> ep = new Env<>();

    try {
      ast.typecheck(new Env<>(), new Env<>(), ep);
      ep = ast.define(ep);
    } catch (Exception e) {
      System.err.println("Typechecking error: " + e.getMessage());
      e.printStackTrace();
      return 1;
    }

    if (onlyAST) {
      ASTPrinter.print(System.out, ast);
      return 0;
    }

    IRProgramOld ir;
    try {
      IRGenerator gen = new IRGenerator();
      gen.optimizeExponentialExpressionToForward = optimizeExponentialExpressionToForward;
      gen.optimizeSendForward = optimizeSendForward;
      gen.optimizeSendValue = optimizeSendValue;
      ir = gen.generate(ep, ast);
    } catch (Exception e) {
      System.err.println("IR generation error: " + e.getMessage());
      e.printStackTrace();
      return 1;
    }

    // try {
    //   IROptimizer optimizer = new IROptimizer();

    //   if (inliningComplexity >= 0) {
    //     optimizer.inlineProcesses(ir, inliningComplexity, false);
    //     optimizer.removeUnusedProcesses(ir, entryProcess);
    //   }

    //   if (optimizeIRWithAnalysis) {
    //     optimizer.analyze(ir);

    //     if (optimizeKnownJumps) {
    //       optimizer.optimizeKnownJumps(ir);
    //     }

    //     if (optimizeSendValue) {
    //       optimizer.removeUnnecessaryValuePushes(ir);
    //       optimizer.removeUnnecessaryValuePops(ir);
    //     }

    //     if (optimizeKnownSlots) {
    //       optimizer.analyze(ir);
    //       optimizer.optimizeKnownSlots(ir);
    //     }

    //     if (optimizeFlipForward) {
    //       optimizer.optimizeFlipForward(ir);
    //     }

    //     if (optimizeKnownEndPoints) {
    //       optimizer.optimizeKnownEndPoints(ir);
    //     }

    //     if (optimizeKnownJumps) {
    //       optimizer.removeUnreachableBlocks(ir);
    //     }
    //     optimizer.removeUnusedRecords(ir);
    //     optimizer.removeUnusedExponentials(ir);

    //     if (onlyAnalyze) {
    //       optimizer.printProcessFlows(ir);
    //       return 0;
    //     }
    //   } else if (onlyAnalyze) {
    //     optimizer.analyze(ir);
    //     optimizer.printProcessFlows(ir);
    //     return 0;
    //   }

    //   if (inliningComplexity >= 0 && inlineRecursiveProcesses) {
    //     optimizer.inlineProcesses(ir, inliningComplexity, true);
    //   }

    //   optimizer.removeUnusedProcesses(ir, entryProcess);
    // } catch (Exception e) {
    //   System.err.println("IR optimization error: " + e.getMessage());
    //   e.printStackTrace();
    //   return 1;
    // }

    if (onlyIR) {
      System.out.print(ir.toString());
      return 0;
    }

    String output;
    try {
      CGenerator gen = new CGenerator();
      gen.entryProcess = entryProcess;
      gen.trace = trace;
      gen.debug = debug;
      gen.profile = profile;
      gen.disableConcurrency = disableConcurrency;
      gen.customAllocatorSizeDivisor = customAllocatorSizeDivisor;
      gen.customAllocatorLevels = customAllocatorLevels;
      gen.optimizePrimitiveExponentials = optimizePrimitiveExponentials;
      gen.optimizeTailCalls = optimizeTailCalls;
      gen.optimizeSendValue = optimizeSendValue;
      gen.optimizeSingleEndpoint = optimizeSingleEndpoint;
      output = gen.generate(ir);
    } catch (Exception e) {
      System.err.println("C Generation error: " + e.getMessage());
      e.printStackTrace();
      return 1;
    }

    System.out.println(output);
    return 0;
  }

  private static ASTProgram parse(Path path) throws Exception {
    HashSet<String> included = new HashSet<>();
    included.add(path.toAbsolutePath().normalize().toString());
    return parse(path, included);
  }

  private static ASTProgram parse(Path path, Set<String> included) throws Exception {
    FileInputStream stream = new FileInputStream(path.toFile());
    ASTProgramWithIncludes astWithIncs = new CLLSj(stream).Program();
    stream.close();
    if (astWithIncs == null) {
      return null;
    }

    // Parse each of the includes.
    List<ASTDList> dLists = new ArrayList<>();
    List<ASTPList> pLists = new ArrayList<>();

    for (ASTInclude inc : astWithIncs.getIncs()) {
      Path incPath = path.getParent().resolve(inc.getFn());
      if (!included.add(incPath.toAbsolutePath().normalize().toString())) {
        continue;
      }

      ASTProgram incAst = parse(incPath, included);
      if (incAst == null) {
        return null;
      }

      // Add the type definitions and procedure definitions from the include to the main AST.
      dLists.addAll(incAst.getDLists());
      pLists.addAll(incAst.getPLists());
    }

    dLists.addAll(astWithIncs.getDLists());
    pLists.addAll(astWithIncs.getPLists());

    return new ASTProgram(dLists, pLists);
  }
}
