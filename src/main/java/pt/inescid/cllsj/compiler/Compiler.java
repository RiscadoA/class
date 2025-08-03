package pt.inescid.cllsj.compiler;

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
import pt.inescid.cllsj.compiler.ir.IRProgram;

public class Compiler {
  public String entryProcess = "main";
  public boolean trace = false;
  public boolean profile = false;
  public boolean onlyIR = false;
  public boolean onlyAST = false;
  public boolean disableConcurrency = false;
  public int customAllocatorSizeDivisor = 32;
  public int customAllocatorLevels = 8;
  public boolean optimizePrimitiveExponentials = true;
  public boolean optimizeExponentialExpressionToForward = true;
  public boolean optimizeSendForward = true;
  public boolean optimizeTailCalls = true;
  public boolean optimizeFlipForward = true;

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
      SessionRenamer.execute(ast);
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

    IRProgram ir;
    try {
      IRGenerator gen = new IRGenerator();
      gen.optimizeExponentialExpressionToForward = optimizeExponentialExpressionToForward;
      gen.optimizeSendForward = optimizeSendForward;
      ir = gen.generate(ep, ast);
    } catch (Exception e) {
      System.err.println("IR generation error: " + e.getMessage());
      e.printStackTrace();
      return 1;
    }

    try {
      IROptimizer optimizer = new IROptimizer();
      if (optimizeFlipForward) {
        optimizer.optimizeFlipForward(ir);
      }
    } catch (Exception e) {
      System.err.println("IR optimization error: " + e.getMessage());
      e.printStackTrace();
      return 1;
    }

    if (onlyIR) {
      System.out.print(ir.toString());
      return 0;
    }

    String output;
    try {
      CGenerator gen = new CGenerator();
      gen.entryProcess = entryProcess;
      gen.trace = trace;
      gen.profile = profile;
      gen.disableConcurrency = disableConcurrency;
      gen.customAllocatorSizeDivisor = customAllocatorSizeDivisor;
      gen.customAllocatorLevels = customAllocatorLevels;
      gen.optimizePrimitiveExponentials = optimizePrimitiveExponentials;
      gen.optimizeTailCalls = optimizeTailCalls;
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
