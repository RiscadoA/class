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

public class Compiler {
  public static int compile(String path, String entryProcess, boolean trace) {
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

    String output;
    try {
      output = Generator.generate(entryProcess, ep, ast, trace);
    } catch (Exception e) {
      System.err.println("Generation error: " + e.getMessage());
      e.printStackTrace();
      return 1;
    }

    System.out.println(output);
    return 0;
  }

  private static ASTProgram parse(Path path) throws Exception {
    return parse(path, new HashSet<>());
  }

  private static ASTProgram parse(Path path, Set<String> included) throws Exception {
    FileInputStream stream = new FileInputStream(path.toFile());
    ASTProgramWithIncludes astWithIncs = new CLLSj(stream).Program();
    if (astWithIncs == null) {
      return null;
    }

    // Parse each of the includes.
    List<ASTDList> dLists = new ArrayList<>();
    List<ASTPList> pLists = new ArrayList<>();

    for (ASTInclude inc : astWithIncs.getIncs()) {
      Path incPath = path.getParent().resolve(inc.getFn());
      if (!included.add(incPath.toString())) {
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
