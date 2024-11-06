package pt.inescid.cllsj.compiler;

import pt.inescid.cllsj.CLLSj;
import pt.inescid.cllsj.Env;
import pt.inescid.cllsj.EnvEntry;
import pt.inescid.cllsj.ast.nodes.ASTProgram;

public class Compiler {
  public static int compile(String entryProcess) {
    ASTProgram ast;
    try {
      ast = new CLLSj(System.in).Program();
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
      output = Generator.generate(entryProcess, ep, ast);
    } catch (Exception e) {
      System.err.println("Generation error: " + e.getMessage());
      e.printStackTrace();
      return 1;
    }

    System.out.println(output);
    return 0;
  }
}
