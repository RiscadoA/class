package pt.inescid.cllsj.compiler;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.InputStream;
import java.io.PrintStream;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.function.Consumer;
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
import pt.inescid.cllsj.compiler.c.CAlignment;
import pt.inescid.cllsj.compiler.c.CArchitecture;
import pt.inescid.cllsj.compiler.c.CGenerator;
import pt.inescid.cllsj.compiler.c.CSize;
import pt.inescid.cllsj.compiler.ir.IRGenerator;
import pt.inescid.cllsj.compiler.ir.instruction.IRProgram;

public class Compiler {
  private Settings settings = new Settings();

  public Settings.Path sourceFile;
  public Settings.Path outputASTFile;
  public Settings.Path outputInitialIRFile;
  public Settings.Path outputFinalIRFile;
  public Settings.Path outputCFile;

  public Settings.Name entryProcess;

  public Settings.Flag tracing;
  public Settings.Flag debug;
  public Settings.Flag profiling;
  public Settings.Flag concurrency;

  public Settings.Int allocatorLevels;
  public Settings.Int allocatorSizeDivisor;

  public CArchitecture arch;
  private Settings.Int intSize;
  private Settings.Int intAlignment;
  private Settings.Int unsignedCharSize;
  private Settings.Int unsignedCharAlignment;
  private Settings.Int pointerSize;
  private Settings.Int pointerAlignment;

  public Settings.Flag optimizeSingleEndpoint;
  public Settings.Flag optimizeSendForward;

  public Compiler() {
    // Compiler operation settings
    sourceFile = settings.addPath("i", "input", "Source file to compile (omit for stdin)", null);
    outputASTFile =
        settings.addPath("oa", "output-ast", "File to output the AST representation", null);
    outputInitialIRFile =
        settings.addPath(
            "oi1",
            "output-initial-ir",
            "File to output the initial (unoptimized) IR representation",
            null);
    outputFinalIRFile =
        settings.addPath(
            "oi2",
            "output-final-ir",
            "File to output the final (optimized) IR representation",
            null);
    outputCFile =
        settings.addPath(
            "o", "output-c", "File to output the generated C code (omit for stdout)", null);

    // Generation flags
    entryProcess = settings.addName("e", "entry", "Entry process name", "main");
    tracing =
        settings.addFlag("t", "tracing", "Generates tracing prints for each IR instruction", false);
    debug =
        settings.addFlag(
            "v", "debug", "Generates very verbose prints for each IR instruction", false);
    profiling =
        settings.addFlag(
            "p",
            "profiling",
            "Generates trackers for profiling memory usage and detecting leaks",
            false);

    concurrency =
        settings.addFlag(
            "concurrency",
            "Generates concurrency aware code, enabling concurrent constructs",
            true);

    allocatorLevels =
        settings.addInt(
            "allocator-levels", "Sets the number of levels in the custom allocator", 64);
    allocatorSizeDivisor =
        settings.addInt(
            "allocator-size-divisor", "Sets the number of levels in the custom allocator", 4);

    intSize = settings.addInt("int-size", "Size of int C type in bytes", 4);
    intAlignment = settings.addInt("int-alignment", "Alignment of int C type in bytes", 4);
    unsignedCharSize =
        settings.addInt("unsigned-char-size", "Size of unsigned char C type in bytes", 1);
    unsignedCharAlignment =
        settings.addInt("unsigned-char-alignment", "Alignment of unsigned char C type in bytes", 1);
    pointerSize = settings.addInt("pointer-size", "Size of pointer C type in bytes", 8);
    pointerAlignment =
        settings.addInt("pointer-alignment", "Alignment of pointer C type in bytes", 8);

    optimizeSingleEndpoint =
        settings.addFlag(
            "optimize-single-endpoint",
            "Optimizes away reference counting for processes with a single end point",
            true);
    optimizeSendForward =
        settings.addFlag(
            "optimize-send-forward",
            "Optimizes away reference counting for processes with a single end point",
            true);

    settings.addMode(
        "Os",
        "optimize-sequential",
        "Modifies setting defaults to maximize performance on sequential code (concurrency is disabled)",
        () -> {
          concurrency.set(false);
        });
    
    settings.addMode(
        "O0",
        "no-optimization", 
        "Disables all optimization flags",
      () -> {
        optimizeSingleEndpoint.set(false);
        optimizeSendForward.set(false);
      }
    );

    settings.addMode(
        "32",
        "32-bits",
        "Changes the target architecture's pointer size to 4 bytes",
        () -> {
          pointerSize.set(4);
          pointerAlignment.set(4);
        });

    settings.addMode(
        "ignore-alignment",
        "Sets all alignments to 1, which leads to data being packed together",
        () -> {
          intAlignment.set(1);
          unsignedCharAlignment.set(1);
          pointerAlignment.set(1);
        });

    settings.addMode(
        "no-custom-allocator",
        "Uses malloc/free directly instead of the custom allocator",
        () -> {
          allocatorLevels.set(0);
        });
  }

  public int compile(String args[]) {
    // Parse settings
    try {
      settings.parse(args);
    } catch (RuntimeException e) {
      System.err.println("Argument parsing failed with: " + e.toString());
      settings.printHelp(System.err);
      return 1;
    }

    // Build architecture from settings
    arch = new CArchitecture();
    arch.intSize = CSize.constant(intSize.get());
    arch.intAlignment = CAlignment.constant(intAlignment.get());
    arch.unsignedCharSize = CSize.constant(unsignedCharSize.get());
    arch.unsignedCharAlignment = CAlignment.constant(unsignedCharAlignment.get());
    arch.pointerSize = CSize.constant(pointerSize.get());
    arch.pointerAlignment = CAlignment.constant(pointerAlignment.get());

    // Parse the source into an AST
    ASTProgram ast;
    try {
      ast = parse();
    } catch (Exception e) {
      System.err.println("Error parsing source code: " + e.getMessage());
      e.printStackTrace();
      return 1;
    }

    // Rename names on the AST
    try {
      ASTSessionRenamer.execute(ast);
    } catch (Exception e) {
      System.err.println("Error renaming AST: " + e.getMessage());
      e.printStackTrace();
      return 1;
    }

    // Perform type checking
    Env<EnvEntry> ep = new Env<>();
    try {
      ast.typecheck(new Env<>(), new Env<>(), ep);
      ep = ast.define(ep);
    } catch (Exception e) {
      System.err.println("Error typechecking: " + e.getMessage());
      e.printStackTrace();
      return 1;
    }

    // Output the AST if requested
    openFileForOutput(outputASTFile, stream -> ASTPrinter.print(stream, ast));

    // Generate the initial IR
    IRProgram ir;
    try {
      ir = IRGenerator.generate(this, ep, ast);
    } catch (Exception e) {
      System.err.println("Error generating initial IR: " + e.getMessage());
      e.printStackTrace();
      return 1;
    }

    // Output the initial IR if requested
    openFileForOutput(outputInitialIRFile, stream -> stream.print(ir));

    // TODO: optimize the IR

    // Output the final IR if requested
    openFileForOutput(outputFinalIRFile, stream -> stream.print(ir));

    // Generate C code directly to the requested output
    try {
      openFileOrStdoutForOutput(outputCFile, stream -> CGenerator.generate(this, ir, stream));
    } catch (Exception e) {
      System.err.println("Error generating C code: " + e.getMessage());
      e.printStackTrace();
      return 1;
    }

    return 0;
  }

  private ASTProgram parse() throws Exception {
    if (sourceFile.get() == null) {
      return parse(System.in, Path.of("."));
    } else {
      return parse(sourceFile.get());
    }
  }

  private void openFileOrStdoutForOutput(Settings.Path path, Consumer<PrintStream> consumer) {
    if (path.get() == null) {
      consumer.accept(System.out);
    } else {
      openFileForOutput(path, consumer);
    }
  }

  private void openFileForOutput(Settings.Path path, Consumer<PrintStream> consumer) {
    if (path.get() == null) {
      return;
    }

    try {
      PrintStream stream = new PrintStream(path.get().toFile());
      consumer.accept(stream);
      stream.close();
    } catch (FileNotFoundException e) {
      System.err.println("Could not open " + path.get() + " for output: " + e.getMessage());
    }
  }

  private static ASTProgram parse(Path path) throws Exception {
    HashSet<String> included = new HashSet<>();
    included.add(path.toAbsolutePath().normalize().toString());
    return parse(path, included);
  }

  private static ASTProgram parse(Path path, Set<String> included) throws Exception {
    FileInputStream stream = new FileInputStream(path.toFile());
    return parse(stream, path.getParent(), included);
  }

  private static ASTProgram parse(InputStream stream, Path includeDir) throws Exception {
    return parse(stream, includeDir, new HashSet<>());
  }

  private static ASTProgram parse(InputStream stream, Path includeDir, Set<String> included)
      throws Exception {
    ASTProgramWithIncludes astWithIncs = new CLLSj(stream).Program();
    stream.close();
    if (astWithIncs == null) {
      return null;
    }

    // Parse each of the includes.
    List<ASTDList> dLists = new ArrayList<>();
    List<ASTPList> pLists = new ArrayList<>();

    for (ASTInclude inc : astWithIncs.getIncs()) {
      Path incPath = includeDir.resolve(inc.getFn());
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
