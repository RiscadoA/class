package pt.inescid.cllsj.compiler;

import pt.inescid.cllsj.compiler.c.CAlignment;
import pt.inescid.cllsj.compiler.c.CArchitecture;
import pt.inescid.cllsj.compiler.c.CSize;

public class Compiler {
  private Settings settings = new Settings();

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

  public Compiler() {
    entryProcess = settings.addName('e', "entry", "Entry process name", "main");
    tracing =
        settings.addFlag('t', "tracing", "Generates tracing prints for each IR instruction", false);
    debug =
        settings.addFlag(
            'v', "debug", "Generates very verbose prints for each IR instruction", false);
    profiling =
        settings.addFlag(
            'p',
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

    optimizeSingleEndpoint = settings.addFlag("optimize-single-endpoint", "Optimizes away reference counting for processes with a single end point", true);

    settings.addMode(
        "optimize-sequential",
        "Modifies setting defaults to maximize performance on sequential code (concurrency is disabled)",
        () -> {
          concurrency.set(false);
        });

    settings.addMode(
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

    return 0;
  }
}
