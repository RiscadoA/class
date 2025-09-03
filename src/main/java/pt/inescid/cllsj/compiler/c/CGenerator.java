package pt.inescid.cllsj.compiler.c;

import java.io.PrintStream;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import pt.inescid.cllsj.compiler.Compiler;
import pt.inescid.cllsj.compiler.ir.expression.IRExpression;
import pt.inescid.cllsj.compiler.ir.id.IRCodeLocation;
import pt.inescid.cllsj.compiler.ir.id.IRDataLocation;
import pt.inescid.cllsj.compiler.ir.id.IRLocalDataId;
import pt.inescid.cllsj.compiler.ir.id.IRProcessId;
import pt.inescid.cllsj.compiler.ir.id.IRSessionId;
import pt.inescid.cllsj.compiler.ir.id.IRTypeId;
import pt.inescid.cllsj.compiler.ir.instruction.*;
import pt.inescid.cllsj.compiler.ir.slot.IRBoolS;
import pt.inescid.cllsj.compiler.ir.slot.IRIntS;
import pt.inescid.cllsj.compiler.ir.slot.IRSlot;
import pt.inescid.cllsj.compiler.ir.slot.IRSlotCombinations;
import pt.inescid.cllsj.compiler.ir.slot.IRSlotOffset;
import pt.inescid.cllsj.compiler.ir.slot.IRSlotSequence;
import pt.inescid.cllsj.compiler.ir.slot.IRSlotTree;
import pt.inescid.cllsj.compiler.ir.slot.IRStringS;

public class CGenerator extends IRInstructionVisitor {
  // Auxiliary registers used in the execution of a single instruction
  private static final String TMP_PTR1 = "tmp_ptr1";
  private static final String TMP_PTR2 = "tmp_ptr2";
  private static final String TMP_INT = "tmp_int";

  // Main register which hold the execution state across many instructions
  private static final String TASK = "task";
  private static final String ENV = "env";

  // Useful constants
  private static final String NULL = "NULL";

  private Compiler compiler;
  private IRProgram program;
  private PrintStream output;

  private IRProcess currentProcess;
  private CProcessLayout currentProcessLayout;
  private int indentLevel = 0;

  public static void generate(Compiler compiler, IRProgram program, PrintStream output) {
    CGenerator gen = new CGenerator();
    gen.compiler = compiler;
    gen.program = program;
    gen.output = output;
    gen.generate();
  }

  private void generate() {
    // Add the necessary includes.
    putLine("#define _POSIX_C_SOURCE 199309L");
    putLine("#include <stdio.h>");
    putLine("#include <stdlib.h>");
    putLine("#include <string.h>");
    putLine("#include <stdint.h>");
    putLine("#include <time.h>");
    if (compiler.concurrency.get()) {
      putLine("#include <pthread.h>");
      putLine("#include <stdatomic.h>");
    }
    putBlankLine();

    // Initialize the threading and profiling variables.
    String counterType = compiler.concurrency.get() ? "atomic_ulong" : "unsigned long";
    if (compiler.concurrency.get()) {
      putStatement("pthread_cond_t thread_stops_cond_var");
      putStatement("pthread_mutex_t thread_stops_mutex");
      putStatement(counterType + " thread_inits = 1");
      putStatement(counterType + " thread_stops = 0");
      putBlankLine();
    }
    if (compiler.profiling.get()) {
      putStatement(counterType + " env_allocs = 0");
      putStatement(counterType + " env_frees = 0");
      putStatement(counterType + " env_current = 0");
      putStatement(counterType + " env_peak = 0");
      putStatement(counterType + " record_allocs = 0");
      putStatement(counterType + " record_reallocs = 0");
      putStatement(counterType + " record_frees = 0");
      putStatement(counterType + " record_current = 0");
      putStatement(counterType + " record_peak = 0");
      putStatement(counterType + " exponential_allocs = 0");
      putStatement(counterType + " exponential_frees = 0");
      putStatement(counterType + " task_allocs = 0");
      putStatement(counterType + " task_frees = 0");
      putStatement(counterType + " string_allocs = 0");
      putStatement(counterType + " string_frees = 0");
      putStatement(counterType + " cell_allocs = 0");
      putStatement(counterType + " cell_frees = 0");
      putBlankLine();
    }

    // Define the allocation data structures used for custom memory allocation.
    if (compiler.allocatorLevels.get() > 0) {
      putStruct(
          "allocation",
          () -> {
            putStatement("int level");
            putStatement("struct allocation* next");
            putStatement("char data[]");
          });

      putStatement("struct allocation* allocator_list[" + compiler.allocatorLevels.get() + "]");
      if (compiler.concurrency.get()) {
        putStatement("pthread_mutex_t allocator_mutex[" + compiler.allocatorLevels.get() + "]");
      }
      putBlankLine();
    }

    // Define the allocation functions.
    if (compiler.allocatorLevels.get() <= 0) {
      putLine("#define managed_alloc(size) malloc(size)");
      putLine("#define managed_free(ptr) free(ptr)");
      putLine("#define managed_realloc(ptr, size) realloc(ptr, size)");
    } else {
      putBlock(
          "void* managed_alloc(size_t size)",
          () -> {
            // Compute the level of the allocator to use based on the size.
            putStatement(
                "int level = (size + "
                    + compiler.allocatorSizeDivisor.get()
                    + " - 1) / "
                    + compiler.allocatorSizeDivisor.get());
            putIfElse(
                "level >= " + compiler.allocatorLevels.get(),
                () -> {
                  putStatement(
                      "struct allocation* alloc = malloc(sizeof(struct allocation) + size)");
                  putStatement("alloc->level = " + (compiler.allocatorLevels.get() - 1));
                  putStatement("return alloc->data");
                },
                () -> {
                  if (compiler.concurrency.get()) {
                    putMutexLock("allocator_mutex[level]");
                  }
                  putIfElse(
                      "allocator_list[level] == NULL",
                      () -> {
                        if (compiler.concurrency.get()) {
                          putMutexUnlock("allocator_mutex[level]");
                        }
                        putStatement(
                            "struct allocation* alloc = malloc(sizeof(struct allocation) + (level + 1) * "
                                + compiler.allocatorSizeDivisor.get()
                                + ")");
                        putStatement("alloc->level = level");
                        putStatement("return alloc->data");
                      },
                      () -> {
                        putStatement("struct allocation* alloc = allocator_list[level]");
                        putStatement("allocator_list[level] = alloc->next");
                        if (compiler.concurrency.get()) {
                          putMutexUnlock("allocator_mutex[level]");
                        }
                        putStatement("return alloc->data");
                      });
                });
          });
      putBlankLine();

      putBlock(
          "void managed_free(void* ptr)",
          () -> {
            putStatement(
                "struct allocation* alloc = (struct allocation*)((char*)ptr - "
                    + "sizeof(struct allocation))");
            putStatement("int level = alloc->level");
            if (compiler.concurrency.get()) {
              putMutexLock("allocator_mutex[level]");
            }
            putStatement("alloc->next = allocator_list[level]");
            putStatement("allocator_list[level] = alloc");
            if (compiler.concurrency.get()) {
              putMutexUnlock("allocator_mutex[level]");
            }
          });
      putBlankLine();

      putBlock(
          "void* managed_realloc(void* ptr, size_t size)",
          () -> {
            putIf(
                "ptr == NULL",
                () -> {
                  putStatement("return managed_alloc(size)");
                });
            putStatement(
                "struct allocation* alloc = (struct allocation*)((char*)ptr - "
                    + "sizeof(struct allocation))");
            putStatement("int level = alloc->level");
            putIfElse(
                "size <= " + compiler.allocatorSizeDivisor.get() + " * (level + 1)",
                () -> {
                  putStatement("return alloc->data");
                },
                () -> {
                  putStatement("void* new_ptr = managed_alloc(size)");
                  putStatement(
                      "memcpy(new_ptr, alloc->data, "
                          + compiler.allocatorSizeDivisor.get()
                          + " * (level + 1))");
                  putStatement("managed_free(alloc->data)");
                  putStatement("return new_ptr");
                });
          });
    }
    putBlankLine();

    // Define types used during execution
    putStruct(
        "type",
        () -> {
          putStatement("int size");
          putStatement("int first_alignment;");
        });

    putStruct(
        "session",
        () -> {
          putStatement("void* cont");
          putStatement("char* cont_env");
          putStatement("char* cont_data");
          putStatement("char* cont_session");
        });

    putStruct(
        "exponential",
        () -> {
          if (compiler.concurrency.get()) {
            putStatement("atomic_int ref_count");
          } else {
            putStatement("int ref_count");
          }
        });

    putStruct(
        "task",
        () -> {
          putStatement("struct task* next");
          putStatement("void* cont");
          putStatement("char* cont_env");
        });

    // Utility macros to for handling alignment and padding
    putLine("#define ALIGN(offset, alignment) ((offset) + ((alignment) - 1) & -(alignment))");
    putLine("#define PADDING(offset, alignment) (-(offset) & ((alignment) - 1))");
    putBlankLine();

    // Utility macro for getting the maximum value of two values.
    putLine("#define MAX(a, b) ((a) > (b) ? (a) : (b))");
    putBlankLine();

    // Functions used for operations on string expressions.
    putBlock(
        "char* string_create(const char* str)",
        () -> {
          putAlloc("char* clone", CSize.expression("strlen(str) + 1"));
          putStatement("strcpy(clone, str)");
          if (compiler.profiling.get()) {
            putIncrementAtomic("string_allocs");
          }
          putReturn("clone");
        });
    putBlankLine();

    putBlock(
        "void string_drop(char* str)",
        () -> {
          putFree("str");
          if (compiler.profiling.get()) {
            putIncrementAtomic("string_frees");
          }
        });
    putBlankLine();

    putBlock(
        "char* string_concat(char* str1, char* str2)",
        () -> {
          putAlloc("char* concat", CSize.expression("strlen(str1) + strlen(str2) + 1"));
          putStatement("strcpy(concat, str1)");
          putStatement("strcat(concat, str2)");
          putFree("str1");
          putFree("str2");
          if (compiler.profiling.get()) {
            putIncrementAtomic("string_allocs");
            putIncrementAtomic("string_frees");
            putIncrementAtomic("string_frees");
          }
          putReturn("concat");
        });
    putBlankLine();

    putBlock(
        "void string_print(const char* fmt, char* str)",
        () -> {
          putStatement("printf(fmt, str)");
          putFree("str");
          if (compiler.profiling.get()) {
            putIncrementAtomic("string_frees");
          }
        });
    putBlankLine();

    putBlock(
        "char* string_from_int(int value)",
        () -> {
          putAlloc("char* str", CSize.constant(12));
          putStatement("sprintf(str, \"%d\", value)");
          if (compiler.profiling.get()) {
            putIncrementAtomic("string_allocs");
          }
          putReturn("str");
        });
    putBlankLine();

    putBlock(
        "int string_equal(char* str1, char* str2)",
        () -> {
          putAssign("int result", "strcmp(str1, str2) == 0");
          putFree("str1");
          putFree("str2");
          if (compiler.profiling.get()) {
            putIncrementAtomic("string_frees");
            putIncrementAtomic("string_frees");
          }
          putReturn("result");
        });
    putBlankLine();

    // Functions used for reading primitives from the standard input.
    putBlock(
        "int int_scan()",
        () -> {
          putStatement("int value");
          putIf(
              "scanf(\"%d\", &value) == 1",
              () -> {
                putReturn("value");
              });
          putReturn("0");
        });
    putBlankLine();

    putBlock(
        "int bool_scan()",
        () -> {
          putStatement("char buffer[6]");
          putIf(
              "scanf(\"%5s\", buffer) == 1",
              () -> {
                putReturn("strcmp(buffer, \"true\") == 0");
              });
          putReturn("0");
        });
    putBlankLine();

    putBlock(
        "char* string_scan()",
        () -> {
          putStatement("char buffer[256]");
          putStatement("char c");
          putStatement("int i = 0");
          putWhile(
              "(c = getchar()) != \'\\n\' && c != EOF",
              () -> {
                putIf(
                    "i < sizeof(buffer) - 1",
                    () -> {
                      putStatement("buffer[i++] = c");
                    });
              });
          putStatement("buffer[i] = '\\0'");
          putReturn("string_create(buffer)");
        });
    putBlankLine();

    // Utility function for sleeping a given number of milliseconds.
    putBlock(
        "void sleep_msecs(int msecs)",
        () -> {
          putStatement("struct timespec ts");
          putStatement("ts.tv_sec = msecs / 1000");
          putStatement("ts.tv_nsec = (msecs % 1000) * 1000000");
          putStatement("nanosleep(&ts, NULL)");
        });
    putBlankLine();

    // Utility function for atomically setting an integer to the maximum of its current value and a
    // given value.
    if (compiler.concurrency.get()) {
      putBlock(
          "void atomic_store_max(atomic_ulong* value, unsigned long new_value)",
          () -> {
            putAssign("unsigned long old_value", "atomic_load(value)");
            putWhile(
                "new_value > old_value",
                () -> {
                  putIf(
                      "atomic_compare_exchange_weak(value, &old_value, new_value)",
                      () -> {
                        putStatement("break");
                      });
                });
          });
      putBlankLine();
    }

    putLine("void* thread(void* entry);");
    putBlankLine();

    // Contains the actual process code and the registers necessary to execute it
    putBlock(
        "void executor(struct task* entry)",
        () -> {
          // Declare the registers.
          putStatement("register struct task* " + TASK);
          putStatement("register char* " + ENV);
          putStatement("register void* " + TMP_PTR1);
          putStatement("register void* " + TMP_PTR2);
          putStatement("register int " + TMP_INT);
          putBlankLine();

          // Initialize the task list.
          putAllocTask(TASK);
          putAssign(taskCont(TASK), labelAddress("end"));
          putBlankLine();

          // Jump to the entry process.
          IRProcess entryProcess = program.get(new IRProcessId(compiler.entryProcess.get()));
          if (entryProcess == null) {
            throw new IllegalArgumentException(
                "Entry process " + compiler.entryProcess.get() + " not found");
          }
          generate(new IRCallProcess(entryProcess.getId(), List.of(), List.of(), List.of(), false));
          putBlankLine();

          // Generate all processes
          program.stream()
              .forEach(
                  p -> {
                    generate(p);
                    putBlankLine();
                  });

          // Generate the end label, where the thread jumps to when it finishes execution
          // If concurrency is enabled, we must notify the main thread that we are stopping
          putLabel("end");
          if (compiler.concurrency.get()) {
            putMutexLock("thread_stops_mutex");
            putIncrementAtomic("thread_stops");
            putCondVarSignal("thread_stops_cond_var");
            putMutexUnlock("thread_stops_mutex");
          }
        });
    putBlankLine();

    // Function which is called by new threads (and the main function)
    putBlock(
        "void* thread(void* entry)",
        () -> {
          putStatement("executor((struct task*)entry)");
          putReturn(NULL);
        });
    putBlankLine();

    // Generate the actual main function
    putBlock(
        "int main()",
        () -> {
          // Start by validating the architecture
          for (CArchitecture.Test test : compiler.arch.getTests()) {
            String actual = CSize.sizeOf(test.cType).toString();
            putIf(
                actual + " != " + test.expected,
                () -> {
                  putDebugLn(
                      "Program was compiled for an architecture where "
                          + actual
                          + " is "
                          + test.expected
                          + ", but instead got %ld",
                      actual);
                  putReturn("1");
                });
          }
          putBlankLine();

          // Initialize allocator, if it is enabled
          if (compiler.allocatorLevels.get() > 0) {
            putFor(
                "i",
                0,
                compiler.allocatorLevels.get(),
                () -> {
                  putAssign("allocator_list[i]", "NULL");
                  if (compiler.concurrency.get()) {
                    putMutexInit("allocator_mutex[i]");
                  }
                });
            putBlankLine();
          }

          // Initialize stop detection condvar/mutex if concurrency is enabled
          if (compiler.concurrency.get()) {
            putCondVarInit("thread_stops_cond_var");
            putMutexInit("thread_stops_mutex");
            putBlankLine();
          }

          // Run main thread
          putStatement("thread(NULL)");

          // Wait for all threads to finish, if concurrency is enabled
          if (compiler.concurrency.get()) {
            putMutexLock("thread_stops_mutex");
            putWhile(
                "thread_stops != thread_inits",
                () -> {
                  putCondVarWait("thread_stops_cond_var", "thread_stops_mutex");
                });
            putMutexUnlock("thread_stops_mutex");
            putMutexDestroy("thread_stops_mutex");
            putCondVarDestroy("thread_stops_cond_var");
            putFor(
                "i",
                0,
                compiler.allocatorLevels.get(),
                () -> {
                  putMutexDestroy("allocator_mutex[i]");
                });
            putBlankLine();
          }

          // Print profiling results
          if (compiler.profiling.get()) {
            putDebugLn("Profiling results:");
            if (compiler.concurrency.get()) {
              putDebugLn("  Thread inits: %ld", "thread_inits");
              putDebugLn("  Thread stops: %ld", "thread_stops");
            }
            putDebugLn("  Environment allocations: %ld", "env_allocs");
            putDebugLn("  Environment frees: %ld", "env_frees");
            putDebugLn("  Environment peak: %ld", "env_peak");
            putDebugLn("  Record allocations: %ld", "record_allocs");
            putDebugLn("  Record reallocations: %ld", "record_reallocs");
            putDebugLn("  Record frees: %ld", "record_frees");
            putDebugLn("  Record peak: %ld", "record_peak");
            putDebugLn("  Exponential allocations: %ld", "exponential_allocs");
            putDebugLn("  Exponential frees: %ld", "exponential_frees");
            putDebugLn("  Task allocations: %ld", "task_allocs");
            putDebugLn("  Task frees: %ld", "task_frees");
            putDebugLn("  String allocations: %ld", "string_allocs");
            putDebugLn("  String frees: %ld", "string_frees");
            putDebugLn("  Cell allocations: %ld", "cell_allocs");
            putDebugLn("  Cell frees: %ld", "cell_frees");
            putIf(
                "env_allocs != env_frees",
                () -> {
                  putDebugLn("Environment leak detected!");
                  putReturn("1");
                });
            putIf(
                "record_allocs != record_frees",
                () -> {
                  putDebugLn("Record leak detected!");
                  putReturn("1");
                });
            putIf(
                "exponential_allocs != exponential_frees",
                () -> {
                  putDebugLn("Exponential leak detected!");
                  putReturn("1");
                });
            putIf(
                "task_allocs != task_frees",
                () -> {
                  putDebugLn("Task leak detected!");
                  putReturn("1");
                });
            putIf(
                "string_allocs != string_frees",
                () -> {
                  putDebugLn("String leak detected!");
                  putReturn("1");
                });
            putIf(
                "cell_allocs != cell_frees",
                () -> {
                  putDebugLn("Cell leak detected!");
                  putReturn("1");
                });
          }

          putReturn("0");
        });
  }

  private void generate(IRProcess process) {
    this.currentProcess = process;
    this.currentProcessLayout =
        CProcessLayout.compute(
            compiler,
            process,
            id -> {
              throw new UnsupportedOperationException("Polymorphic types not supported yet");
            });
    process.streamBlocks().forEach(block -> generate(block));
  }

  private void generate(IRBlock block) {
    putLabel(codeLocationLabel(block.getLocation()));
    block.stream().forEach(i -> generate(i));
  }

  private void generate(IRInstruction instr) {
    if (compiler.tracing.get()) {
      putDebugLn(instr.toString());
    } else {
      putLine("/* " + instr.toString() + " */");
    }
    instr.accept(this);
  }

  // ============================ Instruction generation visit methods ============================

  @Override
  public void visit(IRPushTask instr) {
    putAssign(TMP_PTR1, TASK);
    putAllocTask(TASK);
    putAssign(taskNext(TASK), cast(TMP_PTR1, "struct task*"));
    putAssign(taskCont(TASK), codeLocationAddress(instr.getLocation()));
    putAssign(taskContEnv(TASK), ENV);
  }

  @Override
  public void visit(IRPopTask instr) {
    putDecrementEndPoints(instr.isEndPoint());
    putAssign(TMP_PTR1, TASK);
    putAssign(TMP_PTR2, taskCont(TASK));
    putAssign(ENV, taskContEnv(TASK));
    putAssign(TASK, taskNext(TASK));
    putFreeTask(TMP_PTR1);
    putComputedGoto(TMP_PTR2);
  }

  @Override
  public void visit(IRInitializeSession instr) {
    String session = sessionAddress(instr.getSessionId());
    String contData = data(instr.getContinuationData());

    StringBuilder sb = new StringBuilder("(struct session)");
    sb.append("{.cont=").append(codeLocationAddress(instr.getContinuation()));
    sb.append(",.cont_env=").append(ENV);
    sb.append(",.cont_data=").append(contData);
    sb.append(",.cont_session=").append(session);
    sb.append("}");
    putAssign(accessSession(session), sb.toString());
  }

  @Override
  public void visit(IRContinueSession instr) {
    String session = accessSession(instr.getSessionId());

    String remoteSession = accessRemoteSession(instr.getSessionId());
    String remoteSessionCont = sessionCont(remoteSession);

    putAssign(TMP_PTR1, sessionCont(session));
    putAssign(remoteSessionCont, codeLocationAddress(instr.getContinuation()));
    putAssign(ENV, sessionContEnv(session));
    putComputedGoto(TMP_PTR1);
  }

  @Override
  public void visit(IRFinishSession instr) {
    String session = accessSession(instr.getSessionId());

    putAssign(TMP_PTR1, sessionCont(session));
    if (instr.isEndPoint()) {
      putAssign(TMP_PTR2, sessionContEnv(session));
      putDecrementEndPoints(true);
      putAssign(ENV, cast(TMP_PTR2, "char*"));
    } else {
      putAssign(ENV, sessionContEnv(session));
    }
    putComputedGoto(TMP_PTR1);
  }

  @Override
  public void visit(IRBindSession instr) {
    // Copy the session into this environment
    String source = data(instr.getLocation());
    String target = sessionAddress(instr.getSessionId());
    putCopy(target, source, compiler.arch.sessionSize());

    // Modify the remote session to point to our environment
    String remoteSession = accessRemoteSession(instr.getSessionId());
    putAssign(sessionContEnv(remoteSession), ENV);
    putAssign(sessionContSession(remoteSession), target);
    putAssign(sessionContSession(remoteSession), target);
    putAssign(sessionContData(remoteSession), data(instr.getContinuationData()));
  }

  @Override
  public void visit(IRWriteExpression instr) {
    String data = data(instr.getLocation());
    String ref = access(data, cType(instr.getExpression().getSlot()));
    putAssign(ref, expression(instr.getExpression()));
  }

  @Override
  public void visit(IRWriteScan instr) {
    String cType = cType(instr.getSlot());
    String cValue;
    String ref = access(data(instr.getLocation()), cType);
    
    if (instr.getSlot() instanceof IRIntS) {
      cValue = "int_scan()";
    } else if (instr.getSlot() instanceof IRBoolS) {
      cValue = "bool_scan()";
    } else if (instr.getSlot() instanceof IRStringS) {
      cValue = "string_scan()";
    } else {
      throw new IllegalArgumentException("Cannot scan slot of type " + instr.getSlot());
    }

    putAssign(ref, cValue);
  }

  @Override
  public void visit(IRWriteSession instr) {
    String ref = access(data(instr.getLocation()), "struct session");
    putAssign(ref, accessSession(instr.getSessionId()));
  }

  @Override
  public void visit(IRWriteTag instr) {
    String ref = access(data(instr.getLocation()), "unsigned char");
    putAssign(ref, instr.getTag());
  }

  @Override
  public void visit(IRMoveValue instr) {
    putCopy(instr.getLocation(), instr.getSourceLocation(), instr.getSlots());
  }

  @Override
  public void visit(IRPrint instr) {
    String value = expression(instr.getExpression());
    CPrintGenerator gen = CPrintGenerator.forValue(value, instr.getExpression().getSlot());
    if (instr.hasNewLine()) {
      gen.formatString += "\\n";
    }
    putStatement(gen.function + "(\"" + gen.formatString + "\", " + gen.argument + ")");
  }

  @Override
  public void visit(IRTieSessions instr) {
    putAssign(accessRemoteSession(instr.getLhsId()), accessSession(instr.getRhsId()));
    putAssign(accessRemoteSession(instr.getRhsId()), accessSession(instr.getLhsId()));
  }

  @Override
  public void visit(IRCallProcess instr) {
    IRProcessId calledProcessId = instr.getProcessId();
    IRProcess calledProcess = program.get(calledProcessId);
    if (calledProcess == null) {
      throw new IllegalArgumentException("Called process " + calledProcessId + " not found");
    }

    CProcessLayout calledLayout =
        CProcessLayout.compute(
            compiler,
            calledProcess,
            typeId -> {
              // We need to find the type layout for given type identifier in the called process
              // This type was passed as an argument, so we search for it
              IRCallProcess.TypeArgument arg =
                  instr.getTypeArguments().stream()
                      .filter(a -> a.getTargetType().equals(typeId))
                      .findFirst()
                      .orElseThrow(
                          () ->
                              new IllegalArgumentException(
                                  "Type argument "
                                      + typeId
                                      + " not found in call to process "
                                      + calledProcessId));

              // Now we can just compute the layout of the source type in the current process
              return layout(arg.getSourceTree().combinations());
            });

    // Allocate a new environment for the called process
    putAllocEnvironment(TMP_PTR1, calledLayout);
    String newEnv = cast(TMP_PTR1, "char*");

    // Setup the end points for the new environment
    if (!compiler.optimizeSingleEndpoint.get() || calledProcess.getEndPoints() != 1) {
      putAssign(endPoints(calledLayout, newEnv), calledProcess.getEndPoints());
    }

    for (IRCallProcess.TypeArgument arg : instr.getTypeArguments()) {
      // Get a reference to the target type in the new environment
      String targetType = type(calledLayout, newEnv, arg.getTargetType());

      // Compute the layout of the source type in the current environment
      // Notice that we use only the first slot to determine alignment
      // This is because that is the alignment we need to align offsets to
      CSize size = layout(arg.getSourceTree().combinations()).size;
      CAlignment firstAlignment = CAlignment.one();
      if (arg.getSourceTree().slot().isPresent()) {
        firstAlignment = layout(arg.getSourceTree().slot().get()).alignment;
      }

      // Store the type information
      StringBuilder sb = new StringBuilder("(struct type)");
      sb.append("{.size=").append(size);
      sb.append(",.first_alignment=").append(firstAlignment);
      sb.append("}");
      putAssign(targetType, sb.toString());
    }

    for (IRCallProcess.SessionArgument arg : instr.getSessionArguments()) {
      // Get references to the source and target sessions
      String source = accessSession(arg.getSourceSessionId());
      String target = accessSession(calledLayout, newEnv, arg.getTargetSessionId());

      // Copy the session data
      putAssign(target, source);

      // We must also compute the new continuation data address for the target session
      // This is necessary as the caller may apply an offset to the data
      putAssign(sessionContData(target), offset(sessionContData(target), arg.getDataOffset()));

      // Update the remote session's continuation to match the new environment
      Optional<IRLocalDataId> calledLocalDataId =
          calledProcess.getArgSessionLocalDataId(arg.getTargetSessionId());
      String remoteSession = accessRemoteSession(arg.getSourceSessionId());
      putAssign(sessionContEnv(remoteSession), newEnv);
      if (calledLocalDataId.isPresent()) {
        putAssign(
          sessionContData(remoteSession),
          localData(calledLayout, newEnv, calledLocalDataId.get(), arg.getDataOffset()));
      }
      putAssign(
          sessionContSession(remoteSession),
          sessionAddress(calledLayout, newEnv, arg.getTargetSessionId()));
    }

    for (IRCallProcess.DataArgument arg : instr.getDataArguments()) {
      // Here we simply copy data from some location in the current environment to
      // a local data section in the new environment
      IRDataLocation target = IRDataLocation.local(arg.getTargetDataId(), IRSlotOffset.ZERO);
      putCopy(calledLayout, newEnv, target, arg.getSourceLocation(), arg.getSlots());
    }

    // Decrement the end points of the current process, if applicable
    putDecrementEndPoints(instr.isEndPoint());

    putAssign(ENV, cast(TMP_PTR1, "char*"));
    putConstantGoto(codeLocationLabel(calledProcess, IRCodeLocation.entry()));
  }

  @Override
  public void visit(IRWriteExponentialFromProcess instr) {
    // TODO Auto-generated method stub
    throw new UnsupportedOperationException("Unimplemented method 'visit'");
  }

  @Override
  public void visit(IRCallExponential instr) {
    // TODO Auto-generated method stub
    throw new UnsupportedOperationException("Unimplemented method 'visit'");
  }

  @Override
  public void visit(IRIncreaseExponentialReferences instr) {
    // TODO Auto-generated method stub
    throw new UnsupportedOperationException("Unimplemented method 'visit'");
  }

  @Override
  public void visit(IRDecreaseExponentialReferences instr) {
    // TODO Auto-generated method stub
    throw new UnsupportedOperationException("Unimplemented method 'visit'");
  }

  @Override
  public void visit(IRBranchTag instr) {
    List<Runnable> cases = new ArrayList<>();
    for (IRBranchTag.Case c : instr.getCases()) {
      cases.add(
          () -> {
            putSubtractAtomic(endPoints(), instr.getMaxEndPoints() - c.getEndPoints());
            putConstantGoto(codeLocationLabel(c.getLocation()));
          });
    }

    putSwitch(access(data(instr.getLocation()), "unsigned char"), cases);
  }

  @Override
  public void visit(IRBranchExpression instr) {
    putIfElse(
        expression(instr.getExpression()),
        () -> {
          putSubtractAtomic(endPoints(), instr.getMaxEndPoints() - instr.getThen().getEndPoints());
          putConstantGoto(codeLocationLabel(instr.getThen().getLocation()));
        },
        () -> {
          putSubtractAtomic(
              endPoints(), instr.getMaxEndPoints() - instr.getOtherwise().getEndPoints());
          putConstantGoto(codeLocationLabel(instr.getOtherwise().getLocation()));
        });
  }

  // ============================ Structure expression building helpers ===========================

  private String expression(IRExpression expr) {
    return CExpressionGenerator.generate(
        expr,
        read -> {
          String ptr = data(currentProcessLayout, ENV, read.getLocation());
          return access(ptr, cType(read.getSlot()));
        });
  }

  private String endPoints() {
    return endPoints(currentProcessLayout, ENV);
  }

  private String endPoints(CProcessLayout layout, String env) {
    String ptr = layout.endPointsOffset().advancePointer(env);
    return access(ptr, compiler.concurrency.get() ? "atomic_int" : "int");
  }

  private CLayout typeLayout(IRTypeId typeId) {
    return typeLayout(currentProcessLayout, ENV, typeId);
  }

  private CLayout typeLayout(CProcessLayout layout, String env, IRTypeId typeId) {
    String type = type(layout, env, typeId);
    return new CLayout(
        CSize.expression(typeSize(type)), CAlignment.expression(typeAlignment(type)));
  }

  private String type(CProcessLayout layout, String env, IRTypeId id) {
    CSize offset = layout.typeOffset(id);
    return access(offset.advancePointer(env), "struct type");
  }

  private CSize sessionOffset(IRSessionId id) {
    return sessionOffset(currentProcessLayout, id);
  }

  private CSize sessionOffset(CProcessLayout layout, IRSessionId id) {
    return layout.sessionOffset(id);
  }

  private String accessSession(IRSessionId id) {
    return accessSession(sessionAddress(id));
  }

  private String accessSession(CProcessLayout layout, String env, IRSessionId id) {
    return accessSession(sessionAddress(layout, env, id));
  }

  private String accessSession(String address) {
    return access(address, "struct session");
  }

  private String sessionAddress(IRSessionId id) {
    return sessionAddress(currentProcessLayout, ENV, id);
  }

  private String sessionAddress(CProcessLayout layout, String env, IRSessionId id) {
    return sessionOffset(layout, id).advancePointer(env);
  }

  private String remoteSessionAddress(IRSessionId id) {
    return remoteSessionAddress(accessSession(id));
  }

  private String remoteSessionAddress(String session) {
    return sessionContSession(session);
  }

  private String accessRemoteSession(IRSessionId id) {
    return accessSession(remoteSessionAddress(id));
  }

  private String localData(
      CProcessLayout layout, String env, IRLocalDataId localDataId, IRSlotOffset offset) {
    CSize finalOffset = offset(layout.dataOffset(localDataId), offset);
    return finalOffset.advancePointer(env);
  }

  private String remoteData(IRSessionId sessionId, IRSlotOffset offset) {
    return remoteData(currentProcessLayout, ENV, sessionId, offset);
  }

  private String remoteData(
      CProcessLayout layout, String env, IRSessionId sessionId, IRSlotOffset offset) {
    return offset(sessionContData(accessSession(sessionId)), offset);
  }

  private String data(IRDataLocation data) {
    return data(currentProcessLayout, ENV, data);
  }

  private String data(CProcessLayout layout, String env, IRDataLocation data) {
    if (data.isRemote()) {
      return remoteData(layout, env, data.getSessionId(), data.getOffset());
    } else {
      return localData(layout, env, data.getLocalDataId(), data.getOffset());
    }
  }

  private String typeSize(String var) {
    return var + ".size";
  }

  private String typeAlignment(String var) {
    return var + ".alignment";
  }

  private String taskNext(String var) {
    return var + "->next";
  }

  private String taskCont(String var) {
    return var + "->cont";
  }

  private String taskContEnv(String var) {
    return var + "->cont_env";
  }

  private String sessionCont(String var) {
    return var + ".cont";
  }

  private String sessionContEnv(String var) {
    return var + ".cont_env";
  }

  private String sessionContData(String var) {
    return var + ".cont_data";
  }

  private String sessionContSession(String var) {
    return var + ".cont_session";
  }

  private CSize offset(CSize base, IRSlotOffset offset) {
    if (offset.isZero()) {
      return base;
    } else {
      CSize pastSize = layout(offset.getPast()).size;
      if (offset.getAlignTo().isPresent()) {
        CAlignment alignment = layout(offset.getAlignTo().get()).alignment;
        return base.add(pastSize).align(alignment);
      } else {
        return base.add(pastSize);
      }
    }
  }

  private String offset(String address, IRSlotOffset offset) {
    return offset(CSize.zero(), offset).advancePointer(address);
  }

  private CLayout layout(IRSlotCombinations combinations) {
    return CLayout.compute(combinations, compiler.arch, typeId -> typeLayout(typeId));
  }

  private CLayout layout(IRSlotSequence sequence) {
    return CLayout.compute(sequence, compiler.arch, typeId -> typeLayout(typeId));
  }

  private CLayout layout(IRSlot slot) {
    return CLayout.compute(slot, compiler.arch, typeId -> typeLayout(typeId));
  }

  private String cType(IRSlot slot) {
    if (slot instanceof IRIntS) {
      return "int";
    } else if (slot instanceof IRBoolS) {
      return "unsigned char";
    } else if (slot instanceof IRStringS) {
      return "char*";
    } else {
      throw new UnsupportedOperationException("Other slot types not supported yet");
    }
  }

  // ============================ Structure statement building helpers ============================

  private void putCopy(IRDataLocation target, IRDataLocation source, IRSlotTree slots) {
    putCopy(currentProcessLayout, ENV, target, source, slots);
  }

  private void putCopy(
      CProcessLayout targetLayout,
      String targetEnv,
      IRDataLocation target,
      IRDataLocation source,
      IRSlotTree slots) {
    putCopy(targetLayout, targetEnv, target, currentProcessLayout, ENV, source, slots);
  }

  private void putCopy(
      CProcessLayout targetLayout,
      String targetEnv,
      IRDataLocation target,
      CProcessLayout sourceLayout,
      String sourceEnv,
      IRDataLocation source,
      IRSlotTree slots) {
    putCopy(
        targetLayout,
        targetEnv,
        target,
        sourceLayout,
        sourceEnv,
        source,
        slots,
        IRSlotSequence.EMPTY);
  }

  private void putCopy(
      CProcessLayout targetLayout,
      String targetEnv,
      IRDataLocation target,
      CProcessLayout sourceLayout,
      String sourceEnv,
      IRDataLocation source,
      IRSlotTree slots,
      IRSlotSequence past) {
    // The difficulty here lies in that the source and target may have different layouts.
    // This is due to the fact that they might start in offsets with different alignments,
    // and thus, have different paddings between slots.
    //
    // Additionally, there may be branching in the slots we're copying (e.g., a choice type)
    // which means that we might also need to copy different slots depending on the value of a tag.
    //
    // The simple way to solve this, which is what we do here, is to copy slot by slot.

    if (slots.slot().isPresent()) {
      putCopy(
          data(targetLayout, targetEnv, target.advance(past, slots.slot().get())),
          data(sourceLayout, sourceEnv, source.advance(past, slots.slot().get())),
          layout(slots.slot().get()).size);
    }

    if (slots.isUnary()) {
      putCopy(
          targetLayout,
          targetEnv,
          target,
          sourceLayout,
          sourceEnv,
          source,
          ((IRSlotTree.Unary) slots).child(),
          past.suffix(slots.slot().get()));
    } else if (slots.isTag()) {
      IRSlotTree.Tag tag = (IRSlotTree.Tag) slots;
      IRDataLocation sourceTagLocation = source.advance(past, tag.slot().get());
      List<Runnable> cases = new ArrayList<>();

      for (IRSlotTree child : tag.cases()) {
        cases.add(
            () ->
                putCopy(
                    targetLayout,
                    targetEnv,
                    target,
                    sourceLayout,
                    sourceEnv,
                    source,
                    child,
                    past.suffix(slots.slot().get())));
      }

      putSwitch(access(data(sourceTagLocation), "unsigned char"), cases);
    }
  }

  private void putAllocTask(String var) {
    putAlloc(var, CSize.sizeOf("struct task"));
    if (compiler.profiling.get()) {
      putIncrementAtomic("task_allocs");
    }
  }

  private void putFreeTask(String var) {
    putFree(var);
    if (compiler.profiling.get()) {
      putIncrementAtomic("task_frees");
    }
  }

  private void putAllocEnvironment(String var, CProcessLayout layout) {
    putAlloc(var, layout.size());
    if (compiler.profiling.get()) {
      putIncrementAtomic("env_allocs");
    }
  }

  private void putFreeEnvironment(String var) {
    putFree(var);
    if (compiler.profiling.get()) {
      putIncrementAtomic("env_frees");
    }
  }

  private void putDecrementEndPoints(boolean isEndPoint) {
    putDecrementEndPoints(isEndPoint, () -> putFreeEnvironment(ENV));
  }

  private void putDecrementEndPoints(boolean isEndPoint, Runnable free) {
    if (!isEndPoint) {
      return;
    }

    if (compiler.optimizeSingleEndpoint.get() && currentProcess.getEndPoints() == 1) {
      free.run();
    } else {
      putIf(decrementAtomic(endPoints()) + " == 1", free);
    }
  }

  // =============================== Base expression building helpers =============================

  private String cast(String expr, String type) {
    return "(" + type + ")(" + expr + ")";
  }

  private String castAndDeref(String expr, String type) {
    return "(*" + cast(expr, type) + ")";
  }

  private String access(String expr, String type) {
    return castAndDeref(expr, type + "*");
  }

  private String labelAddress(String label) {
    return "&&" + label;
  }

  private String codeLocationLabel(IRProcess process, IRCodeLocation location) {
    return process.getId() + "_" + location.getLabel();
  }

  private String codeLocationLabel(IRCodeLocation location) {
    return codeLocationLabel(currentProcess, location);
  }

  private String codeLocationAddress(IRCodeLocation location) {
    return labelAddress(codeLocationLabel(location));
  }

  private String incrementAtomic(String var) {
    if (compiler.concurrency.get()) {
      return "atomic_fetch_add(&" + var + ", 1)";
    } else {
      return var + "++";
    }
  }

  private String decrementAtomic(String var) {
    if (compiler.concurrency.get()) {
      return "atomic_fetch_sub(&" + var + ", 1)";
    } else {
      return var + "--";
    }
  }

  // =============================== Base statement building helpers ==============================

  private void putAlloc(String var, CSize size) {
    putAssign(var, "managed_alloc(" + size + ")");
  }

  private void putFree(String var) {
    putStatement("managed_free(" + var + ")");
  }

  private void putMutexInit(String var) {
    putStatement("pthread_mutex_init(&(" + var + "), NULL)");
  }

  private void putMutexDestroy(String var) {
    putStatement("pthread_mutex_destroy(&(" + var + "))");
  }

  private void putMutexLock(String var) {
    putStatement("pthread_mutex_lock(&(" + var + "))");
  }

  private void putMutexUnlock(String var) {
    putStatement("pthread_mutex_unlock(&(" + var + "))");
  }

  private void putCondVarInit(String var) {
    putStatement("pthread_cond_init(&(" + var + "), NULL)");
  }

  private void putCondVarDestroy(String var) {
    putStatement("pthread_cond_destroy(&(" + var + "))");
  }

  private void putCondVarWait(String var, String mutex) {
    putStatement("pthread_cond_wait(&(" + var + "), &(" + mutex + "))");
  }

  private void putCondVarSignal(String var) {
    putStatement("pthread_cond_signal(&(" + var + "))");
  }

  private void putIncrementAtomic(String var) {
    putStatement(incrementAtomic(var));
  }

  private void putDecrementAtomic(String var) {
    putStatement(decrementAtomic(var));
  }

  private void putSubtractAtomic(String var, int value) {
    if (value != 0) {
      if (compiler.concurrency.get()) {
        putStatement("atomic_fetch_sub(&" + var + ", " + value + ")");
      } else {
        putStatement(var + " -= " + value);
      }
    }
  }

  private void putAssignMaxAtomic(String var, String value) {
    putStatement("atomic_store_max(&" + var + ", " + value + ")");
  }

  private void putDebugLn(String message, String... args) {
    putDebug(message + "\\n", args);
  }

  private void putDebug(String message, String... args) {
    message = message.replace("\"", "\\\"");
    String stringArgs = String.join(", ", args);
    putStatement(
        "fprintf(stderr, \""
            + message
            + "\""
            + (stringArgs.isEmpty() ? "" : (", " + stringArgs))
            + ")");
  }

  private void putConstantGoto(String label) {
    putStatement("goto " + label);
  }

  private void putComputedGoto(String address) {
    putStatement("goto *" + address);
  }

  private void putIf(String condition, Runnable then) {
    putBlock("if (" + condition + ")", then);
  }

  private void putIfElse(String condition, Runnable then, Runnable otherwise) {
    putLine("if (" + condition + ") {");
    putIndented(then);
    putLine("} else {");
    putIndented(otherwise);
    putLine("}");
  }

  private void putSwitch(String value, List<Runnable> cases) {
    putBlock(
        "switch (" + value + ")",
        () -> {
          for (int i = 0; i < cases.size(); ++i) {
            putLine("case " + i + ":");
            putIndented(cases.get(i));
            putStatement("break");
          }
        });
  }

  private void putWhile(String condition, Runnable body) {
    putBlock("while (" + condition + ")", body);
  }

  private void putFor(String var, int from, int to, Runnable body) {
    putBlock("for (int " + var + " = " + from + "; " + var + " < " + to + "; ++" + var + ")", body);
  }

  private void putFor(String init, String condition, String step, Runnable body) {
    putBlock("for (" + init + "; " + condition + "; " + step + ")", body);
  }

  private void putLabel(String label) {
    put(label + ":");
    putLineEnd();
  }

  private void putCopy(String dst, String src, CSize size) {
    if (!size.equals(CSize.zero())) {
      putStatement("memcpy(" + dst + ", " + src + ", " + size + ")");
    }
  }

  private void putAssign(String var, Object what) {
    putAssign(var, what.toString());
  }

  private void putAssign(String var, String what) {
    putStatement(var + " = " + what);
  }

  private void putReturn(String what) {
    putStatement("return " + what);
  }

  private void putStatement(String statement) {
    putLine(statement + ";");
  }

  private void putBlankLine() {
    putLine("");
  }

  private void putLine(String line) {
    if (!line.isEmpty()) {
      putIndent();
      put(line);
    }
    putLineEnd();
  }

  private void putStruct(String name, Runnable fields) {
    putLine("struct " + name + " {");
    putIndented(fields);
    putLine("};");
    putBlankLine();
  }

  private void putBlock(String begin, Runnable indented) {
    putLine(begin + " {");
    putIndented(indented);
    putLine("}");
  }

  private void putIndented(Runnable indented) {
    indentLevel += 1;
    indented.run();
    indentLevel -= 1;
  }

  private void putIndent() {
    put("  ".repeat(indentLevel));
  }

  private void putLineEnd() {
    put("\n");
  }

  private void put(String str) {
    output.append(str);
  }

  private void incIndent() {
    indentLevel++;
  }

  private void decIndent() {
    indentLevel--;
  }
}
