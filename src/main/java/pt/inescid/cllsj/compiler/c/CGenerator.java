package pt.inescid.cllsj.compiler.c;

import java.io.PrintStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.function.BiConsumer;
import java.util.function.BiFunction;
import java.util.function.Function;
import pt.inescid.cllsj.compiler.Compiler;
import pt.inescid.cllsj.compiler.ir.expression.IRExpression;
import pt.inescid.cllsj.compiler.ir.id.IRCodeLocation;
import pt.inescid.cllsj.compiler.ir.id.IRDataLocation;
import pt.inescid.cllsj.compiler.ir.id.IRDropId;
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
import pt.inescid.cllsj.compiler.ir.slot.IRTagS;

public class CGenerator extends IRInstructionVisitor {
  // Auxiliary registers used in the execution of a single instruction
  private static final String TMP_PTR1 = "tmp_ptr1";
  private static final String TMP_PTR2 = "tmp_ptr2";
  private static final String TMP_INT = "tmp_int";
  private static final String TMP_SESSION = "tmp_session";

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

  private int nextLabelId = 0;

  private Map<IRProcessId, IRWriteExponential> pendingExponentialManagers = new HashMap<>();

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
          putStatement("int entry_session_offset");
          putStatement("int entry_data_offset");
          putStatement("int env_size");
          putStatement("void(*manager)(char* env, int mode)");
          putStatement("char env[]");
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
        "char* string_clone(const char* str)",
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
          putReturn("string_clone(buffer)");
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
          putStatement("struct session " + TMP_SESSION);
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

    // Generate all exponential managers
    pendingExponentialManagers.entrySet().stream()
        .forEach(
            e -> {
              generateExponentialManager(e.getKey(), e.getValue());
              putBlankLine();
            });

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
    this.currentProcessLayout = layout(process);
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
      putComment(instr.toString());
    }
    instr.accept(this);
  }

  private void generateExponentialManager(IRProcessId processId, IRWriteExponential instr) {
    // The purpose of this section is described in a comment in the IRWriteExponential visit method

    // Set up the layout and the function used to get layouts from types
    CProcessLayout layout = layout(program.get(processId));
    Function<IRTypeId, CLayout> typeLayoutProvider = id -> typeLayout(layout, "exp_env", id);

    putBlock(
        "void " + managerName(processId) + "(char* exp_env, int mode)",
        () -> {
          putIfElse(
              "mode == 0",
              () -> {
                putComment("Clone mode");
                for (IRWriteExponential.DataArgument arg : instr.getDataArguments()) {
                  IRLocalDataId dataId = arg.getTargetDataId();
                  putSlotTraversal(
                      arg.getSlots(),
                      past ->
                          localData(
                              typeLayoutProvider,
                              layout,
                              "exp_env",
                              dataId,
                              IRSlotOffset.of(past, new IRTagS())),
                      (past, slot) -> {
                        CAddress target =
                            localData(
                                typeLayoutProvider,
                                layout,
                                "exp_env",
                                dataId,
                                IRSlotOffset.of(past, slot));
                        putCloneSlot(target, slot);
                      });
                }
              },
              () -> {
                putComment("Drop mode");
                for (IRWriteExponential.DataArgument arg : instr.getDataArguments()) {
                  IRLocalDataId dataId = arg.getTargetDataId();
                  putSlotTraversal(
                      arg.getSlots(),
                      past ->
                          localData(
                              typeLayoutProvider,
                              layout,
                              "exp_env",
                              dataId,
                              IRSlotOffset.of(past, new IRTagS())),
                      (past, slot) -> {
                        CAddress target =
                            localData(
                                typeLayoutProvider,
                                layout,
                                "exp_env",
                                dataId,
                                IRSlotOffset.of(past, slot));
                        putDropSlot(target, slot);
                      });
                }
              });
        });
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
    CAddress session = sessionAddress(instr.getSessionId());
    CAddress contData = data(instr.getContinuationData());

    StringBuilder sb = new StringBuilder("(struct session)");
    sb.append("{.cont=").append(codeLocationAddress(instr.getContinuation()));
    sb.append(",.cont_env=").append(ENV);
    sb.append(",.cont_data=").append(contData);
    sb.append(",.cont_session=").append(session);
    sb.append("}");
    putAssign(session.deref("struct session"), sb.toString());
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
    CAddress source = data(instr.getLocation());
    CAddress target = sessionAddress(instr.getSessionId());
    putCopyMemory(target, source, compiler.arch.sessionSize());

    // Modify the remote session to point to our environment
    String remoteSession = accessRemoteSession(instr.getSessionId());
    putAssign(sessionContEnv(remoteSession), ENV);
    putAssign(sessionContSession(remoteSession), target);
    putAssign(sessionContSession(remoteSession), target);
    putAssign(sessionContData(remoteSession), data(instr.getContinuationData()));
  }

  @Override
  public void visit(IRWriteExpression instr) {
    String ref = data(instr.getLocation()).deref(cType(instr.getExpression().getSlot()));
    putAssign(ref, expression(instr.getExpression()));
  }

  @Override
  public void visit(IRWriteScan instr) {
    String cType = cType(instr.getSlot());
    String cValue;
    String ref = data(instr.getLocation()).deref(cType);

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
    String ref = data(instr.getLocation()).deref("struct session");
    putAssign(ref, accessSession(instr.getSessionId()));
  }

  @Override
  public void visit(IRWriteTag instr) {
    String ref = data(instr.getLocation()).deref("unsigned char");
    putAssign(ref, instr.getTag());
  }

  @Override
  public void visit(IRWriteType instr) {
    String ref = data(instr.getLocation()).deref("struct type");
    putAssign(ref, typeInitializer(instr.getSlots()));
  }

  @Override
  public void visit(IRMoveValue instr) {
    putMoveSlots(instr.getSlots(), false, instr.getLocation(), instr.getSourceLocation());
  }

  @Override
  public void visit(IRCloneValue instr) {
    putMoveSlots(instr.getSlots(), true, instr.getLocation(), instr.getSourceLocation());
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
  public void visit(IRForwardSessions instr) {
    putAssign(TMP_SESSION, accessSession(instr.getNegId()));
    putAssign(accessRemoteSession(instr.getNegId()), accessSession(instr.getPosId()));
    putAssign(TMP_PTR1, sessionCont(accessSession(instr.getPosId())));
    putAssign(TMP_PTR2, sessionContEnv(accessSession(instr.getPosId())));
    putAssign(accessRemoteSession(instr.getPosId()), TMP_SESSION);

    if (instr.isEndPoint()) {
      putDecrementEndPoints(true);
    }
    putAssign(ENV, cast(TMP_PTR2, "char*"));
    putComputedGoto(TMP_PTR1);
  }

  @Override
  public void visit(IRCallProcess instr) {
    IRProcessId calledProcessId = instr.getProcessId();
    IRProcess calledProcess = program.get(calledProcessId);
    if (calledProcess == null) {
      throw new IllegalArgumentException("Called process " + calledProcessId + " not found");
    }

    Function<IRTypeId, CLayout> typeLayoutProvider =
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

          if (arg.isFromLocation()) {
            return typeLayout(data(arg.getSourceLocation()).deref("struct type"));
          } else {
            return layout(arg.getSourceTree().combinations());
          }
        };

    CProcessLayout calledLayout = layout(calledProcess);

    // Allocate a new environment for the called process
    putAllocEnvironment(TMP_PTR1, typeLayoutProvider, calledLayout);
    String newEnv = cast(TMP_PTR1, "char*");

    // Setup the end points for the new environment
    if (!compiler.optimizeSingleEndpoint.get() || calledProcess.getEndPoints() != 1) {
      putAssign(endPoints(calledLayout, newEnv), calledProcess.getEndPoints());
    }

    // Zero out any drop bits
    putZeroEnvironmentDropBits(calledLayout, newEnv);

    for (IRCallProcess.TypeArgument arg : instr.getTypeArguments()) {
      // Get a reference to the target type in the new environment and initialize it
      String targetType = type(calledLayout, newEnv, arg.getTargetType());
      if (arg.isFromLocation()) {
        putAssign(targetType, data(arg.getSourceLocation()).deref("struct type"));
      } else {
        putAssign(targetType, typeInitializer(arg.getSourceTree()));
      }
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
            localData(
                typeLayoutProvider,
                calledLayout,
                newEnv,
                calledLocalDataId.get(),
                arg.getDataOffset()));
      }
      putAssign(
          sessionContSession(remoteSession),
          sessionAddress(calledLayout, newEnv, arg.getTargetSessionId()));
    }

    for (IRCallProcess.DataArgument arg : instr.getDataArguments()) {
      // Here we simply copy data from some location in the current environment to
      // a local data section in the new environment
      IRDataLocation target = IRDataLocation.local(arg.getTargetDataId(), IRSlotOffset.ZERO);
      putMoveSlots(
          arg.getSlots(),
          arg.isClone(),
          (past, slot) ->
              data(typeLayoutProvider, calledLayout, newEnv, target.advance(past, slot)),
          (past, slot) -> data(arg.getSourceLocation().advance(past, slot)));
    }

    // Decrement the end points of the current process, if applicable
    putDecrementEndPoints(instr.isEndPoint());

    putAssign(ENV, cast(TMP_PTR1, "char*"));
    putConstantGoto(codeLocationLabel(calledProcess, IRCodeLocation.entry()));
  }

  @Override
  public void visit(IRWriteExponential instr) {
    IRProcessId expProcessId = instr.getProcessId();
    IRProcess expProcess = program.get(expProcessId);
    if (expProcess == null) {
      throw new IllegalArgumentException("Exponential process " + expProcessId + " not found");
    }

    Function<IRTypeId, CLayout> typeLayoutProvider =
        typeId -> {
          // We need to find the type layout for given type identifier in the called process
          // This type was passed as an argument, so we search for it
          IRWriteExponential.TypeArgument arg =
              instr.getTypeArguments().stream()
                  .filter(a -> a.getTargetType().equals(typeId))
                  .findFirst()
                  .orElseThrow(
                      () ->
                          new IllegalArgumentException(
                              "Type argument "
                                  + typeId
                                  + " not found in call to exponential process "
                                  + expProcessId));

          // Now we can just compute the layout of the source type in the current process
          return layout(arg.getSourceTree().combinations());
        };

    CProcessLayout expProcessLayout = layout(expProcess);

    // Data arguments passed to exponentials need to support clone and drop operations
    // When the exponential is in use, we don't have information about the data it captured
    // Thus, we must store this information in the exponential itself
    // Instead of storing the information directly, we store a code address which
    // performs the necessary operation
    //
    // This code section assumes:
    // - TMP_PTR1 holds the continuation address to jump to after the operation
    // - TMP_PTR2 holds the base address of the exponential's environment
    // - TMP_INT holds the operation to perform (0 = clone, 1 = drop)

    // Allocate a new exponential and write it to the target location
    putAllocExponential(TMP_PTR1, typeLayoutProvider, expProcessLayout);
    String exp = cast(TMP_PTR1, "struct exponential*");
    putAssign(data(instr.getLocation()).deref("struct exponential*"), exp);

    // Initialize the exponential
    pendingExponentialManagers.put(expProcessId, instr);
    putAssign(exponentialRefCount(exp), 1);
    putAssign(
        exponentialEntrySessionOffset(exp), expProcessLayout.sessionOffset(new IRSessionId(0)));
    putAssign(
        exponentialEntryDataOffset(exp),
        expProcessLayout.dataOffset(typeLayoutProvider, new IRLocalDataId(0)));
    putAssign(exponentialEnvSize(exp), expProcessLayout.size(typeLayoutProvider));
    putStatement(
        "void " + managerName(expProcessId) + "(char* env, int mode)"); // Forward declare function
    putAssign(exponentialManager(exp), managerName(expProcessId));
    putAssign(TMP_PTR1, exponentialEnv(exp));
    String newEnv = cast(TMP_PTR1, "char*");

    // Setup the end points for the new environment
    if (!compiler.optimizeSingleEndpoint.get() || expProcess.getEndPoints() != 1) {
      putAssign(endPoints(expProcessLayout, newEnv), expProcess.getEndPoints());
    }

    // Setup the continuation of the entry session
    CAddress entrySessionAddress =
        CAddress.of(newEnv, expProcessLayout.sessionOffset(new IRSessionId(0)));
    String entrySession = entrySessionAddress.deref("struct session");
    putAssign(
        sessionCont(entrySession),
        labelAddress(codeLocationLabel(expProcess, IRCodeLocation.entry())));

    // Write the type arguments to the exponential's environment
    for (IRWriteExponential.TypeArgument arg : instr.getTypeArguments()) {
      // Get a reference to the target type in the new environment and initialize itq
      String targetType = type(expProcessLayout, newEnv, arg.getTargetType());
      putAssign(targetType, typeInitializer(arg.getSourceTree()));
    }

    for (IRWriteExponential.DataArgument arg : instr.getDataArguments()) {
      // Here we simply copy data from some location in the current environment to
      // a local data section in the new environment
      IRDataLocation target = IRDataLocation.local(arg.getTargetDataId(), IRSlotOffset.ZERO);
      putMoveSlots(
          arg.getSlots(),
          arg.isClone(),
          (past, slot) ->
              data(typeLayoutProvider, expProcessLayout, newEnv, target.advance(past, slot)),
          (past, slot) -> data(arg.getSourceLocation().advance(past, slot)));
    }
  }

  @Override
  public void visit(IRCallExponential instr) {
    String exponential = data(instr.getLocation()).deref("struct exponential*");
    CAddress oldEnv = CAddress.of(exponentialEnv(exponential));

    // Allocate the new environment and initialize it by copying the one stored in the exponential
    putAllocEnvironment(TMP_PTR1, CSize.expression(exponentialEnvSize(exponential)));
    CAddress newEnv = castToAddress(TMP_PTR1);
    putCopyMemory(newEnv, oldEnv, CSize.expression(exponentialEnvSize(exponential)));

    // Call the exponential manager to clone the slots
    putStatement(
        exponentialManager(exponential) + "(" + cast(TMP_PTR1, "char*") + ", 0)"); // 0 = Clone

    // Setup the new session and tie it to the exponential's entry session
    CAddress localSessionAddress = sessionAddress(instr.getSessionId());
    CAddress remoteSessionAddress =
        newEnv.offset(CSize.expression(exponentialEntrySessionOffset(exponential)));
    String localSession = localSessionAddress.deref("struct session");
    String remoteSession = remoteSessionAddress.deref("struct session");
    CAddress remoteData = newEnv.offset(CSize.expression(exponentialEntryDataOffset(exponential)));

    // Initialize local session
    StringBuilder sb = new StringBuilder("(struct session)");
    sb.append("{.cont=").append(sessionCont(remoteSession));
    sb.append(",.cont_env=").append(newEnv);
    sb.append(",.cont_data=").append(remoteData);
    sb.append(",.cont_session=").append(remoteSessionAddress);
    sb.append("}");
    putAssign(localSession, sb.toString());

    // Initialize remote session (the one stored in the exponential)
    putAssign(sessionContEnv(remoteSession), ENV);
    putAssign(sessionContData(remoteSession), localData(instr.getLocalDataId()));
    putAssign(sessionContSession(remoteSession), localSessionAddress);
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

    putSwitch(data(instr.getLocation()).deref("unsigned char"), cases);
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

  @Override
  public void visit(IRDeferDrop instr) {
    CSizeBits dropBitOffset = currentProcessLayout.dropBitOffset(instr.getDropId());
    CAddress dropByteAddress = CAddress.of(ENV, dropBitOffset.getSize());
    String dropByte = dropByteAddress.deref("unsigned char");
    putAssign(dropByte, dropByte + " | (1 << " + dropBitOffset.getBits() + ")");
  }

  @Override
  public void visit(IRSleep instr) {
    putStatement("sleep_msecs(" + instr.getMsecs() + ")");
  }

  @Override
  public void visit(IRPanic instr) {
    putDebugLn("Panic: " + instr.getMessage());
    putStatement("exit(1);");
  }

  @Override
  public void visit(IRJump instr) {
    putConstantGoto(codeLocationLabel(instr.getLocation()));
  }

  // ============================ Structure expression building helpers ===========================

  private String expression(IRExpression expr) {
    return CExpressionGenerator.generate(
        expr,
        move -> {
          return data(move.getLocation()).deref(cType(move.getSlot()));
        },
        clone -> {
          String value = data(clone.getLocation()).deref(cType(clone.getSlot()));
          if (clone.getSlot() instanceof IRStringS) {
            value = "string_clone(" + value + ")";
          }
          return value;
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
    return typeLayout(type(layout, env, typeId));
  }

  private CLayout typeLayout(String type) {
    return new CLayout(
        CSize.expression(typeSize(type)), CAlignment.expression(typeFirstAlignment(type)));
  }

  private String typeInitializer(IRSlotTree slots) {
    CSize size = layout(slots.combinations()).size;
    CAlignment firstAlignment = slots.slot().isPresent()
        ? layout(slots.slot().get()).alignment
        : CAlignment.one();

    StringBuilder sb = new StringBuilder("(struct type)");
    sb.append("{.size=").append(size);
    sb.append(",.first_alignment=").append(firstAlignment);
    sb.append("}");
    return sb.toString();
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
    return sessionAddress(id).deref("struct session");
  }

  private String accessSession(CProcessLayout layout, String env, IRSessionId id) {
    return sessionAddress(layout, env, id).deref("struct session");
  }

  private String accessSession(String address) {
    return access(address, "struct session");
  }

  private CAddress sessionAddress(IRSessionId id) {
    return sessionAddress(currentProcessLayout, ENV, id);
  }

  private CAddress sessionAddress(CProcessLayout layout, String env, IRSessionId id) {
    return CAddress.of(env, sessionOffset(layout, id));
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

  private CAddress localData(IRLocalDataId localDataId) {
    return localData(this::typeLayout, currentProcessLayout, ENV, localDataId, IRSlotOffset.ZERO);
  }

  private CAddress localData(IRLocalDataId localDataId, IRSlotOffset offset) {
    return localData(this::typeLayout, currentProcessLayout, ENV, localDataId, offset);
  }

  private CAddress localData(
      Function<IRTypeId, CLayout> typeLayouts,
      CProcessLayout layout,
      String env,
      IRLocalDataId localDataId,
      IRSlotOffset offset) {
    CSize finalOffset = offset(layout.dataOffset(typeLayouts, localDataId), offset);
    return CAddress.of(env, finalOffset);
  }

  private CAddress remoteData(IRSessionId sessionId, IRSlotOffset offset) {
    return remoteData(currentProcessLayout, ENV, sessionId, offset);
  }

  private CAddress remoteData(
      CProcessLayout layout, String env, IRSessionId sessionId, IRSlotOffset offset) {
    return offset(sessionContData(accessSession(sessionId)), offset);
  }

  private CAddress data(IRDataLocation data) {
    return data(this::typeLayout, currentProcessLayout, ENV, data);
  }

  private CAddress data(
      Function<IRTypeId, CLayout> typeLayouts,
      CProcessLayout layout,
      String env,
      IRDataLocation data) {
    if (data.isRemote()) {
      return remoteData(layout, env, data.getSessionId(), data.getOffset());
    } else {
      return localData(typeLayouts, layout, env, data.getLocalDataId(), data.getOffset());
    }
  }

  private String typeSize(String var) {
    return var + ".size";
  }

  private String typeFirstAlignment(String var) {
    return var + ".first_alignment";
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

  private String exponentialRefCount(String var) {
    return var + "->ref_count";
  }

  private String exponentialEntrySessionOffset(String var) {
    return var + "->entry_session_offset";
  }

  private String exponentialEntryDataOffset(String var) {
    return var + "->entry_data_offset";
  }

  private String exponentialEnvSize(String var) {
    return var + "->env_size";
  }

  private String exponentialManager(String var) {
    return var + "->manager";
  }

  private String exponentialEnv(String var) {
    return var + "->env";
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

  private CAddress offset(String address, IRSlotOffset offset) {
    return CAddress.of(address, offset(CSize.zero(), offset));
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

  private CProcessLayout layout(IRProcess process) {
    return CProcessLayout.compute(compiler, process);
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

  void putMoveSlots(
      IRSlotTree slots,
      boolean clone,
      IRDataLocation targetLocation,
      IRDataLocation sourceLocation) {
    putMoveSlots(
        slots,
        clone,
        (past, slot) -> data(targetLocation.advance(past, slot)),
        (past, slot) -> data(sourceLocation.advance(past, slot)));
  }

  void putMoveSlots(
      IRSlotTree slots,
      boolean clone,
      BiFunction<IRSlotSequence, IRSlot, CAddress> targetAddress,
      BiFunction<IRSlotSequence, IRSlot, CAddress> sourceAddress) {
    putSlotTraversal(
        slots,
        past -> sourceAddress.apply(past, new IRTagS()),
        (past, slot) -> {
          CAddress source = sourceAddress.apply(past, slot);
          CAddress target = targetAddress.apply(past, slot);
          putMoveSlot(target, source, slot);
          if (clone) {
            putCloneSlot(target, slot);
          }
        });
  }

  void putMoveSlot(CAddress target, CAddress source, IRSlot slot) {
    putCopyMemory(target, source, layout(slot).size);
  }

  // Marks a slot as cloned, e.g., incrementing reference counts if necessary
  void putCloneSlot(CAddress address, IRSlot slot) {
    CSlotCloner.clone(this, address, slot);
  }

  // Drops each slot on the tree
  void putDropSlots(IRSlotTree slots, BiFunction<IRSlotSequence, IRSlot, CAddress> addresser) {
    putSlotTraversal(
        slots,
        past -> addresser.apply(past, new IRTagS()),
        (past, slot) -> putDropSlot(addresser.apply(past, slot), slot));
  }

  // Drops a slot, e.g., decrementing reference counts if necessary
  void putDropSlot(CAddress address, IRSlot slot) {
    CSlotDropper.drop(this, address, slot);
  }

  void putSlotTraversal(
      IRSlotTree slots,
      Function<IRSlotSequence, CAddress> tagAddresser,
      BiConsumer<IRSlotSequence, IRSlot> atSlot) {
    putSlotTraversal(slots, tagAddresser, atSlot, IRSlotSequence.EMPTY);
  }

  void putSlotTraversal(
      IRSlotTree slots,
      Function<IRSlotSequence, CAddress> tagAddresser,
      BiConsumer<IRSlotSequence, IRSlot> atSlot,
      IRSlotSequence past) {
    if (slots.slot().isPresent()) {
      atSlot.accept(past, slots.slot().get());
    }

    if (slots.isUnary()) {
      putSlotTraversal(
          ((IRSlotTree.Unary) slots).child(),
          tagAddresser,
          atSlot,
          past.suffix(slots.slot().get()));
    } else if (slots.isTag()) {
      IRSlotTree.Tag tag = (IRSlotTree.Tag) slots;
      List<Runnable> cases = new ArrayList<>();

      for (IRSlotTree child : tag.cases()) {
        cases.add(
            () -> putSlotTraversal(child, tagAddresser, atSlot, past.suffix(slots.slot().get())));
      }

      putSwitch(tagAddresser.apply(past).deref("unsigned char"), cases);
    }
  }

  void putAllocTask(String var) {
    putAlloc(var, CSize.sizeOf("struct task"));
    if (compiler.profiling.get()) {
      putIncrementAtomic("task_allocs");
    }
  }

  void putFreeTask(String var) {
    putFree(var);
    if (compiler.profiling.get()) {
      putIncrementAtomic("task_frees");
    }
  }

  void putAllocExponential(
      String var, Function<IRTypeId, CLayout> typeLayouts, CProcessLayout layout) {
    putAlloc(var, compiler.arch.exponentialSize(layout.size(typeLayouts)));
    if (compiler.profiling.get()) {
      putIncrementAtomic("exponential_allocs");
    }
  }

  void putFreeExponential(String var) {
    putFree(var);
    if (compiler.profiling.get()) {
      putIncrementAtomic("exponential_frees");
    }
  }

  void putIncrementExponential(String var) {
    putIncrementAtomic(exponentialRefCount(var));
  }

  void putDecrementExponential(String var) {
    putIf(
        decrementAtomic(exponentialRefCount(var)) + " == 1",
        () -> {
          // Call the exponential's manager
          putStatement(exponentialManager(var) + "(" + exponentialEnv(var) + ", 1)"); // 1 = Drop
          putFreeExponential(var);
        });
  }

  void putAllocEnvironment(
      String var, Function<IRTypeId, CLayout> typeLayouts, CProcessLayout layout) {
    putAllocEnvironment(var, layout.size(typeLayouts));
  }

  void putAllocEnvironment(String var, CSize size) {
    putAlloc(var, size);
    if (compiler.profiling.get()) {
      putIncrementAtomic("env_allocs");
    }
  }

  void putFreeEnvironment(IRProcess process, CProcessLayout processLayout, String var) {
    if (compiler.tracing.get()) {
      putDebugLn("[endCall(" + process.getId() + ")]");
    }
    putDoDeferredEnvironmentDrops(process, processLayout, var);
    putFree(var);
    if (compiler.profiling.get()) {
      putIncrementAtomic("env_frees");
    }
  }

  void putZeroEnvironmentDropBits(CProcessLayout processLayout, String var) {
    putZeroMemory(CAddress.of(var, processLayout.dropByteStart()), processLayout.dropByteCount());
  }

  void putDoDeferredEnvironmentDrops(IRProcess process, CProcessLayout processLayout, String var) {
    for (int i = 0; i < process.getDropOnEnd().size(); ++i) {
      IRDropId dropId = new IRDropId(i);
      IRProcess.DropOnEnd drop = process.getDropOnEnd(dropId);
      Runnable putDrop =
          () ->
              putDropSlots(
                  drop.getSlots(),
                  (past, slot) ->
                      localData(drop.getLocalDataId(), drop.getOffset().advance(past, slot)));

      if (drop.isAlways()) {
        putDrop.run();
      } else {
        CSizeBits dropBitOffset = processLayout.dropBitOffset(dropId);
        CAddress dropByteAddress = CAddress.of(var, dropBitOffset.getSize());
        String dropByte = dropByteAddress.deref("unsigned char");
        putIf(dropByte + " & (1 << " + dropBitOffset.getBits() + ")", putDrop);
      }
    }
  }

  void putDecrementEndPoints(boolean isEndPoint) {
    putDecrementEndPoints(
        isEndPoint, () -> putFreeEnvironment(currentProcess, currentProcessLayout, ENV));
  }

  void putDecrementEndPoints(boolean isEndPoint, Runnable free) {
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
    return "((" + type + ")(" + expr + "))";
  }

  private CAddress castToAddress(String expr) {
    return CAddress.of(cast(expr, "char*"));
  }

  private String castAndDeref(String expr, String type) {
    return "(*(" + type + ")(" + expr + "))";
  }

  private String access(String expr, String type) {
    return castAndDeref(expr, type + "*");
  }

  private String labelAddress(String label) {
    return "&&" + label;
  }

  private String managerName(IRProcessId processId) {
    return processId.getName() + "_exp_manager";
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

  void putAlloc(String var, CSize size) {
    putAssign(var, "managed_alloc(" + size + ")");
  }

  void putFree(String var) {
    putStatement("managed_free(" + var + ")");
  }

  void putMutexInit(String var) {
    putStatement("pthread_mutex_init(&(" + var + "), NULL)");
  }

  void putMutexDestroy(String var) {
    putStatement("pthread_mutex_destroy(&(" + var + "))");
  }

  void putMutexLock(String var) {
    putStatement("pthread_mutex_lock(&(" + var + "))");
  }

  void putMutexUnlock(String var) {
    putStatement("pthread_mutex_unlock(&(" + var + "))");
  }

  void putCondVarInit(String var) {
    putStatement("pthread_cond_init(&(" + var + "), NULL)");
  }

  void putCondVarDestroy(String var) {
    putStatement("pthread_cond_destroy(&(" + var + "))");
  }

  void putCondVarWait(String var, String mutex) {
    putStatement("pthread_cond_wait(&(" + var + "), &(" + mutex + "))");
  }

  void putCondVarSignal(String var) {
    putStatement("pthread_cond_signal(&(" + var + "))");
  }

  void putIncrementAtomic(String var) {
    putStatement(incrementAtomic(var));
  }

  void putDecrementAtomic(String var) {
    putStatement(decrementAtomic(var));
  }

  void putSubtractAtomic(String var, int value) {
    if (value != 0) {
      if (compiler.concurrency.get()) {
        putStatement("atomic_fetch_sub(&" + var + ", " + value + ")");
      } else {
        putStatement(var + " -= " + value);
      }
    }
  }

  void putAssignMaxAtomic(String var, String value) {
    putStatement("atomic_store_max(&" + var + ", " + value + ")");
  }

  void putDebugLn(String message, String... args) {
    putDebug(message + "\\n", args);
  }

  void putDebug(String message, String... args) {
    message = message.replace("\"", "\\\"");
    String stringArgs = String.join(", ", args);
    putStatement(
        "fprintf(stderr, \""
            + message
            + "\""
            + (stringArgs.isEmpty() ? "" : (", " + stringArgs))
            + ")");
  }

  void putComment(String comment) {
    putLine("/* " + comment + " */");
  }

  void putConstantGoto(String label) {
    putStatement("goto " + label);
  }

  void putComputedGoto(String address) {
    putStatement("goto *" + address);
  }

  void putIf(String condition, Runnable then) {
    putBlock("if (" + condition + ")", then);
  }

  void putIfElse(String condition, Runnable then, Runnable otherwise) {
    putLine("if (" + condition + ") {");
    putIndented(then);
    putLine("} else {");
    putIndented(otherwise);
    putLine("}");
  }

  void putSwitch(String value, List<Runnable> cases) {
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

  void putWhile(String condition, Runnable body) {
    putBlock("while (" + condition + ")", body);
  }

  void putFor(String var, int from, int to, Runnable body) {
    putBlock("for (int " + var + " = " + from + "; " + var + " < " + to + "; ++" + var + ")", body);
  }

  void putFor(String init, String condition, String step, Runnable body) {
    putBlock("for (" + init + "; " + condition + "; " + step + ")", body);
  }

  void putLabel(String label) {
    put(label + ":");
    putLineEnd();
  }

  void putCopyMemory(CAddress dst, CAddress src, CSize size) {
    if (!size.equals(CSize.zero())) {
      putStatement("memcpy(" + dst + ", " + src + ", " + size + ")");
    }
  }

  void putZeroMemory(CAddress dst, CSize size) {
    if (!size.equals(CSize.zero())) {
      putStatement("memset(" + dst + ", 0, " + size + ")");
    }
  }

  void putAssign(String var, Object what) {
    putAssign(var, what.toString());
  }

  void putAssign(String var, String what) {
    putStatement(var + " = " + what);
  }

  void putReturn(String what) {
    putStatement("return " + what);
  }

  void putStatement(String statement) {
    putLine(statement + ";");
  }

  void putBlankLine() {
    putLine("");
  }

  void putLine(String line) {
    if (!line.isEmpty()) {
      putIndent();
      put(line);
    }
    putLineEnd();
  }

  void putStruct(String name, Runnable fields) {
    putLine("struct " + name + " {");
    putIndented(fields);
    putLine("};");
    putBlankLine();
  }

  void putBlock(String begin, Runnable indented) {
    putLine(begin + " {");
    putIndented(indented);
    putLine("}");
  }

  void putIndented(Runnable indented) {
    indentLevel += 1;
    indented.run();
    indentLevel -= 1;
  }

  void putIndent() {
    put("  ".repeat(indentLevel));
  }

  void putLineEnd() {
    put("\n");
  }

  void put(String str) {
    output.append(str);
  }

  private String makeLabel(String base) {
    return base + "_" + (nextLabelId++);
  }
}
