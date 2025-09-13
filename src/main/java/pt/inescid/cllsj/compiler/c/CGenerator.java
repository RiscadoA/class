package pt.inescid.cllsj.compiler.c;

import java.io.PrintStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.function.BiConsumer;
import java.util.function.Function;
import pt.inescid.cllsj.compiler.Compiler;
import pt.inescid.cllsj.compiler.ir.IRValueRequisites;
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
import pt.inescid.cllsj.compiler.ir.slot.IRSlotDynamicOffset;
import pt.inescid.cllsj.compiler.ir.slot.IRSlotSequence;
import pt.inescid.cllsj.compiler.ir.slot.IRSlotStaticOffset;
import pt.inescid.cllsj.compiler.ir.slot.IRSlotTree;
import pt.inescid.cllsj.compiler.ir.slot.IRStringS;

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

  // Type flags
  public static final int TYPE_FLAG_VALUE = 1 << 0;

  private Compiler compiler;
  private IRProgram program;
  private PrintStream output;

  private IRProcess currentProcess = null;
  private CProcessLayout currentProcessLayout;
  private int indentLevel = 0;
  private int nextLabelId = 0;

  private Map<IRProcessId, List<IRWriteExponential.DataArgument>> pendingExponentialManagers =
      new HashMap<>();

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
          putStatement("unsigned char alignment;");
          putStatement("unsigned char flags;");
          putStatement("unsigned int strings;");
          putStatement("unsigned int exponentials;");
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
        "cell",
        () -> {
          if (compiler.concurrency.get()) {
            putStatement("pthread_mutex_t mutex");
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

    // Utility for getting and clearing, respectively, the least significant set bit of an integer.
    putLine("#define GET_LSSB(x) ((x) & -(x))");
    putLine("#define CLEAR_LSSB(x) ((x) & ((x) - 1))");
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
          putIfElse(
              "entry == NULL",
              () -> {
                IRProcess entryProcess = program.get(new IRProcessId(compiler.entryProcess.get()));
                if (entryProcess == null) {
                  throw new IllegalArgumentException(
                      "Entry process " + compiler.entryProcess.get() + " not found");
                }
                generate(
                    new IRCallProcess(
                        entryProcess.getId(), List.of(), List.of(), List.of(), false));
              },
              () -> {
                putAssign(ENV, taskContEnv("entry"));
                putAssign(TMP_PTR1, taskCont("entry"));
                putFreeTask("entry");
                putComputedGoto(TMP_PTR1);
              });
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
            if (compiler.allocatorLevels.get() > 0) {
              putFor(
                  "i",
                  0,
                  compiler.allocatorLevels.get(),
                  () -> {
                    putMutexDestroy("allocator_mutex[i]");
                  });
            }
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
    if (compiler.tracing.get()) {
      putDebugLn("[block(" + currentProcess.getId() + ":" + block.getLocation().getLabel() + ")]");
    }
    if (compiler.debug.get()) {
      putDebugLn("| env: %p", ENV);
    }
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

  private void generateExponentialManager(
      IRProcessId processId, List<IRWriteExponential.DataArgument> dataArguments) {
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
                for (IRWriteExponential.DataArgument arg : dataArguments) {
                  IRLocalDataId dataId = arg.getTargetDataId();
                  putSlotTraversal(
                      arg.getSlots(),
                      reqs -> isValue(layout, "exp_env", reqs),
                      offset -> localData(typeLayoutProvider, layout, "exp_env", dataId, offset),
                      (offset, slot) -> {
                        CAddress target =
                            localData(typeLayoutProvider, layout, "exp_env", dataId, offset);
                        putCloneSlot(target, slot);
                      });
                }
              },
              () -> {
                putComment("Drop mode");
                for (IRWriteExponential.DataArgument arg : dataArguments) {
                  IRLocalDataId dataId = arg.getTargetDataId();
                  putSlotTraversal(
                      arg.getSlots(),
                      reqs -> isValue(layout, "exp_env", reqs),
                      offset -> localData(typeLayoutProvider, layout, "exp_env", dataId, offset),
                      (offset, slot) -> {
                        CAddress target =
                            localData(typeLayoutProvider, layout, "exp_env", dataId, offset);
                        putDropSlot(target, slot);
                      });
                }
              });
        });
  }

  // ============================ Instruction generation visit methods ============================

  @Override
  public void visit(IRPushTask instr) {
    if (instr.isConcurrent() && compiler.concurrency.get()) {
      putAllocTask(TMP_PTR1);
      putAssign(taskCont(cast(TMP_PTR1, "struct task*")), codeLocationAddress(instr.getLocation()));
      putAssign(taskContEnv(cast(TMP_PTR1, "struct task*")), ENV);
      putIncrementAtomic("thread_inits");
      putLaunchThread("thread", TMP_PTR1);
    } else {
      putPushTask(TMP_PTR1, codeLocationAddress(instr.getLocation()));
    }
  }

  private void putPushTask(String tmpVar, String address) {
    putAssign(tmpVar, TASK);
    putAllocTask(TASK);
    putAssign(taskNext(TASK), cast(tmpVar, "struct task*"));
    putAssign(taskCont(TASK), address);
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
    putContinue(
        TMP_PTR1,
        accessSession(instr.getSessionId()),
        codeLocationAddress(instr.getContinuation()));
  }

  @Override
  public void visit(IRFinishSession instr) {
    String session = accessSession(instr.getSessionId());

    putAssign(sessionContSession(accessRemoteSession(session)), NULL);

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
    String source = data(instr.getLocation()).deref("struct session*");
    CAddress target = sessionAddress(instr.getSessionId());
    putCopyMemory(target, CAddress.of(source), compiler.arch.sessionSize());

    // Modify the remote session to point to our environment
    String remoteSession = accessRemoteSession(instr.getSessionId());
    putAssign(sessionContEnv(remoteSession), ENV);
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
    String ref = data(instr.getLocation()).deref("struct session*");
    putAssign(ref, sessionAddress(instr.getSessionId()).cast("struct session"));
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
    putMoveSlots(
        instr.getSlots(),
        req -> isValue(req),
        false,
        instr.getLocation(),
        instr.getSourceLocation());
  }

  @Override
  public void visit(IRCloneValue instr) {
    putMoveSlots(
        instr.getSlots(),
        req -> isValue(req),
        true,
        instr.getLocation(),
        instr.getSourceLocation());
  }

  @Override
  public void visit(IRDropValue instr) {
    putDropSlots(
        instr.getSlots(), req -> isValue(req), offset -> data(instr.getLocation().advance(offset)));
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
    putIf(
        remoteSessionAddress(TMP_SESSION) + " != " + NULL,
        () -> {
          putAssign(accessRemoteSession(TMP_SESSION), accessSession(instr.getPosId()));
        });
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
      String source;
      if (arg.isFromLocation()) {
        source = "(*" + data(arg.getSourceSessionLocation()).deref("struct session*") + ")";
      } else {
        source = accessSession(arg.getSourceSessionId());
      }
      String target = accessSession(calledLayout, newEnv, arg.getTargetSessionId());

      // Copy the session data
      putAssign(target, source);

      // We must also compute the new continuation data address for the target session
      // This is necessary as the caller may apply an offset to the data
      putAssign(sessionContData(target), offset(sessionContData(target), arg.getDataOffset()));

      // Update the remote session's continuation to match the new environment
      Optional<IRLocalDataId> calledLocalDataId =
          calledProcess.getArgSessionLocalDataId(arg.getTargetSessionId());
      String remoteSessionAddress = remoteSessionAddress(source);
      putIf(
          remoteSessionAddress + " != " + NULL,
          () -> {
            String remoteSession = accessSession(remoteSessionAddress);
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
          });
    }

    for (IRCallProcess.DataArgument arg : instr.getDataArguments()) {
      // Here we simply copy data from some location in the current environment to
      // a local data section in the new environment
      IRDataLocation target = IRDataLocation.local(arg.getTargetDataId(), IRSlotDynamicOffset.ZERO);
      putMoveSlots(
          arg.getSlots(),
          arg.isClone(),
          req -> isValue(calledLayout, newEnv, req),
          offset -> data(typeLayoutProvider, calledLayout, newEnv, target.advance(offset)),
          offset -> data(arg.getSourceLocation().advance(offset)));
    }

    // Decrement the end points of the current process, if applicable
    putDecrementEndPoints(instr.isEndPoint());

    putAssign(ENV, cast(TMP_PTR1, "char*"));
    putConstantGoto(codeLocationLabel(calledProcess, IRCodeLocation.entry()));
  }

  @Override
  public void visit(IRWriteExponential instr) {
    putWriteExponential(
        data(instr.getLocation()).deref("struct exponential*"),
        instr.getProcessId(),
        instr.getTypeArguments(),
        instr.getDataArguments());
  }

  private void putWriteExponential(
      String exponential,
      IRProcessId processId,
      List<IRWriteExponential.TypeArgument> typeArguments,
      List<IRWriteExponential.DataArgument> dataArguments) {
    IRProcess expProcess = program.get(processId);
    if (expProcess == null) {
      throw new IllegalArgumentException("Exponential process " + processId + " not found");
    }

    Function<IRTypeId, CLayout> typeLayoutProvider =
        typeId -> {
          // We need to find the type layout for given type identifier in the called process
          // This type was passed as an argument, so we search for it
          IRWriteExponential.TypeArgument arg =
              typeArguments.stream()
                  .filter(a -> a.getTargetType().equals(typeId))
                  .findFirst()
                  .orElseThrow(
                      () ->
                          new IllegalArgumentException(
                              "Type argument "
                                  + typeId
                                  + " not found in call to exponential process "
                                  + processId));

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
    putAllocExponential(exponential, typeLayoutProvider, expProcessLayout);

    // Initialize the exponential
    pendingExponentialManagers.put(processId, dataArguments);
    putAssign(exponentialRefCount(exponential), 1);
    putAssign(
        exponentialEntrySessionOffset(exponential),
        expProcessLayout.sessionOffset(new IRSessionId(0)));
    putAssign(
        exponentialEntryDataOffset(exponential),
        expProcessLayout.dataOffset(typeLayoutProvider, new IRLocalDataId(0)));
    putAssign(exponentialEnvSize(exponential), expProcessLayout.size(typeLayoutProvider));
    putStatement(
        "void " + managerName(processId) + "(char* env, int mode)"); // Forward declare function
    putAssign(exponentialManager(exponential), managerName(processId));
    putAssign(TMP_PTR1, exponentialEnv(exponential));
    String newEnv = cast(TMP_PTR1, "char*");

    // Setup the end points for the new environment
    if (!compiler.optimizeSingleEndpoint.get() || expProcess.getEndPoints() != 1) {
      putAssign(endPoints(expProcessLayout, newEnv), expProcess.getEndPoints());
    }

    // Zero out any drop bits
    putZeroEnvironmentDropBits(expProcessLayout, newEnv);

    // Setup the continuation of the entry session
    CAddress entrySessionAddress =
        CAddress.of(newEnv, expProcessLayout.sessionOffset(new IRSessionId(0)));
    String entrySession = entrySessionAddress.deref("struct session");
    putAssign(
        sessionCont(entrySession),
        labelAddress(codeLocationLabel(expProcess, IRCodeLocation.entry())));

    // Write the type arguments to the exponential's environment
    for (IRWriteExponential.TypeArgument arg : typeArguments) {
      // Get a reference to the target type in the new environment and initialize itq
      String targetType = type(expProcessLayout, newEnv, arg.getTargetType());
      putAssign(targetType, typeInitializer(arg.getSourceTree()));
    }

    for (IRWriteExponential.DataArgument arg : dataArguments) {
      // Here we simply copy data from some location in the current environment to
      // a local data section in the new environment
      IRDataLocation target = IRDataLocation.local(arg.getTargetDataId(), IRSlotDynamicOffset.ZERO);
      putMoveSlots(
          arg.getSlots(),
          arg.isClone(),
          req -> isValue(expProcessLayout, newEnv, req),
          offset -> data(typeLayoutProvider, expProcessLayout, newEnv, target.advance(offset)),
          offset -> data(arg.getSourceLocation().advance(offset)));
    }
  }

  @Override
  public void visit(IRCallExponential instr) {
    String exponential = data(instr.getLocation()).deref("struct exponential*");
    Optional<CAddress> localSessionAddress = Optional.of(sessionAddress(instr.getSessionId()));
    Optional<CAddress> localDataAddress = Optional.of(localData(instr.getLocalDataId()));
    putCallExponential(TMP_PTR1, exponential, localSessionAddress, localDataAddress);
  }

  private void putCallExponential(
      String envVar,
      String exponential,
      Optional<CAddress> localSessionAddress,
      Optional<CAddress> localDataAddress) {
    CAddress oldEnv = CAddress.of(exponentialEnv(exponential));

    // Allocate the new environment and initialize it by copying the one stored in the exponential
    putAllocEnvironment(envVar, CSize.expression(exponentialEnvSize(exponential)));
    CAddress newEnv = castToAddress(envVar);
    putCopyMemory(newEnv, oldEnv, CSize.expression(exponentialEnvSize(exponential)));

    // Call the exponential manager to clone the slots
    putStatement(
        exponentialManager(exponential) + "(" + cast(envVar, "char*") + ", 0)"); // 0 = Clone

    // Setup the new session and tie it to the exponential's entry session
    CAddress remoteSessionAddress =
        newEnv.offset(CSize.expression(exponentialEntrySessionOffset(exponential)));
    String remoteSession = remoteSessionAddress.deref("struct session");
    CAddress remoteData = newEnv.offset(CSize.expression(exponentialEntryDataOffset(exponential)));

    // Initialize local session
    if (localSessionAddress.isPresent()) {
      StringBuilder sb = new StringBuilder("(struct session)");
      sb.append("{.cont=").append(sessionCont(remoteSession));
      sb.append(",.cont_env=").append(newEnv);
      sb.append(",.cont_data=").append(remoteData);
      sb.append(",.cont_session=").append(remoteSessionAddress);
      sb.append("}");
      putAssign(localSessionAddress.get().deref("struct session"), sb.toString());
    }

    // Initialize remote session (the one stored in the exponential)
    putAssign(sessionContEnv(remoteSession), ENV);
    if (localDataAddress.isPresent()) {
      putAssign(sessionContData(remoteSession), localDataAddress.get());
    }
    if (localSessionAddress.isPresent()) {
      putAssign(sessionContSession(remoteSession), localSessionAddress.get());
    } else {
      putAssign(sessionContSession(remoteSession), remoteSession);
    }
  }

  @Override
  public void visit(IRBranchTag instr) {
    List<Runnable> cases = new ArrayList<>();
    for (IRBranchTag.Case c : instr.getCases()) {
      cases.add(
          () -> {
            putSubtractAtomic(endPoints(), instr.getMaxEndPoints() - c.getEndPoints());
            if (instr.getMaxEndPoints() != c.getEndPoints() && compiler.debug.get()) {
              putDebugLn("| end points: %d", endPoints());
            }
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
  public void visit(IRBranchIsValue instr) {
    // TODO:
    throw new UnsupportedOperationException("TODO: implement");
  }

  @Override
  public void visit(IRDeferDrop instr) {
    CSizeBits dropBitOffset = currentProcessLayout.dropBitOffset(instr.getDropId());
    CAddress dropByteAddress = CAddress.of(ENV, dropBitOffset.getSize());
    String dropByte = dropByteAddress.deref("unsigned char");
    putAssign(dropByte, dropByte + " | (1 << " + dropBitOffset.getBits() + ")");
  }

  @Override
  public void visit(IRWriteCell instr) {
    CAddress slotAddr = data(instr.getLocation());
    String cell = slotAddr.deref("struct cell*");
    putAllocCell(cell, layout(instr.getSlots().combinations()).size);
    putAssign(cellRefCount(cell), 1);
    if (compiler.concurrency.get()) {
      putMutexInit(cellMutex(cell));
    }
  }

  @Override
  public void visit(IRIncrementCell instr) {
    putIncrementCell(data(instr.getLocation()).deref("struct cell*"));
  }

  @Override
  public void visit(IRDecrementCell instr) {
    putDecrementCell(data(instr.getLocation()));
  }

  @Override
  public void visit(IRLockCell instr) {
    if (compiler.concurrency.get()) {
      putMutexLock(cellMutex(data(instr.getLocation()).deref("struct cell*")));
    }
  }

  @Override
  public void visit(IRUnlockCell instr) {
    if (compiler.concurrency.get()) {
      putMutexUnlock(cellMutex(data(instr.getLocation()).deref("struct cell*")));
    }
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
    CAlignment alignment =
        compiler.arch.pointerAlignment.equals(CAlignment.one())
            ? CAlignment.one()
            : CAlignment.expression(typeAlignment(type));
    return new CLayout(CSize.expression(typeSize(type)), alignment);
  }

  private String typeInitializer(IRSlotTree slots) {
    CLayout layout = layout(slots.combinations());
    StringBuilder sb = new StringBuilder("(struct type)");
    sb.append("{.size=").append(layout.size);
    sb.append(",.alignment=").append(layout.alignment);
    sb.append("}");
    return sb.toString();
  }

  private String type(CProcessLayout layout, String env, IRTypeId id) {
    CSize offset = layout.typeOffset(id);
    return access(offset.advancePointer(env), "struct type");
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

  private String accessRemoteSession(String session) {
    return accessSession(remoteSessionAddress(session));
  }

  private CAddress localData(IRLocalDataId localDataId) {
    return localData(this::typeLayout, currentProcessLayout, ENV, localDataId, IRSlotDynamicOffset.ZERO);
  }

  private CAddress localData(IRLocalDataId localDataId, IRSlotDynamicOffset offset) {
    return localData(this::typeLayout, currentProcessLayout, ENV, localDataId, offset);
  }

  private CAddress localData(
      Function<IRTypeId, CLayout> typeLayouts,
      CProcessLayout layout,
      String env,
      IRLocalDataId localDataId,
      IRSlotDynamicOffset offset) {
    return offset(CAddress.of(env, layout.dataOffset(typeLayouts, localDataId)), offset);
  }

  private CAddress remoteData(
      CProcessLayout layout, String env, IRSessionId sessionId, IRSlotDynamicOffset offset) {
    return offset(sessionContData(accessSession(sessionId)), offset);
  }

  private CAddress cellData(CAddress cell, IRSlotDynamicOffset offset) {
    CAddress base =
        CAddress.of(cell.deref("char*"), compiler.arch.cellDataOffset(compiler.concurrency.get()));
    return offset(base, offset);
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
    } else if (data.isLocal()) {
      return localData(typeLayouts, layout, env, data.getLocalDataId(), data.getOffset());
    } else if (data.isCell()) {
      return cellData(data(typeLayouts, layout, env, data.getCell()), data.getOffset());
    } else {
      throw new IllegalArgumentException("Unknown data location " + data);
    }
  }

  private String typeSize(String var) {
    return var + ".size";
  }

  private String typeAlignment(String var) {
    return var + ".alignment";
  }

  private String typeFlags(String var) {
    return var + ".flags";
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

  private String cellRefCount(String var) {
    return var + "->ref_count";
  }

  private String cellMutex(String var) {
    return var + "->mutex";
  }

  // private CSize offset(CSize base, IRSlotDynamicOffset offset) {
  //   if (offset.isZero()) {
  //     return base;
  //   } else {
  //     CSize pastSize = layout(offset.getPast()).size;
  //     CAlignment futureAlignment = layout(offset.getFuture()).alignment;
  //     return base.add(pastSize).align(futureAlignment);
  //   }
  // }

  private CAddress offset(String address, IRSlotDynamicOffset offset) {
    return offset(CAddress.of(address), offset);
  }

  private CAddress offset(CAddress address, IRSlotDynamicOffset offset) {
    CSize pastSize = layout(offset.getPast(), address).size;
    CAlignment futureAlignment = layout(offset.getFuture()).alignment;
    return address.offset(pastSize.align(futureAlignment));
  }

  private CAddress offset(CAddress address, IRSlotStaticOffset offset) {
    CSize pastSize = layout(IRSlotCombinations.of(offset.getPast())).size;
    CAlignment futureAlignment = layout(offset.getFuture()).alignment;
    return address.offset(pastSize.align(futureAlignment));
  }

  private CLayout layout(IRSlotCombinations combinations) {
    return CLayout.compute(combinations, compiler.arch, typeId -> typeLayout(typeId));
  }

  private CLayout layout(IRSlot slot) {
    return CLayout.compute(slot, compiler.arch, typeId -> typeLayout(typeId));
  }

  private CLayout layout(IRSlotTree slots, CAddress baseAddress) {
    return CLayout.compute(slots, compiler.arch, 
      this::typeLayout,
      this::isValue,
      (offset, tag) -> CCondition.maybe(offset(baseAddress, offset).deref("unsigned char") + " == " + tag));
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

  void putContinue(String tmpVar, String session, String continuationAddress) {
    putAssign(tmpVar, sessionCont(session));
    putAssign(sessionCont(accessRemoteSession(session)), continuationAddress);
    putAssign(ENV, sessionContEnv(session));
    putComputedGoto(tmpVar);
  }

  void putMoveSlots(
      IRSlotTree slots,
      Function<IRValueRequisites, CCondition> typeIsValue,
      boolean clone,
      IRDataLocation targetLocation,
      IRDataLocation sourceLocation) {
    putMoveSlots(
        slots,
        clone,
        typeIsValue,
        offset -> data(targetLocation.advance(offset)),
        offset -> data(sourceLocation.advance(offset)));
  }

  void putMoveSlots(
      IRSlotTree slots,
      boolean clone,
      Function<IRValueRequisites, CCondition> typeIsValue,
      Function<IRSlotDynamicOffset, CAddress> targetAddress,
      Function<IRSlotDynamicOffset, CAddress> sourceAddress) {
    putSlotTraversal(
        slots,
        typeIsValue,
        offset -> sourceAddress.apply(offset),
        (offset, slot) -> {
          CAddress source = sourceAddress.apply(offset);
          CAddress target = targetAddress.apply(offset);
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
  void putDropSlots(
      IRSlotTree slots,
      Function<IRValueRequisites, CCondition> typeIsValue,
      Function<IRSlotDynamicOffset, CAddress> addresser) {
    putSlotTraversal(
        slots,
        typeIsValue,
        offset -> addresser.apply(offset),
        (offset, slot) -> putDropSlot(addresser.apply(offset), slot));
  }

  // Drops a slot, e.g., decrementing reference counts if necessary
  void putDropSlot(CAddress address, IRSlot slot) {
    CSlotDropper.drop(this, address, slot);
  }

  // Drops an affine session, i..e, writes a '0' (discard) tag and continues it.
  void putDropAffine(String session) {
    String ref = CAddress.of(sessionContData(session)).deref("unsigned char");
    putAssign(ref, 0); // 0 means discard

    String label = makeLabel("drop_affine");
    putContinue(TMP_PTR1, session, labelAddress(label));
    putLabel(label);
  }

  void putSlotTraversal(
      IRSlotTree slots,
      Function<IRValueRequisites, CCondition> typeIsValue,
      Function<IRSlotDynamicOffset, CAddress> tagAddresser,
      BiConsumer<IRSlotDynamicOffset, IRSlot> atSlot) {
    putSlotTraversal(slots, typeIsValue, tagAddresser, atSlot, IRSlotSequence.EMPTY);
  }

  void putSlotTraversal(
      IRSlotTree slots,
      Function<IRValueRequisites, CCondition> typeIsValue,
      Function<IRSlotDynamicOffset, CAddress> tagAddresser,
      BiConsumer<IRSlotDynamicOffset, IRSlot> atSlot,
      IRSlotSequence past) {
    if (slots.singleHead().isPresent()) {
      atSlot.accept(IRSlotDynamicOffset.of(past, slots.combinations()), slots.singleHead().get());
    }

    if (slots.isUnary()) {
      putSlotTraversal(
          ((IRSlotTree.Unary) slots).child(),
          typeIsValue,
          tagAddresser,
          atSlot,
          past.suffix(slots.singleHead().get()));
    } else if (slots.isTag()) {
      IRSlotTree.Tag tag = (IRSlotTree.Tag) slots;
      List<Runnable> cases = new ArrayList<>();

      for (IRSlotTree child : tag.cases()) {
        cases.add(
            () ->
                putSlotTraversal(
                    child,
                    typeIsValue,
                    tagAddresser,
                    atSlot,
                    past.suffix(slots.singleHead().get())));
      }

      putSwitch(
          tagAddresser.apply(IRSlotDynamicOffset.of(past, tag.combinations())).deref("unsigned char"),
          cases);
    } else if (slots.isIsValue()) {
      IRSlotTree.IsValue isValue = (IRSlotTree.IsValue) slots;

      putIfElse(
          typeIsValue.apply(isValue.requisites()),
          () -> {
            putSlotTraversal(isValue.value(), typeIsValue, tagAddresser, atSlot, past);
          },
          () -> {
            putSlotTraversal(isValue.notValue(), typeIsValue, tagAddresser, atSlot, past);
          });
    } else if (!slots.isLeaf()) {
      throw new IllegalArgumentException("Unknown slot tree " + slots);
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
          if (compiler.tracing.get()) {
            putDebugLn("[freeExponential]");
          }

          // Call the exponential's manager
          putStatement(exponentialManager(var) + "(" + exponentialEnv(var) + ", 1)"); // 1 = Drop
          putFreeExponential(var);
        });
  }

  void putAllocCell(String var, CSize dataSize) {
    putAlloc(var, compiler.arch.cellSize(compiler.concurrency.get(), dataSize));
    if (compiler.profiling.get()) {
      putIncrementAtomic("cell_allocs");
    }
  }

  void putFreeCell(String var) {
    putFree(var);
    if (compiler.profiling.get()) {
      putIncrementAtomic("cell_frees");
    }
  }

  void putIncrementCell(String var) {
    putIncrementAtomic(cellRefCount(var));
  }

  void putDecrementCell(CAddress cellAddr) {
    String ref = cellAddr.deref("struct cell*");

    putIf(
        decrementAtomic(cellRefCount(ref)) + " == 1",
        () -> {
          if (compiler.tracing.get()) {
            putDebugLn("[freeCell]");
          }

          // To drop the cell's contents, we simply drop its affine session
          String sessionPtr = cellData(cellAddr, IRSlotDynamicOffset.ZERO).deref("struct session*");
          putDropAffine(accessSession(sessionPtr));

          // Now we can free the cell itself
          if (compiler.concurrency.get()) {
            putMutexDestroy(cellMutex(ref));
          }
          putFreeCell(ref);
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
      putAssignMaxAtomic("env_peak", "env_allocs - env_frees");
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
                  reqs -> isValue(processLayout, var, reqs),
                  offset -> localData(drop.getLocalDataId(), drop.getOffset().advance(offset)));

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
      if (compiler.debug.get()) {
        putDebugLn("| end points: %d", endPoints());
      }
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

  private String makeLabel(String prefix) {
    return currentProcess.getId() + "_" + prefix + "_" + (nextLabelId++);
  }

  private CCondition isValue(IRValueRequisites reqs) {
    return isValue(currentProcessLayout, ENV, reqs);
  }

  private CCondition isValue(CProcessLayout processLayout, String env, IRValueRequisites reqs) {
    if (reqs.mustBeValue()) {
      return CCondition.certainlyTrue();
    } else if (reqs.canBeValue()) {
      String expr = "";
      for (IRTypeId typeId : reqs.typesWhichMustBeValue()) {
        if (!expr.isEmpty()) {
          expr += " && ";
        }
        expr += "(" + typeFlags(type(processLayout, env, typeId)) + " & " + TYPE_FLAG_VALUE + ")";
      }
      return CCondition.maybe(expr);
    } else {
      return CCondition.certainlyFalse();
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

  void putLaunchThread(String func, String arg) {
    putBlock(
        "",
        () -> {
          putStatement("pthread_t result_thread");
          putStatement("pthread_create(&result_thread, NULL, " + func + ", " + arg + ")");
        });
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
    if (compiler.concurrency.get()) {
      putStatement("atomic_store_max(&" + var + ", " + value + ")");
    } else {
      putStatement(var + " = MAX(" + var + ", " + value + ")");
    }
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
    putIfElse(CCondition.maybe(condition), then, otherwise);
  }

  void putIfElse(CCondition condition, Runnable then, Runnable otherwise) {
    if (condition.isCertainlyTrue()) {
      then.run();
    } else if (condition.isCertainlyFalse()) {
      otherwise.run();
    } else {
      putLine("if (" + condition.expression() + ") {");
      putIndented(then);
      putLine("} else {");
      putIndented(otherwise);
      putLine("}");
    }
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
          if (compiler.debug.get()) {
            putLine("default:");
            putIndented(
                () -> {
                  putDebugLn("Invalid switch case: %d", value);
                  putStatement("exit(1)");
                });
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
    putLine(begin + (begin.isEmpty() ? "{" : " {"));
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
}
