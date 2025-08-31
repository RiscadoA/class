package pt.inescid.cllsj.compiler;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import pt.inescid.cllsj.compiler.ir.*;
import pt.inescid.cllsj.compiler.ir.expressions.*;
import pt.inescid.cllsj.compiler.ir.instructions_old.*;
import pt.inescid.cllsj.compiler.ir.instructions_old.IRCallProcess.ExponentialArgument;
import pt.inescid.cllsj.compiler.ir.instructions_old.IRCallProcess.LinearArgument;
import pt.inescid.cllsj.compiler.ir.instructions_old.IRCallProcess.TypeArgument;
import pt.inescid.cllsj.compiler.ir.type.*;

public class CGenerator extends IRInstructionVisitor {
  private static final String TMP_TASK = "tmp_task";
  private static final String TMP_ENV = "tmp_env";
  private static final String TMP_CONT = "tmp_cont";
  private static final String TMP_RECORD = "tmp_record";
  private static final String TMP_EXPONENTIAL = "tmp_exponential";
  private static final String TMP_THREAD = "tmp_thread";
  private static final String TMP_CELL = "tmp_cell";
  private static final String TMP_INT = "tmp_int";

  private static final String TASK = "task";
  private static final String ENV = "env";

  private static final int TYPE_ID_OTHER = 0;
  private static final int TYPE_ID_INT = 1;
  private static final int TYPE_ID_BOOL = 2;
  private static final int TYPE_ID_CLOSE = 3;

  private static int nextLabel = 0;

  private IRProgram ir;
  private String code = "";
  private int indentLevel = 0;
  private String procName;
  private int typeCount;
  private int recordCount;
  private int exponentialCount;
  private boolean entryCall = true;

  public String entryProcess = "main";
  public boolean trace = false;
  public boolean debug = false;
  public boolean profile = false;
  public boolean disableConcurrency = false;
  public boolean optimizePrimitiveExponentials = true;
  public boolean optimizeTailCalls = true;
  public boolean optimizeSendValue = true;
  public boolean optimizeSingleEndpoint = true;
  public CArchitecture arch = new CArchitecture();

  public int customAllocatorSizeDivisor = 64;
  public int customAllocatorLevels = 4;

  public String generate(IRProgram ir) {
    this.ir = ir;

    // Add the necessary includes.
    putLine("#define _POSIX_C_SOURCE 199309L");
    putLine("#include <stdio.h>");
    putLine("#include <stdlib.h>");
    putLine("#include <string.h>");
    putLine("#include <stdint.h>");
    if (!disableConcurrency) {
      putLine("#include <pthread.h>");
      putLine("#include <stdatomic.h>");
    }
    putLine("#include <time.h>");
    putBlankLine();

    // Initialize the profiling variables.
    String counterType = disableConcurrency ? "unsigned long" : "atomic_ulong";
    if (!disableConcurrency) {
      putStatement("pthread_cond_t thread_stops_cond_var");
      putStatement("pthread_mutex_t thread_stops_mutex");
      putStatement(counterType + " thread_inits = 1");
      putStatement(counterType + " thread_stops = 0");
      putBlankLine();
    }
    if (profile) {
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
    if (customAllocatorLevels > 0) {
      putLine("struct allocation {");
      incIndent();
      putLine("int level;");
      putLine("struct allocation* next;");
      putLine("char data[];");
      decIndent();
      putLine("};");
      putBlankLine();

      putStatement("struct allocation* allocator_list[" + customAllocatorLevels + "]");
      if (!disableConcurrency) {
        putStatement("pthread_mutex_t allocator_mutex[" + customAllocatorLevels + "]");
      }
      putBlankLine();
    }

    // Define the allocation functions.
    if (customAllocatorLevels == 0) {
      putLine("#define managed_alloc(size) malloc(size)");
    } else {
      putLine("void* managed_alloc(size_t size) {");
      incIndent();
      // Compute the level of the allocator to use based on the size.
      putStatement(
          "int level = (size + "
              + customAllocatorSizeDivisor
              + " - 1) / "
              + customAllocatorSizeDivisor);
      putIfElse(
          "level >= " + customAllocatorLevels,
          () -> {
            putStatement("struct allocation* alloc = malloc(sizeof(struct allocation) + size)");
            putStatement("alloc->level = " + (customAllocatorLevels - 1));
            putStatement("return alloc->data");
          },
          () -> {
            if (!disableConcurrency) {
              putStatement("pthread_mutex_lock(&allocator_mutex[level])");
            }
            putIfElse(
                "allocator_list[level] == NULL",
                () -> {
                  if (!disableConcurrency) {
                    putStatement("pthread_mutex_unlock(&allocator_mutex[level])");
                  }
                  putStatement(
                      "struct allocation* alloc = malloc(sizeof(struct allocation) + (level + 1) * "
                          + customAllocatorSizeDivisor
                          + ")");
                  putStatement("alloc->level = level");
                  putStatement("return alloc->data");
                },
                () -> {
                  putStatement("struct allocation* alloc = allocator_list[level]");
                  putStatement("allocator_list[level] = alloc->next");
                  if (!disableConcurrency) {
                    putStatement("pthread_mutex_unlock(&allocator_mutex[level])");
                  }
                  putStatement("return alloc->data");
                });
          });
      decIndent();
      putLine("}");
    }
    putBlankLine();
    if (customAllocatorLevels == 0) {
      putLine("#define managed_free(ptr) free(ptr)");
    } else {
      putLine("void managed_free(void* ptr) {");
      incIndent();
      putStatement(
          "struct allocation* alloc = (struct allocation*)((char*)ptr - "
              + "sizeof(struct allocation))");
      putStatement("int level = alloc->level");
      if (!disableConcurrency) {
        putStatement("pthread_mutex_lock(&allocator_mutex[level])");
      }
      putStatement("alloc->next = allocator_list[level]");
      putStatement("allocator_list[level] = alloc");
      if (!disableConcurrency) {
        putStatement("pthread_mutex_unlock(&allocator_mutex[level])");
      }
      decIndent();
      putLine("}");
    }
    putBlankLine();
    if (customAllocatorLevels == 0) {
      putLine("#define managed_realloc(ptr, size) realloc(ptr, size)");
    } else {
      putLine("void* managed_realloc(void* ptr, size_t size) {");
      incIndent();
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
          "size <= " + customAllocatorSizeDivisor + " * (level + 1)",
          () -> {
            putStatement("return alloc->data");
          },
          () -> {
            putStatement("void* new_ptr = managed_alloc(size)");
            putStatement(
                "memcpy(new_ptr, alloc->data, " + customAllocatorSizeDivisor + " * (level + 1))");
            putStatement("managed_free(alloc->data)");
            putStatement("return new_ptr");
          });
      decIndent();
      putLine("}");
    }
    putBlankLine();

    // Define the environment struct.
    putLine("struct environment {");
    incIndent();
    if (disableConcurrency) {
      putLine("int end_points;");
    } else {
      putLine("atomic_int end_points;");
    }
    putLine("void* bindings[];");
    decIndent();
    putLine("};");
    putBlankLine();

    // Holds the data present at the start of a record buffer.
    putLine("struct record_header {");
    incIndent();
    putLine("void* cont;");
    putLine("struct environment* cont_env;");
    putLine("unsigned char cont_record;");
    decIndent();
    putLine("};");
    putBlankLine();

    // Define the exponential struct.
    putLine("struct exponential {");
    incIndent();
    if (disableConcurrency) {
      putLine("int ref_count;");
    } else {
      putLine("atomic_int ref_count;");
    }
    putLine("int env_record_count;");
    putLine("int env_exponential_count;");
    putLine("int env_type_count;");
    putLine("void* entry;");
    putLine("void* data[];");
    decIndent();
    putLine("};");
    putBlankLine();

    // Define the type struct.
    putLine("struct type {");
    incIndent();
    putLine("int flags;");
    putLine("int size;");
    putLine("int alignment;");
    decIndent();
    putLine("};");
    putBlankLine();

    // Define the type slot struct.
    putLine("struct type_slot {");
    incIndent();
    putLine("char* record;");
    putLine("struct type type;");
    decIndent();
    putLine("};");
    putBlankLine();

    // Define the task struct.
    putLine("struct task {");
    incIndent();
    putLine("struct task* next;");
    putLine("void* cont;");
    putLine("struct environment* cont_env;");
    decIndent();
    putLine("};");
    putBlankLine();

    // Define the cell struct.
    // putLine("struct cell {");
    // incIndent();
    // if (disableConcurrency) {
    //   putLine("int ref_count;");
    // } else {
    //   putLine("pthread_mutex_t mutex;");
    //   putLine("atomic_int ref_count;");
    // }
    // putLine("char* record;");
    // decIndent();
    // putLine("};");
    // putBlankLine();

    // Utility macros for accessing records and exponentials on a given environment, and on
    // exponentials.
    put("#define RECORD(env, rec) (*(char**)(");
    put("(char*)(env) + ");
    put("sizeof(struct environment) + ");
    put("sizeof(char*) * (rec)");
    put("))");
    putLineEnd();
    putLine("#define ACCESS(rec, offset, type) (*(type*)((rec) + (offset)))");
    put("#define EXPONENTIAL(env, rec_count, exp) (*(struct exponential**)(");
    put("(char*)(env) + ");
    put("sizeof(struct environment) + ");
    put("sizeof(char*) * (rec_count) + ");
    put("sizeof(struct exponential*) * (exp)");
    put("))");
    putLineEnd();
    put("#define TYPE(env, rec_count, exp_count, type_i) (*(struct type*)(");
    put("(char*)(env) + ");
    put("sizeof(struct environment) + ");
    put("sizeof(char*) * (rec_count) + ");
    put("sizeof(struct exponential*) * (exp_count) + ");
    put("sizeof(struct type) * (type_i)");
    put("))");
    putLineEnd();
    putBlankLine();

    // Utility macros for accessing exponential arguments and the template exponential env
    put("#define EXPONENTIAL_ARG(var, exp_i) (*(struct exponential**)(");
    put("(char*)(var) + ");
    put("sizeof(struct exponential) + ");
    put("sizeof(struct exponential*) * (exp_i)");
    put("))");
    putLineEnd();
    put("#define EXPONENTIAL_ENV_TEMPLATE(var, exp_count) ((struct environment*)(");
    put("(char*)(var) + ");
    put("sizeof(struct exponential) + ");
    put("sizeof(struct exponential*) * (exp_count)");
    put("))");
    putLineEnd();
    putBlankLine();

    // Utility macros to for handling alignment and padding
    putLine("#define ALIGN(offset, alignment) ((offset) + ((alignment) - 1) & -(alignment))");
    putLine("#define PADDING(offset, alignment) (-(offset) & ((alignment) - 1))");
    putBlankLine();

    // Utility macro for getting the maximum value of two values.
    putLine("#define MAX(a, b) ((a) > (b) ? (a) : (b))");
    putBlankLine();

    // Function used to decrement the reference counts of all exponential arguments of a given
    // exponential.
    putLine("void dec_ref_arg_exponentials(struct exponential* exp) {");
    incIndent();
    putLine("for (int i = 0; i < " + exponentialEnvExponentialCount("exp") + "; ++i) {");
    incIndent();
    putIf(
        exponentialArgExponential("exp", "i") + " != NULL",
        () -> {
          putDecrementExponentialRefCount(exponentialArgExponential("exp", "i"));
        });
    decIndent();
    putLine("}");
    decIndent();
    putLine("}");
    putBlankLine();

    // Functions used for printing debug info
    if (debug) {
      putLine("void debug_type(int i, struct type* type) {");
      incIndent();
      putDebug("    <type %d:", "i");
      putDebug(" flags(%d)", typeFlags("(*type)"));
      putDebug(" size(%d)", typeSize("(*type)").toString());
      putDebugLn(" alignment(%d)>", typeAlignment("(*type)").toString());
      decIndent();
      putLine("}");
      putBlankLine();

      putLine("void debug_record(int i, char* record) {");
      incIndent();
      putDebugLn("    <record %d: %p>", "i", "record");
      decIndent();
      putLine("}");
      putBlankLine();

      putLine("void debug_exponential(int i, struct exponential* exponential) {");
      incIndent();
      putDebug("    <exponential %d: %p>", "i", "exponential");
      putIfElse(
          "exponential != NULL",
          () -> putDebugLn(" <ref_count: %d>", exponentialRefCount("exponential")),
          () -> putDebugLn(""));
      decIndent();
      putLine("}");
      putBlankLine();
    }

    // Functions used for operations on string expressions.
    putLine("char* string_create(const char* str) {");
    incIndent();
    putLine("char* clone = managed_alloc(strlen(str) + 1);");
    putStatement("strcpy(clone, str)");
    if (profile) {
      putStatement("string_allocs += 1");
    }
    putStatement("return clone");
    decIndent();
    putLine("}");
    putBlankLine();
    putLine("void string_drop(char* str) {");
    incIndent();
    putStatement("managed_free(str)");
    if (profile) {
      putStatement("string_frees += 1");
    }
    decIndent();
    putLine("}");
    putBlankLine();
    putLine("char* string_concat(char* str1, char* str2) {");
    incIndent();
    putStatement("char* concat = managed_alloc(strlen(str1) + strlen(str2) + 1)");
    putStatement("strcpy(concat, str1)");
    putStatement("strcat(concat, str2)");
    putStatement("managed_free(str1)");
    putStatement("managed_free(str2)");
    if (profile) {
      putStatement("string_allocs += 1");
      putStatement("string_frees += 2");
    }
    putStatement("return concat");
    decIndent();
    putLine("}");
    putBlankLine();
    putLine("void string_print(const char* fmt, char* str) {");
    incIndent();
    putStatement("printf(fmt, str)");
    putStatement("managed_free(str)");
    if (profile) {
      putStatement("string_frees += 1");
    }
    decIndent();
    putLine("}");
    putBlankLine();
    putLine("char* string_from_int(int value) {");
    incIndent();
    putStatement("char* str = managed_alloc(12)");
    putStatement("sprintf(str, \"%d\", value)");
    if (profile) {
      putStatement("string_allocs += 1");
    }
    putStatement("return str");
    decIndent();
    putLine("}");
    putBlankLine();
    putLine("int string_equal(char* str1, char* str2) {");
    incIndent();
    putStatement("int result = strcmp(str1, str2) == 0");
    putStatement("managed_free(str1)");
    putStatement("managed_free(str2)");
    if (profile) {
      putStatement("string_frees += 2");
    }
    putStatement("return result");
    decIndent();
    putLine("}");
    putBlankLine();

    // Functions used for reading primitives from the standard input.
    putLine("int int_scan() {");
    incIndent();
    putLine("int value;");
    putIf(
        "scanf(\"%d\", &value) == 1",
        () -> {
          putStatement("return value");
        });
    putStatement("return 0");
    decIndent();
    putLine("}");
    putBlankLine();

    putLine("int bool_scan() {");
    incIndent();
    putLine("char buffer[6];");
    putIf(
        "scanf(\"%5s\", buffer) == 1",
        () -> {
          putStatement("return strcmp(buffer, \"true\") == 0");
        });
    putStatement("return 0");
    decIndent();
    putLine("}");
    putBlankLine();

    putLine("char* string_scan() {");
    incIndent();
    putLine("char buffer[256];");
    putLine("char c;");
    putLine("int i = 0;");
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
    putStatement("return string_create(buffer)");
    decIndent();
    putLine("}");
    putBlankLine();

    // Utility function for sleeping a given number of milliseconds.
    putLine("void sleep_msecs(int msecs) {");
    incIndent();
    putStatement("struct timespec ts");
    putStatement("ts.tv_sec = msecs / 1000");
    putStatement("ts.tv_nsec = (msecs % 1000) * 1000000");
    putStatement("nanosleep(&ts, NULL)");
    decIndent();
    putLine("}");
    putBlankLine();

    // Utility function for atomically setting an integer to the maximum of its current value and a
    // given value.
    if (!disableConcurrency) {
      putLine("void atomic_store_max(atomic_ulong* value, unsigned long new_value) {");
      incIndent();
      putStatement("unsigned long old_value = atomic_load(value)");
      putWhile(
          "new_value > old_value",
          () -> {
            putIf(
                "atomic_compare_exchange_weak(value, &old_value, new_value)",
                () -> {
                  putStatement("break");
                });
          });
      decIndent();
      putLine("}");
      putBlankLine();
    }

    // Execution function.
    putLine("void* thread(void* entry);");
    putBlankLine();
    putLine("void executor(struct task* entry) {");
    incIndent();

    // Define registers.
    putStatement("struct task* " + TASK);
    putStatement("struct task* " + TMP_TASK);
    putStatement("register struct environment* " + ENV);
    putStatement("register struct environment* " + TMP_ENV);
    putStatement("register void* " + TMP_CONT);
    putStatement("register char* " + TMP_RECORD);
    putStatement("register struct exponential* " + TMP_EXPONENTIAL);
    if (!disableConcurrency) {
      putStatement("pthread_t " + TMP_THREAD);
    }
    putStatement("struct cell* " + TMP_CELL);
    putStatement("int " + TMP_INT);
    putBlankLine();

    // Initialize the task list.
    putAllocTask(TASK);
    putAssign(taskCont(TASK), labelAddress("end"));
    putBlankLine();

    // Jump to the entry process.
    putIfElse(
        "entry == NULL",
        () -> {
          if (!ir.getProcesses().containsKey(entryProcess)) {
            throw new RuntimeException("Entry process not found: " + entryProcess);
          }
          if (ir.getProcesses().get(entryProcess).hasArguments()) {
            throw new RuntimeException("Entry process cannot have arguments: " + entryProcess);
          }
          visitInstruction(
              new IRCallProcess(
                  entryProcess, new ArrayList<>(), new ArrayList<>(), new ArrayList<>()));
        },
        () -> {
          putAssign(ENV, taskContEnv("entry"));
          putAssign(TMP_CONT, taskCont("entry"));
          putFreeTask("entry");
          putComputedGoto(TMP_CONT);
        });

    // Generate code for each process.
    for (Map.Entry<String, IRProcess> procEntry : ir.getProcesses().entrySet()) {
      typeCount = procEntry.getValue().getTypeVariableCount();
      recordCount = procEntry.getValue().getRecordCount();
      exponentialCount = procEntry.getValue().getExponentialCount();

      String label = "proc_" + procEntry.getKey();
      putBlankLine();
      putLabel(label);
      visitBlock(procEntry.getKey(), procEntry.getValue().getEntry());

      for (IRBlock block : procEntry.getValue().getBlocks()) {
        label = "block_" + procEntry.getKey() + "_" + block.getLabel();
        putLabel(label);
        visitBlock(procEntry.getKey(), block);
      }
    }

    putBlankLine();
    putLabel("end");
    if (!disableConcurrency) {
      putStatement("pthread_mutex_lock(&thread_stops_mutex)");
      putIncrementAtomic("thread_stops");
      putStatement("pthread_cond_signal(&thread_stops_cond_var)");
      putStatement("pthread_mutex_unlock(&thread_stops_mutex)");
    }

    decIndent();
    putLine("}");
    putBlankLine();
    putLine("void* thread(void* entry) {");
    incIndent();
    putStatement("executor((struct task*)entry)");
    putLine("return NULL;");
    decIndent();
    putLine("}");
    putBlankLine();

    putLine("int main() {");
    incIndent();

    // Start by validating the architecture
    for (CArchitecture.Test test : arch.getTests()) {
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
            putStatement("return 1");
          });
    }
    putBlankLine();

    if (customAllocatorLevels > 0) {
      putLine("for (int i = 0; i < " + customAllocatorLevels + "; ++i) {");
      incIndent();
      putAssign("allocator_list[i]", "NULL");
      if (!disableConcurrency) {
        putStatement("pthread_mutex_init(&allocator_mutex[i], NULL)");
      }
      decIndent();
      putLine("}");
    }
    if (!disableConcurrency) {
      putStatement("pthread_cond_init(&thread_stops_cond_var, NULL)");
      putStatement("pthread_mutex_init(&thread_stops_mutex, NULL)");
    }
    putBlankLine();
    putStatement("thread(NULL)");
    putBlankLine();
    if (!disableConcurrency) {
      putStatement("pthread_mutex_lock(&thread_stops_mutex)");
      putWhile(
          "thread_stops != thread_inits",
          () -> {
            putStatement("pthread_cond_wait(&thread_stops_cond_var, &thread_stops_mutex)");
          });
      putStatement("pthread_mutex_unlock(&thread_stops_mutex)");
      putStatement("pthread_mutex_destroy(&thread_stops_mutex)");
      putStatement("pthread_cond_destroy(&thread_stops_cond_var)");
      for (int i = 0; i < customAllocatorLevels; ++i) {
        putStatement("pthread_mutex_destroy(&allocator_mutex[" + i + "])");
      }
      putBlankLine();
    }
    if (profile) {
      putDebugLn("Profiling results:");
      if (!disableConcurrency) {
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
            putLine("return 1;");
          });
      putIf(
          "record_allocs != record_frees",
          () -> {
            putDebugLn("Record leak detected!");
            putLine("return 1;");
          });
      putIf(
          "exponential_allocs != exponential_frees",
          () -> {
            putDebugLn("Exponential leak detected!");
            putLine("return 1;");
          });
      putIf(
          "task_allocs != task_frees",
          () -> {
            putDebugLn("Task leak detected!");
            putLine("return 1;");
          });
      putIf(
          "string_allocs != string_frees",
          () -> {
            putDebugLn("String leak detected!");
            putLine("return 1;");
          });
      putIf(
          "cell_allocs != cell_frees",
          () -> {
            putDebugLn("Cell leak detected!");
            putLine("return 1;");
          });
    }

    putLine("return 0;");
    decIndent();
    putLine("}");

    return code;
  }

  // ================================= IR instruction visitors ==================================

  private void visitBlock(String procName, IRBlock block) {
    this.procName = procName;

    for (IRInstruction instruction : block.getInstructions()) {
      visitInstruction(instruction);
    }
  }

  private void visitInstruction(IRInstruction instruction) {
    if (debug) {
      putDebugLn("    <proc: " + procName + "> <env: %p>", ENV);
      for (int i = 0; i < typeCount; ++i) {
        putStatement("debug_type(" + i + ", &" + type(i) + ")");
      }
      for (int i = 0; i < recordCount; ++i) {
        putStatement("debug_record(" + i + ", " + record(i) + ")");
      }
      for (int i = 0; i < exponentialCount; ++i) {
        putStatement("debug_exponential(" + i + ", " + exponential(i) + ")");
      }
    }
    if (trace) {
      putDebugLn(instruction.toString());
    } else {
      putLine("/* " + instruction.toString() + " */");
    }
    instruction.accept(this);
  }

  @Override
  public void visit(IRInstruction instruction) {
    throw new UnsupportedOperationException(
        "Unsupported instruction type: " + instruction.getClass().getName());
  }

  @Override
  public void visit(IRCallProcess instruction) {
    IRProcess process = ir.getProcesses().get(instruction.getProcessName());

    Runnable notATailCall =
        () -> {
          if (instruction.getLinearArguments().isEmpty()
              && instruction.getExponentialArguments().isEmpty()
              && instruction.getTypeArguments().isEmpty()) {
            if (!entryCall) {
              putDecrementEndPoints(instruction.isEndPoint(), () -> putFreeEnvironment(ENV));
            } else {
              entryCall = false;
            }
            putAllocEnvironment(ENV, instruction.getProcessName());
          } else {
            putAllocEnvironment(TMP_ENV, instruction.getProcessName());

            // Bind the arguments to the new environment
            for (LinearArgument arg : instruction.getLinearArguments()) {
              putAssign(record(TMP_ENV, arg.getTargetRecord()), record(ENV, arg.getSourceRecord()));
              putConsumeRecord(record(ENV, arg.getSourceRecord()), arg.getRecordType());
            }
            for (ExponentialArgument arg : instruction.getExponentialArguments()) {
              putAssign(
                  exponential(TMP_ENV, process.getRecordCount(), arg.getTargetExponential()),
                  exponential(ENV, recordCount, arg.getSourceExponential()));
            }
            for (TypeArgument arg : instruction.getTypeArguments()) {
              putAssign(
                  type(
                      TMP_ENV,
                      process.getRecordCount(),
                      process.getExponentialCount(),
                      arg.getTargetType()),
                  typeInitializer(arg.getSourceType(), arg.getSourceTypePolarity()));
            }

            putDecrementEndPoints(instruction.isEndPoint(), () -> putFreeEnvironment(ENV));
            putAssign(ENV, TMP_ENV);
          }
        };

    // Check if the type arguments are the same as the types in the current environment.
    // That condition is necessary to perform a tail call.
    boolean sameTypes = true;
    for (TypeArgument arg : instruction.getTypeArguments()) {
      if (!(arg.getSourceType() instanceof IRVarT)) {
        sameTypes = false;
        break;
      }

      IRVarT var = (IRVarT) arg.getSourceType();
      if (var.getType() != arg.getTargetType()) {
        sameTypes = false;
        break;
      }
    }

    // If we're recursively calling the same process, we might be able to do tail call optimization.
    if (optimizeTailCalls
        && instruction.getProcessName().equals(procName)
        && sameTypes
        && instruction.isEndPoint()) {
      // We must increment the end points as we might decrement them twice, if this ends up not
      // being a tail call.
      putStatement(incrementAtomic(environmentEndPoints()));
      putIfElse(
          decrementAtomic(environmentEndPoints()) + " == 1",
          () -> {
            // Instead of allocating a new environment, we'll just reuse the current one.
            // We need this map to store the record and exponential swaps that we need to do.
            Map<Integer, Integer> recordMap = new HashMap<>();
            Map<Integer, Integer> exponentialMap = new HashMap<>();

            for (LinearArgument arg : instruction.getLinearArguments()) {
              // If the binding is the same, we don't need to do anything.
              int sourceRecord =
                  recordMap.getOrDefault(arg.getSourceRecord(), arg.getSourceRecord());
              if (sourceRecord != arg.getTargetRecord()) {
                recordMap.put(arg.getTargetRecord(), sourceRecord);

                // Swap the records.
                putAssign(TMP_RECORD, record(arg.getTargetRecord()));
                putAssign(record(arg.getTargetRecord()), record(sourceRecord));
                putAssign(record(sourceRecord), TMP_RECORD);
              }
            }
            for (ExponentialArgument arg : instruction.getExponentialArguments()) {
              // If the binding is the same, we don't need to do anything.
              int sourceExponential =
                  exponentialMap.getOrDefault(
                      arg.getSourceExponential(), arg.getSourceExponential());
              if (sourceExponential != arg.getTargetExponential()) {
                exponentialMap.put(arg.getTargetExponential(), sourceExponential);

                // Swap the exponentials.
                putAssign(TMP_EXPONENTIAL, exponential(arg.getTargetExponential()));
                putAssign(exponential(arg.getTargetExponential()), exponential(sourceExponential));
                putAssign(exponential(sourceExponential), TMP_EXPONENTIAL);
              }
            }

            if (trace && procName != null) {
              putDebugLn("[tailCall(" + procName + ")]");
            }
          },
          notATailCall);
    } else {
      notATailCall.run();
    }

    putAssign(environmentEndPoints(), process.getEndPoints());
    putConstantGoto("proc_" + instruction.getProcessName());
  }

  @Override
  public void visit(IRCallLoop instruction) {
    Set<Integer> sourceRecords = new HashSet<>();
    Set<Integer> sourceExponentials = new HashSet<>();
    for (IRCallLoop.LinearArgument arg : instruction.getLinearArguments()) {
      sourceRecords.add(arg.getSourceRecord());
    }
    for (IRCallLoop.ExponentialArgument arg : instruction.getExponentialArguments()) {
      sourceExponentials.add(arg.getSourceExponential());
    }

    // We need this map to store the record and exponential swaps that we need to do.
    Map<Integer, Integer> recordMap = new HashMap<>();
    Map<Integer, Integer> exponentialMap = new HashMap<>();

    for (IRCallLoop.LinearArgument arg : instruction.getLinearArguments()) {
      sourceRecords.remove(arg.getSourceRecord());

      // If the binding is the same, we don't need to do anything.
      int sourceRecord = recordMap.getOrDefault(arg.getSourceRecord(), arg.getSourceRecord());
      if (sourceRecord != arg.getTargetRecord()) {
        if (sourceRecords.contains(arg.getTargetRecord())) {
          // If the target record will also be passed as an argument, we use a swap instead of a
          // direct assignment
          putAssign(TMP_RECORD, record(arg.getTargetRecord()));
          putAssign(record(arg.getTargetRecord()), record(sourceRecord));
          putAssign(record(sourceRecord), TMP_RECORD);
          recordMap.put(arg.getTargetRecord(), sourceRecord);
        } else {
          putAssign(record(arg.getTargetRecord()), record(sourceRecord));
        }
      }
    }
    for (IRCallLoop.ExponentialArgument arg : instruction.getExponentialArguments()) {
      sourceExponentials.remove(arg.getSourceExponential());

      // If the binding is the same, we don't need to do anything.
      int sourceExponential =
          exponentialMap.getOrDefault(arg.getSourceExponential(), arg.getSourceExponential());
      if (sourceExponential != arg.getTargetExponential()) {
        if (sourceExponentials.contains(arg.getTargetExponential())) {
          // Swap the exponentials.
          putAssign(TMP_EXPONENTIAL, exponential(arg.getTargetExponential()));
          putAssign(exponential(arg.getTargetExponential()), exponential(sourceExponential));
          putAssign(exponential(sourceExponential), TMP_EXPONENTIAL);
          exponentialMap.put(arg.getTargetExponential(), sourceExponential);
        } else {
          putAssign(exponential(arg.getTargetExponential()), exponential(sourceExponential));
        }
      }
    }

    putConstantGoto(blockLabel(instruction.getEntryLabel()));
  }

  @Override
  public void visit(IRForward i) {
    // Copy the buffer from the negative record to the positive record.
    String offset = " - (" + lastSlotOffset(i.getType()) + ")";
    CSize size = layout(i.getType()).size;
    putCopy(record(i.getPosRecord()) + offset, record(i.getNegRecord()) + offset, size);

    // Store the continuation we'll be jumping to
    putConsumeRecord(i.getPosRecord(), i.getType());
    String posHeader = accessRecord(i.getPosRecord(), "struct record_header");
    if (i.shouldReturn()) {
      putAssign(TMP_CONT, recordHeaderCont(posHeader));
      putAssign(TMP_ENV, recordHeaderContEnv(posHeader));
    }

    // Store the record we'll be freeing
    putAssign(TMP_RECORD, record(i.getNegRecord()));
    putConsumeRecord(TMP_RECORD, i.getType());

    // If the negative record has a continuation (i.e., type is not a value), we must handle it
    putIfTypeIsNotValue(
        i.getType().valueRequisites(),
        () -> {
          // Set the continuation of the positive record to the continuation of the negative record.
          String negHeader = accessRecord(TMP_RECORD, "0", "struct record_header");
          putAssign(recordHeaderCont(posHeader), recordHeaderCont(negHeader));
          putAssign(recordHeaderContEnv(posHeader), recordHeaderContEnv(negHeader));
          putAssign(recordHeaderContRecord(posHeader), recordHeaderContRecord(negHeader));

          // Overwrite the negative record on its continuation environment with the positive record.
          putAssign(
              record(recordHeaderContEnv(negHeader), recordHeaderContRecord(negHeader)),
              record(i.getPosRecord()));
        });

    // Decrement the end points and free the environment if necessary.
    putDecrementEndPoints(i.isEndPoint(), () -> putFreeEnvironment(ENV));

    // Finally, free the negative record and jump to the continuation.
    putFreeRecord(TMP_RECORD);
    if (i.shouldReturn()) {
      putAssign(ENV, TMP_ENV);
      putComputedGoto(TMP_CONT);
    }
  }

  @Override
  public void visit(IRFlipForward i) {
    throw new UnsupportedOperationException("Unimplemented");

    // // First of all, we need to check if we need to, and if so, expand the buffer of the negative
    // // record.
    // // This is a pessimistic check, as we don't know the actual size of the buffer at runtime.
    // String currentSize = maxSize(i.getType());
    // String desiredSize =
    //     currentSize + " + " + written(i.getPosRecord()) + " - " + read(i.getPosRecord());
    // putIf(
    //     currentSize + " < " + desiredSize,
    //     () -> {
    //       putReallocRecord(record(i.getNegRecord()), desiredSize);
    //     });

    // // Copy the unread data on the positive buffer into the negative record's buffer.
    // putStatement(
    //     "memcpy("
    //         + buffer(i.getNegRecord())
    //         + " + "
    //         + written(i.getNegRecord())
    //         + ", "
    //         + buffer(i.getPosRecord())
    //         + " + "
    //         + read(i.getPosRecord())
    //         + ", "
    //         + written(i.getPosRecord())
    //         + " - "
    //         + read(i.getPosRecord())
    //         + ")");
    // putStatement(
    //     written(i.getNegRecord())
    //         + " += "
    //         + written(i.getPosRecord())
    //         + " - "
    //         + read(i.getPosRecord()));

    // // Set the continuation of the negative record to the continuation of the positive record.
    // putAssign(TMP_CONT, recordCont(i.getNegRecord()));
    // putAssign(TMP_ENV, recordContEnv(i.getNegRecord()));
    // putAssign(recordCont(i.getNegRecord()), recordCont(i.getPosRecord()));
    // putAssign(recordContEnv(i.getNegRecord()), recordContEnv(i.getPosRecord()));
    // putAssign(recordContRecord(i.getNegRecord()), recordContRecord(i.getPosRecord()));

    // // Overwrite the positive record on its continuation environment with the negative record.
    // // We only do this if the positive record has a continuation environment.
    // putAssign(TMP_RECORD, record(i.getPosRecord()));
    // putIf(
    //     recordContEnv(i.getPosRecord()) + " != NULL",
    //     () -> {
    //       putAssign(
    //           record(recordContEnv(i.getPosRecord()), recordContRecord(i.getPosRecord())),
    //           record(i.getNegRecord()));
    //     });

    // // Decrement the end points and free the environment if necessary.
    // putDecrementEndPoints(
    //     i.isEndPoint(),
    //     () -> putFreeEnvironment(ENV),
    //     () -> {
    //       // If the environment was not freed, and if the records won't be used in this
    // environment
    //       // anymore, we need to remove their bindings. This is necessary to prevent them from
    // being
    //       // cloned or freed again later on.

    //       // The positive record will be deleted, but it's binding may now point to the negative
    //       // record. If it doesn't, then we set it to null.
    //       putIf(
    //           TMP_RECORD + " == " + record(i.getPosRecord()),
    //           () -> {
    //             putAssign(record(i.getPosRecord()), "NULL");
    //           });

    //       // The negative record will only be needed if either the environment we're going to
    // jump
    //       // to or its continuation environment are the current environment. If not, we also need
    // to
    //       // remove its binding from the current environment.
    //       putIf(
    //           ENV + " != " + recordContEnv(i.getNegRecord()) + " && " + ENV + " != " + TMP_ENV,
    //           () -> {
    //             putAssign(record(i.getNegRecord()), "NULL");
    //           });
    //     });

    // // Finally, free the positive record and jump to the continuation.
    // putFreeRecord(TMP_RECORD);
    // putAssign(ENV, TMP_ENV);
    // putComputedGoto(TMP_CONT);
  }

  @Override
  public void visit(IRResetSession i) {
    putResetRecord(i.getRecord(), i.getRecordType());
  }

  @Override
  public void visit(IRFlip i) {
    String header = accessRecord(i.getRecord(), "struct record_header");

    putAssign(TMP_CONT, recordHeaderCont(header));
    putAssign(TMP_ENV, recordHeaderContEnv(header));

    putAssign(recordHeaderCont(header), labelAddress(blockLabel(i.getContLabel())));
    putAssign(recordHeaderContEnv(header), ENV);
    putAssign(recordHeaderContRecord(header), i.getRecord());

    putAssign(ENV, TMP_ENV);
    putComputedGoto(TMP_CONT);
  }

  @Override
  public void visit(IRReturn i) {
    String header = accessRecord(i.getRecord(), "struct record_header");

    putAssign(TMP_CONT, recordHeaderCont(header));
    putAssign(TMP_ENV, recordHeaderContEnv(header));

    putDecrementEndPoints(i.isEndPoint(), () -> putFreeEnvironment(ENV));

    putAssign(ENV, TMP_ENV);
    putComputedGoto(TMP_CONT);
  }

  @Override
  public void visit(IRPopSession instruction) {
    if (instruction.getRecordType() instanceof IRSessionT == false) {
      throw new UnsupportedOperationException("Record must be of session type");
    }
    IRSessionT sessionType = (IRSessionT) instruction.getRecordType();

    putSwitchTypeIsValue(
        sessionType.getArg().valueRequisites(),
        () -> {
          putAllocRecord(record(instruction.getArgRecord()), instruction.getArgRecordType());
          putResetRecord(instruction.getArgRecord(), instruction.getArgRecordType());

          putPopValue(
              record(instruction.getRecord()),
              sessionType,
              record(instruction.getArgRecord()),
              instruction.getArgRecordType());
        },
        () -> {
          putAssign(
              record(instruction.getArgRecord()), accessRecord(instruction.getRecord(), "char*"));
          putAdvanceRecord(instruction.getRecord(), instruction.getRecordType());
        });
  }

  @Override
  public void visit(IRPushSession instruction) {
    if (instruction.getRecordType() instanceof IRSessionT == false) {
      throw new UnsupportedOperationException("Record must be of session type");
    }
    IRSessionT sessionType = (IRSessionT) instruction.getRecordType();

    putSwitchTypeIsValue(
        sessionType.getArg().valueRequisites(),
        () -> {
          putPushValue(
              record(instruction.getRecord()),
              sessionType,
              record(instruction.getArgRecord()),
              instruction.getArgRecordType());
          putFreeRecord(record(instruction.getArgRecord()));
        },
        () -> {
          putAssign(
              accessRecord(instruction.getRecord(), "char*"), record(instruction.getArgRecord()));
          putAdvanceRecord(instruction.getRecord(), instruction.getRecordType());
          putConsumeRecord(instruction.getArgRecord(), instruction.getArgRecordType());
        });
  }

  @Override
  public void visit(IRPopTag instruction) {
    if (!(instruction.getRecordType() instanceof IRTagT)) {
      throw new UnsupportedOperationException("Record must be of tag type");
    }
    IRTagT type = (IRTagT) instruction.getRecordType();

    putLine("switch (" + accessRecord(instruction.getRecord(), "unsigned char") + ") {");
    incIndent();

    // We'll take the end points of all other cases for each case, since these paths won't be taken
    Integer maxEndPoints = 0;
    for (Map.Entry<Integer, IRPopTag.Case> entry : instruction.getCases().entrySet()) {
      maxEndPoints = Math.max(maxEndPoints, entry.getValue().getEndPoints());
    }

    for (Map.Entry<Integer, IRPopTag.Case> entry : instruction.getCases().entrySet()) {
      putLine("case " + entry.getKey() + ":");
      incIndent();

      // Decrement end points depending on the chosen branch.
      putAssign(
          environmentEndPoints(),
          environmentEndPoints() + " - " + (maxEndPoints - entry.getValue().getEndPoints()));

      // Advance the record depending on the chosen branch.
      putAdvanceRecordWithTag(instruction.getRecord(), type, entry.getKey());

      // Jump to the right place.
      putConstantGoto(blockLabel(entry.getValue().getLabel()));
      decIndent();
    }

    decIndent();
    putLine("}");
  }

  @Override
  public void visit(IRPushTag instruction) {
    if (!(instruction.getRecordType() instanceof IRTagT)) {
      throw new UnsupportedOperationException("Record must be of tag type");
    }
    IRTagT type = (IRTagT) instruction.getRecordType();

    putAssign(accessRecord(instruction.getRecord(), "unsigned char"), instruction.getTag());
    putAdvanceRecordWithTag(instruction.getRecord(), type, instruction.getTag());
  }

  @Override
  public void visit(IRPopClose instruction) {}

  @Override
  public void visit(IRPushClose instruction) {}

  @Override
  public void visit(IRNewSession instruction) {
    putAllocRecord(record(instruction.getRecord()), instruction.getType());

    // If the record has a continuation, write it to the beginning of the buffer
    if (instruction.getLabel().isPresent()) {
      StringBuilder value = new StringBuilder("(struct record_header) {");
      value
          .append(".cont = ")
          .append(labelAddress(blockLabel(instruction.getLabel().get())))
          .append(", ");
      value.append(".cont_env = ").append(ENV).append(", ");
      value.append(".cont_record = ").append(instruction.getRecord());
      value.append("}");
      putAssign(accessRecord(instruction.getRecord(), "struct record_header"), value.toString());
    }
  }

  @Override
  public void visit(IRFreeSession instruction) {
    // The cursor will already be on the header of the record, since all pop instructions will
    // have been executed.
    putFreeRecord(record(instruction.getRecord()));
  }

  @Override
  public void visit(IRNextTask instruction) {
    putDecrementEndPoints(instruction.isEndPoint(), () -> putFreeEnvironment(ENV));
    putAssign(TMP_TASK, TASK);
    putAssign(TASK, taskNext(TASK));
    putAssign(TMP_CONT, taskCont(TMP_TASK));
    putAssign(ENV, taskContEnv(TMP_TASK));
    putFreeTask(TMP_TASK);
    putComputedGoto(TMP_CONT);
  }

  @Override
  public void visit(IRNewTask instruction) {
    putAssign(TMP_TASK, TASK);
    putAllocTask(TASK);
    putAssign(taskNext(TASK), TMP_TASK);
    putAssign(taskCont(TASK), labelAddress(blockLabel(instruction.getLabel())));
    putAssign(taskContEnv(TASK), ENV);
  }

  @Override
  public void visit(IRNewThread instruction) {
    if (disableConcurrency) {
      new IRNewTask(instruction.getLabel()).accept(this);
    } else {
      putAllocTask(TMP_TASK);
      putAssign(taskCont(TMP_TASK), labelAddress(blockLabel(instruction.getLabel())));
      putAssign(taskContEnv(TMP_TASK), ENV);
      putIncrementAtomic("thread_inits");
      putStatement("pthread_create(&" + TMP_THREAD + ", NULL, thread, (void*)" + TMP_TASK + ")");
    }
  }

  @Override
  public void visit(IRJump instruction) {
    putConstantGoto(blockLabel(instruction.getLabel()));
  }

  @Override
  public void visit(IRBranch instruction) {
    int maxEndPoints = instruction.getEndPoints();
    putIfElse(
        expression(instruction.getExpression()),
        () -> {
          putAssign(
              environmentEndPoints(),
              environmentEndPoints()
                  + " - "
                  + (maxEndPoints - instruction.getThen().getEndPoints()));
          putConstantGoto(blockLabel(instruction.getThen().getLabel()));
        },
        () -> {
          putAssign(
              environmentEndPoints(),
              environmentEndPoints()
                  + " - "
                  + (maxEndPoints - instruction.getOtherwise().getEndPoints()));
          putConstantGoto(blockLabel(instruction.getOtherwise().getLabel()));
        });
  }

  @Override
  public void visit(IRPrint instruction) {
    generatePrint(instruction.getExpression(), instruction.hasNewLine());
  }

  @Override
  public void visit(IRPushScan instruction) {
    IRType irType = instruction.getRecordType().leftmostTail();
    boolean promote = false;
    if (irType instanceof IRExponentialT) {
      irType = ((IRExponentialT) irType).getInner();
      promote = true;
    }

    String cType = cType(irType);
    String cValue;

    if (irType instanceof IRIntT) {
      cValue = "int_scan()";
    } else if (irType instanceof IRBoolT) {
      cValue = "bool_scan()";
    } else if (irType instanceof IRStringT) {
      cValue = "string_scan()";
    } else {
      throw new UnsupportedOperationException(
          "Unsupported type for IRScan: " + instruction.getRecordType().getClass().getName());
    }

    if (promote && (!optimizePrimitiveExponentials || irType instanceof IRStringT)) {
      throw new UnsupportedOperationException("Exponentials are not supported");
    } else {
      putAssign(accessRecord(instruction.getRecord(), cType), cValue);
      putAdvanceRecord(instruction.getRecord(), instruction.getRecordType());
    }
  }

  @Override
  public void visit(IRPushExpression instruction) {
    if (instruction.isExponential()
        && (!optimizePrimitiveExponentials
            || instruction.getExpression().getType() instanceof IRStringT)) {
      throw new UnsupportedOperationException("Exponentials are not supported");
    } else {
      putAssign(
          accessRecord(instruction.getRecord(), cType(instruction.getExpression().getType())),
          expression(instruction.getExpression()));
      putAdvanceRecord(instruction.getRecord(), instruction.getRecordType());
    }
  }

  @Override
  public void visit(IRPushType instruction) {
    StringBuilder value = new StringBuilder("(struct type_slot) {");
    value.append(".record = ").append(record(instruction.getContRecord())).append(", ");
    value
        .append(".type = ")
        .append(typeInitializer(instruction.getArgType(), instruction.isArgPositive()));
    value.append("}");
    putAssign(accessRecord(instruction.getRecord(), "struct type_slot"), value.toString());
    putAdvanceRecord(instruction.getRecord(), instruction.getRecordType());
    putConsumeRecord(instruction.getContRecord(), instruction.getContRecordType());
  }

  @Override
  public void visit(IRPopType instruction) {
    String typeSlot = accessRecord(instruction.getRecord(), "struct type_slot");
    putAssign(record(instruction.getArgRecord()), typeSlot + ".record");
    putAssign(type(instruction.getArgType()), typeSlot + ".type");
    putAdvanceRecord(instruction.getRecord(), instruction.getRecordType());

    if (instruction.getPositive().isPresent() || instruction.getNegative().isPresent()) {
      int maxEndPoints = instruction.getEndPoints();
      putIfElse(
          typePolarity(type(instruction.getArgType())),
          () -> {
            if (instruction.getPositive().isPresent()) {
              putAssign(
                  environmentEndPoints(),
                  environmentEndPoints()
                      + " - "
                      + (maxEndPoints - instruction.getPositive().get().getEndPoints()));
              putConstantGoto(blockLabel(instruction.getPositive().get().getLabel()));
            }
          },
          () -> {
            if (instruction.getNegative().isPresent()) {
              putAssign(
                  environmentEndPoints(),
                  environmentEndPoints()
                      + " - "
                      + (maxEndPoints - instruction.getNegative().get().getEndPoints()));
              putConstantGoto(blockLabel(instruction.getNegative().get().getLabel()));
            }
          });
    }
  }

  @Override
  public void visit(IRNewExponentialProcess instruction) {
    IRProcess process = ir.getProcesses().get(instruction.getProcessName());

    putAllocExponential(
        exponential(instruction.getExponential()),
        process.getRecordCount(),
        process.getExponentialCount(),
        process.getTypeVariableCount());
    putAssign(exponentialRefCount(instruction.getExponential()), "1");
    putAssign(exponentialEnvRecordCount(instruction.getExponential()), process.getRecordCount());
    putAssign(
        exponentialEnvExponentialCount(instruction.getExponential()),
        process.getExponentialCount());
    putAssign(
        exponentialEnvTypeCount(instruction.getExponential()), process.getTypeVariableCount());
    putAssign(
        exponentialEntry(instruction.getExponential()),
        labelAddress("proc_" + instruction.getProcessName()));

    // Initialize the argument exponentials
    // These pointers are only kept for reference counting purposes
    for (int i = 0; i < process.getExponentialCount(); ++i) {
      String value = "NULL";
      for (ExponentialArgument arg : instruction.getExponentialArguments()) {
        if (arg.getTargetExponential() == i) {
          value =
              ternaryTypeId(
                  arg.getExponentialType(),
                  "NULL",
                  "NULL",
                  exponential(arg.getSourceExponential()));
          break;
        }
      }
      putAssign(exponentialArgExponential(instruction.getExponential(), i), value);
    }

    // Initialize the environment template with the necessary arguments
    String envTemplate = exponentialEnvTemplate(instruction.getExponential());
    putAssign(environmentEndPoints(envTemplate), process.getEndPoints());
    for (ExponentialArgument arg : instruction.getExponentialArguments()) {
      putAssign(
          exponential(envTemplate, process.getRecordCount(), arg.getTargetExponential()),
          exponential(arg.getSourceExponential()));
    }
    for (TypeArgument arg : instruction.getTypeArguments()) {
      putAssign(
          type(
              envTemplate,
              process.getRecordCount(),
              process.getExponentialCount(),
              arg.getTargetType()),
          typeInitializer(arg.getSourceType(), arg.getSourceTypePolarity()));
    }

    // Runnable ifValue = () -> {
    //   putAllocEnvironment(TMP_ENV, instruction.getProcessName());
    //   String contLabel = makeLabel("new_exponential_process");

    //   // Initialize the linear record
    //   putAllocRecord(record(TMP_ENV, 0), instruction.getType());
    //   StringBuilder value = new StringBuilder("(struct record_header) {");
    //   value
    //       .append(".cont = ")
    //       .append(labelAddress(contLabel))
    //       .append(", ");
    //   value.append(".cont_env = ").append(ENV).append(", ");
    //   value.append("}");
    //   putAssign(accessRecord(TMP_ENV, 0, "0", "struct record_header"), value.toString());
    //   putResetRecord(record(TMP_ENV, 0), instruction.getType());

    //   // Bind the arguments to the new environment
    //   for (ExponentialArgument arg : instruction.getExponentialArguments()) {
    //     putAssign(
    //         exponential(TMP_ENV, process.getRecordCount(), arg.getTargetExponential()),
    //         exponential(arg.getSourceExponential()));
    //   }
    //   for (TypeArgument arg : instruction.getTypeArguments()) {
    //     putAssign(
    //         type(
    //             TMP_ENV,
    //             process.getRecordCount(),
    //             process.getExponentialCount(),
    //             arg.getTargetType()),
    //         typeInitializer(arg.getSourceType(), arg.getSourceTypePolarity()));
    //   }

    //   // Jump to the process
    //   putAssign(ENV, TMP_ENV);
    //   putConstantGoto("proc_" + instruction.getProcessName());

    //   // After returning from the process, we must copy the stored value into the exponential
    //   // The problem is that
    //   putLabel(contLabel);
    // };

  }

  @Override
  public void visit(IRNewExponentialExpression instruction) {
    throw new UnsupportedOperationException("Exponentials are still not supported");
  }

  @Override
  public void visit(IRNewExponentialScan instruction) {
    throw new UnsupportedOperationException("Exponentials are still not supported");
  }

  @Override
  public void visit(IRPushExponential instruction) {
    Runnable forInt =
        () ->
            putAssign(
                accessRecord(instruction.getRecord(), "int"),
                exponentialAsInteger(instruction.getArgExponential()));

    Runnable forBool =
        () ->
            putAssign(
                accessRecord(instruction.getRecord(), "unsigned char"),
                exponentialAsInteger(instruction.getArgExponential()));

    Runnable forOther =
        () ->
            putAssign(
                accessRecord(instruction.getRecord(), "struct exponential*"),
                exponential(instruction.getArgExponential()));

    putSwitchTypeId(instruction.getArgExponentialType(), forInt, forBool, forOther);
    putAdvanceRecord(instruction.getRecord(), instruction.getRecordType());
  }

  @Override
  public void visit(IRPopExponential instruction) {
    String forInt = integerAsExponential(accessRecord(instruction.getRecord(), "int"));
    String forBool = boolAsExponential(accessRecord(instruction.getRecord(), "unsigned char"));
    String forOther = accessRecord(instruction.getRecord(), "struct exponential*");

    String value = ternaryTypeId(instruction.getArgExponentialType(), forInt, forBool, forOther);
    putAssign(exponential(instruction.getArgExponential()), value);
    putAdvanceRecord(instruction.getRecord(), instruction.getRecordType());
  }

  @Override
  public void visit(IRCallExponential instruction) {
    // Start by allocating the new record
    IRType expType = instruction.getExponentialType();
    putAllocRecord(record(instruction.getArgRecord()), expType);

    Runnable forInt = () -> {
      putResetRecord(record(instruction.getArgRecord()), expType);
      putAssign(accessRecord(instruction.getArgRecord(), "int"), exponentialAsInteger(instruction.getExponential()));
    };

    Runnable forBool = () -> {
      putResetRecord(record(instruction.getArgRecord()), expType);
      putAssign(accessRecord(instruction.getArgRecord(), "unsigned char"), exponentialAsBool(instruction.getExponential()));
    };

    Runnable forOther = () -> {
      // Instantiate an environment for the exponential process from its template
      putAllocEnvironment(
          TMP_ENV,
          exponentialEnvRecordCount(instruction.getExponential()),
          exponentialEnvExponentialCount(instruction.getExponential()),
          exponentialEnvTypeCount(instruction.getExponential()));
      putCopy(
          TMP_ENV,
          exponentialEnvTemplate(instruction.getExponential()),
          environmentSize(
              exponentialEnvRecordCount(instruction.getExponential()),
              exponentialEnvExponentialCount(instruction.getExponential()),
              exponentialEnvTypeCount(instruction.getExponential())));

      // Increment the reference count of any argument exponentials
      putLine(
          "for (int i = 0; i < "
              + exponentialEnvExponentialCount(instruction.getExponential())
              + "; ++i) {");
      incIndent();
      putIf(
          exponentialArgExponential(instruction.getExponential(), "i") + " != NULL",
          () ->
              putIncrementAtomic(
                  exponentialRefCount(exponentialArgExponential(instruction.getExponential(), "i"))));
      decIndent();
      putLine("}");

      // If the exponential type is reset, then it means that we'll be writing to it immediately
      // In that case, the continuation should be set to the exponential process
      Runnable ifReset = () -> {
        StringBuilder value = new StringBuilder("(struct record_header) {");
        value.append(".cont = ").append(exponentialEntry(instruction.getExponential())).append(", ");
        value.append(".cont_env = ").append(TMP_ENV).append(", ");
        value.append(".cont_record = 0");
        value.append("}");
        putAssign(accessRecord(instruction.getArgRecord(), "struct record_header"), value.toString());

        putResetRecord(record(instruction.getArgRecord()), ((IRResetT)expType).getCont());
        putAssign(record(TMP_ENV, 0), record(instruction.getArgRecord()));
      };

      // Otherwise, since we'll reading from it, we must set the continuation to the current
      // process and jump to the exponential process immediately
      Runnable ifNotReset = () -> {
        String label = makeLabel("call_exponential_return");

        StringBuilder value = new StringBuilder("(struct record_header) {");
        value.append(".cont = ").append(labelAddress(label)).append(", ");
        value.append(".cont_env = ").append(ENV).append(", ");
        value.append(".cont_record = ").append(instruction.getArgRecord());
        value.append("}");
        putAssign(accessRecord(instruction.getArgRecord(), "struct record_header"), value.toString());

        putResetRecord(record(instruction.getArgRecord()), expType);
        putAssign(record(TMP_ENV, 0), record(instruction.getArgRecord()));

        putAssign(TMP_CONT, exponentialEntry(instruction.getExponential()));
        putAssign(ENV, TMP_ENV);
        putComputedGoto(TMP_CONT);
        putLabel(label);
      };

      putSwitchTypeIsResetAndNotClose(expType, ifReset, ifNotReset);
    };

    putSwitchTypeId(expType, forInt, forBool, forOther);
  }

  @Override
  public void visit(IRIncRefExponential instruction) {
    putIfNotIntOrBool(
        instruction.getType(),
        () -> putIncrementAtomic(exponentialRefCount(instruction.getExponential())));
  }

  @Override
  public void visit(IRDecRefExponential instruction) {
    putDecrementExponentialRefCount(
        exponential(instruction.getExponential()), instruction.getType());
  }

  @Override
  public void visit(IRIncRefCell instruction) {
    throw new UnsupportedOperationException("Cells are still not supported");
  }

  @Override
  public void visit(IRDecRefCell instruction) {
    throw new UnsupportedOperationException("Cells are still not supported");
  }

  @Override
  public void visit(IRPushCell instruction) {
    throw new UnsupportedOperationException("Cells are still not supported");
  }

  @Override
  public void visit(IRTakeCell instruction) {
    throw new UnsupportedOperationException("Cells are still not supported");
  }

  @Override
  public void visit(IRPutCell instruction) {
    throw new UnsupportedOperationException("Cells are still not supported");
  }

  @Override
  public void visit(IRSleep instruction) {
    putStatement("sleep_msecs(" + instruction.getMsecs() + ")");
  }

  @Override
  public void visit(IRPanic instruction) {
    putDebugLn("Panic: " + instruction.getMsg());
    putStatement("exit(1);");
  }

  // =============================== Expression building helpers ================================

  private String taskNext(String task) {
    return task + "->next";
  }

  private String taskCont(String task) {
    return task + "->cont";
  }

  private String taskContEnv(String task) {
    return task + "->cont_env";
  }

  private String environmentEndPoints(String env) {
    return env + "->end_points";
  }

  private String environmentEndPoints() {
    return environmentEndPoints(ENV);
  }

  private String type(String env, int recordCount, int exponentialCount, String type) {
    return "TYPE(" + env + ", " + recordCount + ", " + exponentialCount + ", " + type + ")";
  }

  private String type(String env, int recordCount, int exponentialCount, int type) {
    return type(env, recordCount, exponentialCount, Integer.toString(type));
  }

  private String type(String env, String type) {
    return type(env, recordCount, exponentialCount, type);
  }

  private String type(String env, int type) {
    return type(env, Integer.toString(type));
  }

  private String type(int type) {
    return type(ENV, type);
  }

  private String typeInitializer(IRType type, boolean polarity) {
    String flags = ", .flags = 0";
    flags += " | (" + typeId(type) + ") << 3";
    flags += " | (" + typeIsReset(type) + ") << 2";
    if (optimizeSendValue) {
      flags += " | (" + typeIsValue(type) + ") << 1";
    }
    flags += " | " + (polarity ? "1" : "0");

    LayoutCalculator layout = layout(type);

    return "(struct type){.size = "
        + layout.size
        + flags
        + ", .alignment = "
        + layout.alignment
        + "}";
  }

  private CSize typeSize(String type) {
    return CSize.expression(type + ".size");
  }

  private CAlignment typeAlignment(String type) {
    return CAlignment.expression(type + ".alignment");
  }

  private String typeId(IRType type) {
    if (type instanceof IRVarT) {
      return typeId(type(((IRVarT) type).getType()));
    } else if (type instanceof IRIntT) {
      return Integer.toString(TYPE_ID_INT);
    } else if (type instanceof IRBoolT) {
      return Integer.toString(TYPE_ID_BOOL);
    } else if (type instanceof IRCloseT) {
      return Integer.toString(TYPE_ID_CLOSE);
    } else {
      return Integer.toString(TYPE_ID_OTHER);
    }
  }

  private String typeId(String type) {
    return "((" + type + ".flags >> 3) & 3)";
  }

  private String ternaryTypeId(IRType type, String forInt, String forBool, String forOther) {
    if (optimizePrimitiveExponentials && type instanceof IRIntT) {
      return forInt;
    } else if (optimizePrimitiveExponentials && type instanceof IRBoolT) {
      return forBool;
    } else if (optimizePrimitiveExponentials && type instanceof IRVarT) {
      String typeId = typeId(type(((IRVarT) type).getType()));
      return "("
          + typeId
          + " == "
          + TYPE_ID_INT
          + " ? ("
          + forInt
          + ") : "
          + typeId
          + " ? ("
          + forBool
          + ") : ("
          + forOther
          + "))";
    } else {
      return forOther;
    }
  }

  private CSize ternaryTypeId(IRType type, CSize forInt, CSize forBool, CSize forOther) {
    if (optimizePrimitiveExponentials && type instanceof IRIntT) {
      return forInt;
    } else if (optimizePrimitiveExponentials && type instanceof IRBoolT) {
      return forBool;
    } else if (optimizePrimitiveExponentials && type instanceof IRVarT) {
      String typeId = typeId(type(((IRVarT) type).getType()));
      return CSize.ternary(typeId
          + " == "
          + TYPE_ID_INT, forInt, CSize.ternary(typeId + " == " + TYPE_ID_BOOL, forBool, forOther));
    } else {
      return forOther;
    }
  }

  private CAlignment ternaryTypeId(IRType type, CAlignment forInt, CAlignment forBool, CAlignment forOther) {
    if (optimizePrimitiveExponentials && type instanceof IRIntT) {
      return forInt;
    } else if (optimizePrimitiveExponentials && type instanceof IRBoolT) {
      return forBool;
    } else if (optimizePrimitiveExponentials && type instanceof IRVarT) {
      String typeId = typeId(type(((IRVarT) type).getType()));
      return CAlignment.ternary(typeId
          + " == "
          + TYPE_ID_INT, forInt, CAlignment.ternary(typeId + " == " + TYPE_ID_BOOL, forBool, forOther));
    } else {
      return forOther;
    }
  }

  private String typeFlags(String type) {
    return type + ".flags";
  }

  private String typeIsReset(String type, Optional<Boolean> flipPolarity) {
    String flagCheck = "(" + typeFlags(type) + " & 4) != 0";
    if (flipPolarity.isPresent()) {
      return "(" + flagCheck + " && " + typePolarity(type) + " == " + flipPolarity.get() + ")";
    } else {
      return "(" + flagCheck + ")";
    }
  }

  private String typeIsReset(IRType type) {
    if (type instanceof IRCloseT || type instanceof IRResetT) {
      return "1";
    } else if (type instanceof IRVarT) {
      IRVarT var = (IRVarT) type;
      return typeIsReset(type(var.getType()), var.getFlipPolarity());
    } else {
      return "0";
    }
  }

  private String typeIsResetAndNotClose(String type, Optional<Boolean> flipPolarity) {
    return typeId(type) + " != " + TYPE_ID_CLOSE + " && " + typeIsReset(type, flipPolarity);
  }

  private String typeIsResetAndNotClose(IRType type) {
    if (type instanceof IRResetT) {
      return "1";
    } else if (type instanceof IRVarT) {
      IRVarT var = (IRVarT) type;
      return typeIsResetAndNotClose(type(var.getType()), var.getFlipPolarity());
    } else {
      return "0";
    }
  }

  private String typeIsValue(String type) {
    return "((" + typeFlags(type) + " & 2) != 0)";
  }

  private String typePolarity(String type) {
    return "((" + typeFlags(type) + " & 1) != 0)";
  }

  private String typeIsValue(IRType type) {
    return typeIsValue(type.valueRequisites());
  }

  private String typeIsValue(IRType.ValueRequisites requisites) {
    if (!optimizeSendValue) {
      throw new UnsupportedOperationException(
          "Cannot check if type is value when optimizeSendValue is false");
    }

    if (!requisites.canBeValue()) {
      return "0";
    }

    StringBuilder sb = new StringBuilder("1");
    for (int i : requisites.getTypesWhichMustBeValues()) {
      sb.append(" && ");
      sb.append(typeIsValue(type(i)));
    }
    for (Map.Entry<Integer, Boolean> e : requisites.getRequiredTypePolarities().entrySet()) {
      sb.append(" && ");
      if (!e.getValue()) {
        sb.append("!");
      }
      sb.append(typePolarity(type(e.getKey())));
    }
    return sb.toString();
  }

  private CSize ternaryTypeIsValue(
      IRType.ValueRequisites requisites, CSize ifValue, CSize ifNotValue) {
    if (optimizeSendValue && requisites.mustBeValue()) {
      return ifValue;
    } else if (optimizeSendValue && requisites.canBeValue()) {
      return CSize.ternary(typeIsValue(requisites), ifValue, ifNotValue);
    } else {
      return ifNotValue;
    }
  }

  private CSize ternaryTypeIsReset(IRType type, CSize ifReset, CSize ifNotReset) {
    if (type instanceof IRCloseT || type instanceof IRResetT) {
      return ifReset;
    } else if (type instanceof IRVarT) {
      return CSize.ternary(typeIsReset(type), ifReset, ifNotReset);
    } else {
      return ifNotReset;
    }
  }

  private String record(String env, String record) {
    return "RECORD(" + env + ", " + record + ")";
  }

  private String record(String env, int record) {
    return record(env, Integer.toString(record));
  }

  private String record(int record) {
    return record(ENV, record);
  }

  private String accessRecord(String record, String offset, String type) {
    return "ACCESS(" + record + ", " + offset + ", " + type + ")";
  }

  private String accessRecord(String env, String record, String offset, String type) {
    return accessRecord(record(env, record), offset, type);
  }

  private String accessRecord(String env, int record, String offset, String type) {
    return accessRecord(env, Integer.toString(record), offset, type);
  }

  private String accessRecord(int record, String type) {
    return accessRecord(ENV, record, "0", type);
  }

  private String recordHeaderCont(String header) {
    return header.concat(".cont");
  }

  private String recordHeaderContEnv(String header) {
    return header.concat(".cont_env");
  }

  private String recordHeaderContRecord(String header) {
    return header.concat(".cont_record");
  }

  private String exponential(String env, String recordCount, String exponential) {
    return "EXPONENTIAL(" + env + ", " + recordCount + ", " + exponential + ")";
  }

  private String exponential(String env, int recordCount, int exponential) {
    return exponential(env, Integer.toString(recordCount), Integer.toString(exponential));
  }

  private String exponential(int exponential) {
    return exponential(ENV, recordCount, exponential);
  }

  private String exponentialAsInteger(int exponential) {
    // Hacky, but works as long as sizeof(int) < sizeof(struct exponential*)
    // We're just casting the address of the exponential to an int
    return "(int)(unsigned int)(uintptr_t)(" + exponential(exponential) + ")";
  }

  private String exponentialAsBool(int exponential) {
    // Hacky, but works as long as sizeof(unsigned char) < sizeof(struct exponential*)
    // We're just casting the address of the exponential to an unsigned char
    return "(unsigned char)(uintptr_t)(" + exponential(exponential) + ")";
  }

  private String integerAsExponential(String cValue) {
    // Same as above but the cast is in the opposite direction
    return "(struct exponential*)(uintptr_t)((unsigned int)(" + cValue + "))";
  }

  private String boolAsExponential(String cValue) {
    // Same as above but the cast is in the opposite direction
    return "(struct exponential*)(uintptr_t)(" + cValue + ")";
  }

  private String exponentialRefCount(String exponential) {
    return exponential + "->ref_count";
  }

  private String exponentialRefCount(int exponential) {
    return exponentialRefCount(exponential(exponential));
  }

  private String exponentialEnvRecordCount(String exponential) {
    return exponential + "->env_record_count";
  }

  private String exponentialEnvRecordCount(int exponential) {
    return exponentialEnvRecordCount(exponential(exponential));
  }

  private String exponentialEnvExponentialCount(String exponential) {
    return exponential + "->env_exponential_count";
  }

  private String exponentialEnvExponentialCount(int exponential) {
    return exponentialEnvExponentialCount(exponential(exponential));
  }

  private String exponentialEnvTypeCount(String exponential) {
    return exponential + "->env_type_count";
  }

  private String exponentialEnvTypeCount(int exponential) {
    return exponentialEnvTypeCount(exponential(exponential));
  }

  private String exponentialEntry(String exponential) {
    return exponential + "->entry";
  }

  private String exponentialEntry(int exponential) {
    return exponentialEntry(exponential(exponential));
  }

  private String exponentialArgExponential(String exponential, String index) {
    return "EXPONENTIAL_ARG(" + exponential + ", " + index + ")";
  }

  private String exponentialArgExponential(int exponential, String index) {
    return exponentialArgExponential(exponential(exponential), index);
  }

  private String exponentialArgExponential(String exponential, int index) {
    return exponentialArgExponential(exponential, Integer.toString(index));
  }

  private String exponentialArgExponential(int exponential, int index) {
    return exponentialArgExponential(exponential(exponential), index);
  }

  private String exponentialEnvTemplate(String exponential) {
    return "EXPONENTIAL_ENV_TEMPLATE("
        + exponential
        + ", "
        + exponentialEnvExponentialCount(exponential)
        + ")";
  }

  private String exponentialEnvTemplate(int exponential) {
    return exponentialEnvTemplate(exponential(exponential));
  }

  private CSize environmentSize(int recordCount, int exponentialCount, int typeCount) {
    return environmentSize(
        Integer.toString(recordCount),
        Integer.toString(exponentialCount),
        Integer.toString(typeCount));
  }

  private CSize environmentSize(String recordCount, String exponentialCount, String typeCount) {
    return CSize.expression(
        "sizeof(struct environment) + "
            + recordCount
            + " * sizeof(char*) + "
            + exponentialCount
            + " * sizeof(struct exponential*) + "
            + typeCount
            + " * sizeof(struct type)");
  }

  private String cellMutex(String cell) {
    return cell + "->mutex";
  }

  private String cellRefCount(String cell) {
    return cell + "->ref_count";
  }

  private String cellRecord(String cell) {
    return cell + "->record";
  }

  private String labelAddress(String label) {
    return "&&" + label;
  }

  private String blockLabel(String label) {
    return "block_" + procName + "_" + label;
  }

  private String incrementAtomic(String var) {
    if (disableConcurrency) {
      return "++" + var;
    } else {
      return "atomic_fetch_add(&" + var + ", 1) + 1";
    }
  }

  private String decrementAtomic(String var) {
    if (disableConcurrency) {
      return "--" + var;
    } else {
      return "atomic_fetch_sub(&" + var + ", 1) - 1";
    }
  }

  // ================================= Statement building helpers =================================

  private void putDecrementExponentialRefCount(String var) {
    putIf(
        decrementAtomic(exponentialRefCount(var)) + " == 0",
        () -> {
          putStatement("dec_ref_arg_exponentials(" + var + ")");
          putFreeExponential(var);
        });
  }

  private void putDecrementExponentialRefCount(String var, IRType type) {
    putIfNotIntOrBool(type, () -> putDecrementExponentialRefCount(var));
  }

  private void putDecrementEndPoints(boolean isEndPoint, Runnable free) {
    if (isEndPoint) {
      if (optimizeSingleEndpoint && ir.getProcesses().get(procName).getEndPoints() == 1) {
        free.run();
      } else {
        putIf(decrementAtomic(environmentEndPoints()) + " == 0", free);
      }
    }
  }

  private void putDecrementEndPoints(boolean isEndPoint, Runnable free, Runnable otherwise) {
    if (isEndPoint) {
      if ((optimizeSingleEndpoint && ir.getProcesses().get(procName).getEndPoints() == 1)) {
        free.run();
      } else {
        putIfElse(decrementAtomic(environmentEndPoints()) + " == 0", free, otherwise);
      }
    } else {
      otherwise.run();
    }
  }

  private void putAllocEnvironment(
      String var, String recordCount, String exponentialCount, String typeCount) {
    putAssign(
        var, "managed_alloc(" + environmentSize(recordCount, exponentialCount, typeCount) + ")");
    if (profile) {
      putIncrementAtomic("env_allocs");
      putIncrementAtomic("env_current");
      putAssignMaxAtomic("env_peak", "env_current");
    }
  }

  private void putAllocEnvironment(
      String var, int recordCount, int exponentialCount, int typeCount) {
    putAllocEnvironment(
        var,
        Integer.toString(recordCount),
        Integer.toString(exponentialCount),
        Integer.toString(typeCount));
  }

  private void putAllocEnvironment(String var, String processName) {
    IRProcess process = ir.getProcesses().get(processName);
    putAllocEnvironment(
        var,
        process.getRecordCount(),
        process.getExponentialCount(),
        process.getTypeVariableCount());
  }

  private void putFreeEnvironment(String var) {
    if (trace && procName != null) {
      putDebugLn("[endCall(" + procName + ")]");
    }
    putLine("managed_free(" + var + ");");
    if (profile) {
      putIncrementAtomic("env_frees");
      putDecrementAtomic("env_current");
    }
  }

  private void putAllocRecord(String var, CSize bufferSize) {
    putAssign(var, "managed_alloc(" + bufferSize + ")");
    if (profile) {
      putIncrementAtomic("record_allocs");
      putIncrementAtomic("record_current");
      putAssignMaxAtomic("record_peak", "record_current");
    }
  }

  private void putAllocRecord(String var, IRType type) {
    putAllocRecord(var, allocationSize(type));
  }

  private void putReallocRecord(String var, String bufferSize) {
    putAssign(
        var, "managed_realloc(" + var + ", sizeof(struct record_header) + " + bufferSize + ")");
    if (profile) {
      putIncrementAtomic("record_reallocs");
    }
  }

  private void putFreeRecord(String var) {
    putLine("managed_free(" + var + ");");
    if (profile) {
      putIncrementAtomic("record_frees");
      putDecrementAtomic("record_current");
    }
  }

  private void putAllocTask(String var) {
    putAssign(var, "managed_alloc(sizeof(struct task))");
    if (profile) {
      putIncrementAtomic("task_allocs");
    }
  }

  private void putFreeTask(String var) {
    putLine("managed_free(" + var + ");");
    if (profile) {
      putIncrementAtomic("task_frees");
    }
  }

  private void putAllocExponential(
      String var, int recordCount, int exponentialCount, int typeCount) {
    putAssign(
        var,
        "managed_alloc(sizeof(struct exponential) + "
            + exponentialCount
            + " * sizeof(struct exponential*) + "
            + environmentSize(recordCount, exponentialCount, typeCount)
            + ")");
    if (profile) {
      putIncrementAtomic("exponential_allocs");
    }
  }

  private void putFreeExponential(String var) {
    if (trace) {
      putDebugLn("[freeExponential(" + var + ")]");
    }
    putLine("managed_free(" + var + ");");
    if (profile) {
      putIncrementAtomic("exponential_frees");
    }
  }

  private void putAllocCell(String var) {
    putAssign(var, "managed_alloc(sizeof(struct cell))");

    if (!disableConcurrency) {
      putStatement("pthread_mutex_init(&" + cellMutex(var) + ", NULL)");
    }
    if (profile) {
      putIncrementAtomic("cell_allocs");
    }
  }

  private void putFreeCell(String var) {
    if (trace) {
      putDebugLn("[freeCell(" + var + ")]");
    }
    if (!disableConcurrency) {
      putStatement("pthread_mutex_destroy(&" + cellMutex(var) + ")");
    }
    putStatement("managed_free(" + var + ")");
    if (profile) {
      putIncrementAtomic("cell_frees");
    }
  }

  private void putPushValue(String record, IRSessionT type, String argRecord, IRType argType) {
    if (!type.getArg().equals(argType)) {
      if (type.getArg() instanceof IRSessionT) {
        putPushValue(record, (IRSessionT) type.getArg(), argRecord, argType);
      }

      throw new UnsupportedOperationException("Value type does not match record argument type");
    }

    // A simple copy may not be enough as the paddings might differ
    //
    // E.g, the argument type 'send lint; bool':
    // - the argument record by itself could be packed like so: BOOL:3xPADDING:LINT
    // - the full type could be send (send lint; bool); bool: BOOL:BOOL:2xPADDING:LINT
    //
    // Thus, here we may have to copy each slot individually into its right place

    LayoutCalculator contLayout = layout(type.getCont());
    LayoutCalculator desiredLayout = layout(type.getArg(), contLayout.offset.add(contLayout.size));
    LayoutCalculator currentLayout = layout(argType);

    if (currentLayout.padding.equals(desiredLayout.padding)
        && currentLayout.size.equals(desiredLayout.size)) {
      // Equal memory layout, we can just copy the entire data at once!
      putCopy(
          desiredLayout.size.subtract(desiredLayout.firstSlotOffset).retreatPointer(record),
          currentLayout.size.subtract(currentLayout.firstSlotOffset).retreatPointer(argRecord),
          desiredLayout.size);
      putAssignSub(
          record,
          desiredLayout
              .size
              .subtract(desiredLayout.firstSlotSize)
              .add(desiredLayout.padding)
              .add(contLayout.firstSlotOffset));
      putConsumeRecord(argRecord, argType);
    } else {
      // Uneven padding, we need to copy this slot manually
      putCopy(record, argRecord, currentLayout.firstSlotSize);
      putAssignSub(record, desiredLayout.nextSlotOffset);
      putAssignSub(argRecord, currentLayout.nextSlotOffset);

      if (argType instanceof IRSessionT) {
        argType = ((IRSessionT) argType).getCont();
        type = new IRSessionT(argType, type.getCont());
        putPushValue(record, type, argRecord, argType);
      }
    }
  }

  private void putPopValue(String record, IRSessionT type, String argRecord, IRType argType) {
    if (!type.getArg().equals(argType)) {
      if (type.getArg() instanceof IRSessionT) {
        putPushValue(record, (IRSessionT) type.getArg(), argRecord, argType);
      }

      throw new UnsupportedOperationException(
          "Value type " + argType + " does not match record argument type " + type.getArg());
    }

    // See putPushValue for an explanation of why this is needed

    LayoutCalculator contLayout = layout(type.getCont());
    LayoutCalculator desiredLayout = layout(argType);
    LayoutCalculator currentLayout = layout(type.getArg(), contLayout.offset.add(contLayout.size));

    if (currentLayout.padding.equals(desiredLayout.padding)
        && currentLayout.size.equals(desiredLayout.size)) {
      // Equal padding, we can just copy the entire data at once!
      putCopy(
          desiredLayout.firstSlotSize.subtract(desiredLayout.size).advancePointer(argRecord),
          currentLayout.firstSlotSize.subtract(currentLayout.size).advancePointer(record),
          desiredLayout.size);
      putAssignSub(
          record,
          currentLayout
              .size
              .subtract(currentLayout.firstSlotSize)
              .add(currentLayout.padding)
              .add(contLayout.firstSlotOffset));
    } else {
      // Uneven padding, we need to copy this slot manually
      putCopy(argRecord, record, desiredLayout.firstSlotSize);
      putAssignSub(record, currentLayout.nextSlotOffset);

      if (argType instanceof IRSessionT) {
        argType = ((IRSessionT) argType).getCont();
        type = new IRSessionT(argType, type.getCont());
        putPopValue(record, type, desiredLayout.nextSlotOffset.retreatPointer(argRecord), argType);
      }
    }
  }

  private String makeLabel(String base) {
    return procName + "_" + base + (nextLabel++);
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

  private void putAssign(String var, String value) {
    putStatement(var + " = " + value);
  }

  private void putAssign(String var, int value) {
    putStatement(var + " = " + value);
  }

  private void putAssignAdd(String var, CSize value) {
    putAssignAdd(var, value.toString());
  }

  private void putAssignAdd(String var, String value) {
    if (!value.equals("0")) {
      putStatement(var + " += " + value);
    }
  }

  private void putAssignSub(String var, CSize value) {
    putAssignSub(var, value.toString());
  }

  private void putAssignSub(String var, String value) {
    if (!value.equals("0")) {
      putStatement(var + " -= " + value);
    }
  }

  // Advances the record cursor to the start of the header, assuming it currently is located at the
  // start of the first slot
  private void putConsumeRecord(int record, IRType currentType) {
    putConsumeRecord(record(record), currentType);
  }

  private void putConsumeRecord(String record, IRType currentType) {
    putAssignSub(record, headerOffset(currentType));
  }

  // Rolls back the cursor to the first slot, assuming it currently is located at the start of the
  // header
  private void putResetRecord(int record, IRType currentType) {
    putResetRecord(record(record), currentType);
  }

  private void putResetRecord(String record, IRType currentType) {
    putAssignAdd(record, headerOffset(currentType));
  }

  private void putAdvanceRecord(int record, IRType currentType) {
    putAdvanceRecord(record(record), currentType);
  }

  private void putAdvanceRecord(String var, IRType currentType) {
    putAssignSub(var, nextSlotOffset(currentType));
  }

  private void putAdvanceRecordWithTag(int record, IRTagT currentType, int tag) {
    putAdvanceRecordWithTag(record(record), currentType, tag);
  }

  private void putAdvanceRecordWithTag(String var, IRTagT currentType, int tag) {
    putAssignSub(var, nextSlotOffsetWithTag(currentType, tag));
  }

  private void putIfNotIntOrBool(IRType type, Runnable runnable) {
    if (optimizePrimitiveExponentials && type instanceof IRVarT) {
      putIf(typeId(type(((IRVarT) type).getType())) + " == " + TYPE_ID_OTHER, runnable);
    } else if (!optimizePrimitiveExponentials
        || !(type instanceof IRIntT || type instanceof IRBoolT)) {
      runnable.run();
    }
  }

  private void putSwitchTypeId(IRType type, Runnable forInt, Runnable forBool, Runnable forOther) {
    if (optimizePrimitiveExponentials && type instanceof IRIntT) {
      forInt.run();
    } else if (optimizePrimitiveExponentials && type instanceof IRBoolT) {
      forBool.run();
    } else if (optimizePrimitiveExponentials && type instanceof IRVarT) {
      String typeId = typeId(type(((IRVarT) type).getType()));
      putLine("switch (" + typeId + ") {");
      incIndent();
      putLine("case " + TYPE_ID_INT + ": {/* int */");
      incIndent();
      forInt.run();
      decIndent();
      putLine("} break;");
      putLine("case " + TYPE_ID_BOOL + ": {/* bool */");
      incIndent();
      forBool.run();
      decIndent();
      putLine("} break;");
      putLine("default: {/* other */");
      incIndent();
      forOther.run();
      decIndent();
      putLine("} break;");
      decIndent();
      putLine("}");
    } else {
      forOther.run();
    }
  }

  private void putIfTypeIsNotValue(IRType.ValueRequisites requisites, Runnable ifNotValue) {
    putSwitchTypeIsValue(requisites, Optional.empty(), Optional.of(ifNotValue));
  }

  private void putSwitchTypeIsValue(
      IRType.ValueRequisites requisites, Runnable ifValue, Runnable ifNotValue) {
    putSwitchTypeIsValue(requisites, Optional.of(ifValue), Optional.of(ifNotValue));
  }

  private void putSwitchTypeIsValue(
      IRType.ValueRequisites requisites,
      Optional<Runnable> ifValue,
      Optional<Runnable> ifNotValue) {
    if (optimizeSendValue && requisites.mustBeValue()) {
      if (ifValue.isPresent()) {
        ifValue.get().run();
      }
    } else if (optimizeSendValue && requisites.canBeValue()) {
      if (ifValue.isPresent() && ifNotValue.isPresent()) {
        putIfElse(typeIsValue(requisites), ifValue.get(), ifNotValue.get());
      } else if (ifValue.isPresent()) {
        putIf(typeIsValue(requisites), ifValue.get());
      } else if (ifNotValue.isPresent()) {
        putIf("!" + typeIsValue(requisites), ifNotValue.get());
      }
    } else if (ifNotValue.isPresent()) {
      ifNotValue.get().run();
    }
  }

  private void putSwitchTypeIsResetAndNotClose(IRType type, Runnable ifReset, Runnable ifNotReset) {
    if (type instanceof IRResetT) {
      ifReset.run();
    } else if (type instanceof IRVarT) {
      putIfElse(typeIsReset(type), ifReset, ifNotReset);
    } else {
      ifNotReset.run();
    }
  }

  private void putIncrementAtomic(String var) {
    putStatement(incrementAtomic(var));
  }

  private void putDecrementAtomic(String var) {
    putStatement(decrementAtomic(var));
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
    putLine("if (" + condition + ") {");
    incIndent();
    then.run();
    decIndent();
    putLine("}");
  }

  private void putIfElse(String condition, Runnable then, Runnable otherwise) {
    putLine("if (" + condition + ") {");
    incIndent();
    then.run();
    decIndent();
    putLine("} else {");
    incIndent();
    otherwise.run();
    decIndent();
    putLine("}");
  }

  private void putWhile(String condition, Runnable body) {
    putLine("while (" + condition + ") {");
    incIndent();
    body.run();
    decIndent();
    putLine("}");
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

  private void putIndent() {
    put("  ".repeat(indentLevel));
  }

  private void putLineEnd() {
    put("\n");
  }

  private void put(String str) {
    code += str;
  }

  private void incIndent() {
    indentLevel++;
  }

  private void decIndent() {
    indentLevel--;
  }

  // ========================== Type visitor used to determine type size ==========================

  // Returns the maximum size of all segments of the given type, also including the record tail.
  // This is used to determine the size of the buffer that we'll need to allocate for the record.
  private CSize allocationSize(IRType type) {
    return arch.recordHeaderSize.add(totalSize(type));
  }

  // Returns the maximum size of all segments of the given type, including padding.
  private CSize totalSize(IRType type) {
    return totalSize(type, 0);
  }

  private CSize totalSize(IRType type, int definedTypes) {
    LayoutCalculator layout = layout(type, definedTypes);
    CSize size = layout.padding.add(layout.size);
    for (LayoutCalculator.Segment segment : layout.nextSegments) {
      size = size.max(totalSize(segment.type, segment.definedTypes));
    }
    return size;
  }

  // Returns the offset of the last slot of the given type in relation to the current cursor
  private CSize lastSlotOffset(IRType type) {
    LayoutCalculator layout = layout(type);
    return layout.size.subtract(layout.firstSlotOffset);
  }

  // Returns the offset of the header of the given type in relation to the current cursor
  private CSize headerOffset(IRType type) {
    LayoutCalculator layout = layout(type);
    return arch.recordHeaderSize
        .add(layout.padding)
        .add(layout.size)
        .subtract(layout.firstSlotOffset);
  }

  // Given the current record type, returns how much the cursor should be moved back to access
  // the next slot. Doesn't work for tag types, as these depend on the tag value.
  private CSize nextSlotOffset(IRType type) {
    return layout(type).nextSlotOffset;
  }

  // Given the a tag type, returns how much the cursor should be moved back, depending on the
  // tag value.
  private CSize nextSlotOffsetWithTag(IRTagT type, int tag) {
    LayoutCalculator layout = layout(type);
    LayoutCalculator caseLayout = layout(type.getChoices().get(tag));

    // We get the offset by going back the full size, which is the maximum size of all branches,
    // and then forward by the size of the specific chosen branch
    return layout
        .size
        .subtract(layout.firstSlotOffset)
        .subtract(caseLayout.size)
        .add(caseLayout.firstSlotOffset);
  }

  private CAlignment alignment(IRType type, int definedTypes) {
    // The offset doesn't affect the alignment
    LayoutCalculator calc = new LayoutCalculator(definedTypes, CSize.zero(), CSize.zero());
    type.accept(calc);
    return calc.alignment;
  }

  private LayoutCalculator layout(IRType type) {
    return layout(type, 0);
  }

  private LayoutCalculator layout(IRType type, int definedTypes) {
    return layout(type, definedTypes, arch.recordHeaderSize);
  }

  private LayoutCalculator layout(IRType type, CSize offset) {
    return layout(type, 0, offset);
  }

  private LayoutCalculator layout(IRType type, int definedTypes, CSize offset) {
    CAlignment alignment = alignment(type, definedTypes);
    LayoutCalculator calc =
        new LayoutCalculator(definedTypes, offset.align(alignment), offset.align(alignment));
    calc.padding = offset.padding(alignment);
    type.accept(calc);
    return calc;
  }

  private class LayoutCalculator extends IRTypeVisitor {
    private CSize resetOffset;
    private CSize offset;
    private int definedTypes;

    // Size of the type until the next reset starting from the offset, excluding padding
    public CSize size;

    // Offset from the end of the buffer to the start of the slot
    // Usually, this is the size of the slot, but for close/reset types, it is equal to the offset
    // to the header.
    public CSize firstSlotOffset;
    public CSize firstSlotSize;
    public CSize nextSlotOffset; // Offset to the start of the next slot
    public CAlignment alignment; // Required alignment of the type
    public CSize padding; // Number of padding bytes to add after the offset
    public List<Segment> nextSegments = List.of(); // Segments which comes after this one

    public class Segment {
      public IRType type;
      public int definedTypes;

      public Segment(IRType type, int definedTypes) {
        this.type = type;
        this.definedTypes = definedTypes;
      }
    }

    public LayoutCalculator(int definedTypes, CSize resetOffset, CSize nextSlotOffset) {
      this(definedTypes, resetOffset, resetOffset, nextSlotOffset);
    }

    public LayoutCalculator(
        int definedTypes, CSize resetOffset, CSize offset, CSize nextSlotOffset) {
      this.resetOffset = resetOffset;
      this.offset = offset;
      this.definedTypes = definedTypes;
      this.nextSlotOffset = nextSlotOffset;
    }

    private LayoutCalculator recurse(IRType type, CSize offset) {
      return recurse(type, offset, nextSlotOffset);
    }

    private LayoutCalculator recurse(IRType type, CSize offset, CSize nextSlotOffset) {
      LayoutCalculator calc =
          new LayoutCalculator(definedTypes, resetOffset, offset, nextSlotOffset);
      type.accept(calc);
      return calc;
    }

    @Override
    public void visit(IRType type) {
      throw new UnsupportedOperationException("Unsupported type: " + type.getClass().getName());
    }

    @Override
    public void visit(IRCloseT type) {
      size = CSize.zero();
      firstSlotOffset = offset;
      firstSlotSize = CSize.zero();
      alignment = CAlignment.one();
    }

    @Override
    public void visit(IRSessionT type) {
      LayoutCalculator contLayout = recurse(type.getCont(), offset);

      CAlignment alignmentIfValue = alignment(type.getArg(), definedTypes);
      CAlignment alignmentIfNotValue = arch.pointerAlignment;

      CSize paddingIfValue = offset.add(contLayout.size).padding(alignmentIfValue);
      CSize paddingIfNotValue = offset.add(contLayout.size).padding(alignmentIfNotValue);

      LayoutCalculator argValueLayout =
          recurse(
              type.getArg(),
              offset.add(contLayout.size).align(alignmentIfValue),
              contLayout.firstSlotOffset.add(paddingIfValue));

      CSize sizeIfValue = contLayout.size.add(paddingIfValue).add(argValueLayout.size);
      CSize sizeIfNotValue = contLayout.size.add(paddingIfNotValue).add(arch.pointerSize);

      CSize firstSlotOffsetIfValue = argValueLayout.firstSlotSize;
      CSize firstSlotOffsetIfNotValue = arch.pointerSize;

      CSize firstSlotSizeIfValue = argValueLayout.firstSlotSize;
      CSize firstSlotSizeIfNotValue = arch.pointerSize;

      CSize nextSlotOffsetIfValue = argValueLayout.nextSlotOffset;
      CSize nextSlotOffsetIfNotValue = contLayout.firstSlotOffset.add(paddingIfNotValue);

      size = ternaryTypeIsValue(type.getArg().valueRequisites(), sizeIfValue, sizeIfNotValue);
      firstSlotOffset =
          ternaryTypeIsValue(
              type.getArg().valueRequisites(), firstSlotOffsetIfValue, firstSlotOffsetIfNotValue);
      firstSlotSize =
          ternaryTypeIsValue(
              type.getArg().valueRequisites(), firstSlotSizeIfValue, firstSlotSizeIfNotValue);
      nextSlotOffset =
          ternaryTypeIsValue(
              type.getArg().valueRequisites(), nextSlotOffsetIfValue, nextSlotOffsetIfNotValue);
      alignment = contLayout.alignment;
      nextSegments = contLayout.nextSegments;
    }

    @Override
    public void visit(IRTagT type) {
      size = CSize.zero();
      firstSlotOffset = arch.unsignedCharSize;
      firstSlotSize = arch.unsignedCharSize;
      alignment = CAlignment.one();

      nextSegments = new ArrayList<>();
      for (int i = 0; i < type.getChoices().size(); ++i) {
        LayoutCalculator layout = recurse(type.getChoices().get(i), offset);
        size = size.max(layout.size);
        alignment = alignment.max(layout.alignment);
        nextSegments.addAll(layout.nextSegments);
      }

      size = arch.unsignedCharSize.add(size);
      nextSlotOffset = CSize.expression("UNKNOWN"); // nextSlotOffsetWithTag should be used for this
    }

    @Override
    public void visit(IRRecT type) {
      nextSegments = List.of(new Segment(type.getInner(), definedTypes + 1));
      size = CSize.zero(); // End of the segment
      firstSlotOffset = offset;
      firstSlotSize = CSize.zero();
      alignment = CAlignment.one();
    }

    @Override
    public void visit(IRVarT type) {
      if (type.getType() >= definedTypes) {
        // The variable must refer to a type bound in the environment.
        String typeVar = type(type.getType());
        size = typeSize(typeVar);
        firstSlotOffset = ternaryTypeIsReset(type, offset, size);
        firstSlotSize = size;
        alignment = typeAlignment(typeVar);
      } else {
        // The variable refers to a type bound in the current type, e.g. a recursive type.
        // In that case, since recursive types reset the buffer, we consider their size to be 0.
        size = CSize.zero();
        firstSlotOffset = CSize.zero();
        firstSlotSize = CSize.zero();
        alignment = CAlignment.one();
      }
    }

    @Override
    public void visit(IRIntT type) {
      size = arch.intSize;
      firstSlotOffset = arch.intSize;
      firstSlotSize = arch.intSize;
      alignment = arch.intAlignment;
    }

    @Override
    public void visit(IRBoolT type) {
      size = arch.unsignedCharSize;
      firstSlotOffset = arch.unsignedCharSize;
      firstSlotSize = arch.unsignedCharSize;
      alignment = arch.unsignedCharAlignment;
    }

    @Override
    public void visit(IRStringT type) {
      size = arch.pointerSize;
      firstSlotOffset = arch.pointerSize;
      firstSlotSize = arch.pointerSize;
      alignment = arch.pointerAlignment;
    }

    @Override
    public void visit(IRTypeT type) {
      size = CSize.sizeOf("struct type_slot");
      firstSlotOffset = CSize.sizeOf("struct type_slot");
      firstSlotSize = CSize.sizeOf("struct type_slot");
      alignment = arch.pointerAlignment;
    }

    @Override
    public void visit(IRExponentialT type) {
      size = ternaryTypeId(type.getInner(), arch.intSize, arch.unsignedCharSize, arch.pointerSize);
      firstSlotOffset = ternaryTypeId(type.getInner(), arch.intSize, arch.unsignedCharSize, arch.pointerSize);
      firstSlotSize = ternaryTypeId(type.getInner(), arch.intSize, arch.unsignedCharSize, arch.pointerSize);
      alignment = ternaryTypeId(type.getInner(), arch.intAlignment, arch.unsignedCharAlignment, arch.pointerAlignment);
    }

    @Override
    public void visit(IRCellT type) {
      throw new UnsupportedOperationException("Cells unimplemented");
      // size += "sizeof(struct cell*)";
    }

    @Override
    public void visit(IRResetT type) {
      nextSegments = List.of(new Segment(type.getCont(), definedTypes));
      size = CSize.zero(); // End of the segment
      firstSlotOffset = offset;
      firstSlotSize = CSize.zero();
      alignment = CAlignment.one();
    }
  }

  // ========================== Visitor used to generate expression code ==========================

  private String expression(IRExpression expr) {
    ExpressionGenerator gen = new ExpressionGenerator();
    expr.accept(gen);
    return gen.code;
  }

  private String expressionToString(IRExpression expr) {
    if (expr.getType() instanceof IRIntT) {
      return "string_from_int(" + expression(expr) + ")";
    } else if (expr.getType() instanceof IRBoolT) {
      return "string_create(" + expression(expr) + " ? \"true\" : \"false\")";
    } else if (expr.getType() instanceof IRStringT) {
      return expression(expr);
    } else {
      throw new UnsupportedOperationException(
          "Unsupported expression type: " + expr.getType().getClass().getName());
    }
  }

  private class ExpressionGenerator extends IRExpressionVisitor {
    private String code = "";

    private void binary(String op, IRExpression lhs, IRExpression rhs) {
      code += "(";
      lhs.accept(this);
      code += " " + op + " ";
      rhs.accept(this);
      code += ")";
    }

    @Override
    public void visit(IRExpression expr) {
      throw new UnsupportedOperationException(
          "Unsupported expression: " + expr.getClass().getName());
    }

    @Override
    public void visit(IRInt expr) {
      code += expr.getValue();
    }

    @Override
    public void visit(IRBool expr) {
      code += expr.getValue() ? "1" : "0";
    }

    @Override
    public void visit(IRString expr) {
      code += "string_create(\"" + expr.getEscapedValue() + "\")";
    }

    @Override
    public void visit(IRVar expr) {
      CSize offset = nextSlotOffset(expr.getType());
      code +=
          accessRecord(
              record(expr.getRecord()) + " -= " + offset, offset.toString(), cType(expr.getType()));
    }

    @Override
    public void visit(IRExponentialVar expr) {
      throw new UnsupportedOperationException(
          "Exponentials are still not supported in expressions: " + expr.getClass().getName());
    }

    @Override
    public void visit(IRAdd expr) {
      if (expr.getType() instanceof IRStringT) {
        code +=
            "string_concat("
                + expressionToString(expr.getLhs())
                + ", "
                + expressionToString(expr.getRhs())
                + ")";
      } else {
        if (!(expr.getType() instanceof IRIntT)
            || !(expr.getLhs().getType() instanceof IRIntT)
            || !(expr.getRhs().getType() instanceof IRIntT)) {
          throw new UnsupportedOperationException(
              "Unsupported addition: " + expr.getType().getClass().getName());
        }

        binary("+", expr.getLhs(), expr.getRhs());
      }
    }

    @Override
    public void visit(IRSub expr) {
      binary("-", expr.getLhs(), expr.getRhs());
    }

    @Override
    public void visit(IRMul expr) {
      binary("*", expr.getLhs(), expr.getRhs());
    }

    @Override
    public void visit(IRDiv expr) {
      binary("/", expr.getLhs(), expr.getRhs());
    }

    @Override
    public void visit(IREq expr) {
      if (expr.getLhs() instanceof IRString || expr.getRhs() instanceof IRString) {
        code +=
            "string_equal("
                + expressionToString(expr.getLhs())
                + ", "
                + expressionToString(expr.getRhs())
                + ")";
      } else {
        binary("==", expr.getLhs(), expr.getRhs());
      }
    }

    @Override
    public void visit(IRLt expr) {
      binary("<", expr.getLhs(), expr.getRhs());
    }

    @Override
    public void visit(IRGt expr) {
      binary(">", expr.getLhs(), expr.getRhs());
    }

    @Override
    public void visit(IRAnd expr) {
      binary("&&", expr.getLhs(), expr.getRhs());
    }

    @Override
    public void visit(IROr expr) {
      binary("||", expr.getLhs(), expr.getRhs());
    }

    @Override
    public void visit(IRNot expr) {
      code += "!(";
      expr.getInner().accept(this);
      code += ")";
    }
  }

  // ========================= Visitor used to get C types from IR types ==========================

  private String cType(IRType type) {
    TypeGenerator tGen = new TypeGenerator();
    type.accept(tGen);
    return tGen.code;
  }

  private class TypeGenerator extends IRTypeVisitor {
    private String code;

    @Override
    public void visit(IRType type) {
      throw new UnsupportedOperationException("Unsupported type: " + type.getClass().getName());
    }

    @Override
    public void visit(IRIntT type) {
      code = "int";
    }

    @Override
    public void visit(IRBoolT type) {
      code = "unsigned char";
    }

    @Override
    public void visit(IRStringT type) {
      code = "char*";
    }

    @Override
    public void visit(IRSessionT type) {
      throw new UnsupportedOperationException(
          "Session types should not be used as C types directly, as their type depends on they being a value or not");
    }

    @Override
    public void visit(IRExponentialT type) {
      code = "struct exponential*";
    }

    @Override
    public void visit(IRTagT type) {
      code = "unsigned char";
    }
  }

  // ========================= Visitor used to generate print statements ==========================

  private void generatePrint(IRExpression expr, boolean newLine) {
    PrintGenerator pGen = new PrintGenerator(expr, newLine);
    expr.getType().accept(pGen);
  }

  private class PrintGenerator extends IRTypeVisitor {
    private IRExpression expr;
    private boolean newLine;

    public PrintGenerator(IRExpression expr, boolean newLine) {
      this.expr = expr;
      this.newLine = newLine;
    }

    private String nl() {
      return newLine ? "\\n" : "";
    }

    @Override
    public void visit(IRType type) {
      throw new UnsupportedOperationException("Non-printable type: " + type.getClass().getName());
    }

    @Override
    public void visit(IRIntT type) {
      putStatement("printf(\"%d" + nl() + "\", " + expression(expr) + ")");
    }

    @Override
    public void visit(IRBoolT type) {
      putStatement("printf(\"%s" + nl() + "\", " + expression(expr) + " ? \"true\" : \"false\")");
    }

    @Override
    public void visit(IRStringT type) {
      putStatement("string_print(\"%s" + nl() + "\", " + expression(expr) + ")");
    }
  }
}
