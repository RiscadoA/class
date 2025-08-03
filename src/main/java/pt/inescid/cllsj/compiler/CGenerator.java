package pt.inescid.cllsj.compiler;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import pt.inescid.cllsj.compiler.ir.*;
import pt.inescid.cllsj.compiler.ir.expressions.*;
import pt.inescid.cllsj.compiler.ir.instructions.*;
import pt.inescid.cllsj.compiler.ir.instructions.IRCallProcess.ExponentialArgument;
import pt.inescid.cllsj.compiler.ir.instructions.IRCallProcess.LinearArgument;
import pt.inescid.cllsj.compiler.ir.instructions.IRCallProcess.TypeArgument;
import pt.inescid.cllsj.compiler.ir.type.*;

public class CGenerator extends IRInstructionVisitor {
  private static final String TMP_TASK = "tmp_task";
  private static final String TMP_ENV = "tmp_env";
  private static final String TMP_CONT = "tmp_cont";
  private static final String TMP_RECORD = "tmp_record";
  private static final String TMP_EXPONENTIAL = "tmp_exponential";
  private static final String TMP_THREAD = "tmp_thread";
  private static final String TMP_CELL = "tmp_cell";

  private static final String TASK = "task";
  private static final String ENV = "env";
  private static final String MANAGER_STATE = "manager_state";

  private static final int TYPE_ID_OTHER = 0;
  private static final int TYPE_ID_INT = 1;
  private static final int TYPE_ID_BOOL = 2;

  private IRProgram ir;
  private String code = "";
  private int indentLevel = 0;
  private int genLabelCountInBlock;
  private String procName;
  private int recordCount;
  private int exponentialCount;
  private Optional<String> blockName;
  private boolean entryCall = true;
  private boolean inManager = false;

  public String entryProcess = "main";
  public boolean trace = false;
  public boolean profile = false;
  public boolean disableConcurrency = false;
  public boolean optimizePrimitiveExponentials = true;
  public boolean optimizeTailCalls = true;

  private Set<IRType> usedRecordManagers = new HashSet<>();

  public String generate(IRProgram ir) {
    this.ir = ir;

    // Add the necessary includes.
    putLine("#define _POSIX_C_SOURCE 199309L");
    putLine("#include <stdio.h>");
    putLine("#include <stdlib.h>");
    putLine("#include <string.h>");
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

    // Define the record struct.
    putLine("struct record {");
    incIndent();
    putLine("void* cont;");
    putLine("struct environment* cont_env;");
    putLine("int cont_record;");
    putLine("unsigned char read;");
    putLine("unsigned char written;");
    putLine("char buffer[];");
    decIndent();
    putLine("};");
    putBlankLine();

    // Define the manager stack struct.
    putLine("struct manager_state {");
    incIndent();
    putLine("struct environment** env;");
    putLine("struct record** record;");
    putLine("struct type* type;");
    putLine("int env_capacity;");
    putLine("int record_capacity;");
    putLine("int type_capacity;");
    putLine("int env_count;");
    putLine("int record_count;");
    putLine("int type_count;");
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
    putLine("struct record* record;");
    putLine(
        "void(*manager)(const char* old, char* new, int written, int read, struct manager_state* "
            + MANAGER_STATE
            + ");");
    decIndent();
    putLine("};");
    putBlankLine();

    // Define the type struct.
    putLine("struct type {");
    incIndent();
    if (optimizePrimitiveExponentials) {
      putLine("int id;");
    }
    putLine("int size;");
    putLine(
        "void(*manager)(const char* old, char* new, int written, int read, struct manager_state* "
            + MANAGER_STATE
            + ");");
    decIndent();
    putLine("};");
    putBlankLine();

    // Define the environment struct.
    putLine("struct environment {");
    incIndent();
    putLine(
        "struct environment*(*manager)(struct environment* env, struct manager_state* "
            + MANAGER_STATE
            + ", int clone);");
    if (disableConcurrency) {
      putLine("int end_points;");
    } else {
      putLine("atomic_int end_points;");
    }
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
    putLine("struct cell {");
    incIndent();
    if (disableConcurrency) {
      putLine("int ref_count;");
    } else {
      putLine("pthread_mutex_t mutex;");
      putLine("atomic_int ref_count;");
    }
    putLine("struct record* record;");
    decIndent();
    putLine("};");
    putBlankLine();

    // Utility macros for accessing records and exponentials on a given environment, and on
    // exponentials.
    put("#define RECORD(env, rec) (*(struct record**)(");
    put("(char*)(env) + ");
    put("sizeof(struct environment) + ");
    put("sizeof(struct record*) * rec");
    put("))");
    putLineEnd();
    put("#define EXPONENTIAL(env, rec_count, exp) (*(struct exponential**)(");
    put("(char*)(env) + ");
    put("sizeof(struct environment) + ");
    put("sizeof(struct record*) * rec_count + ");
    put("sizeof(struct exponential*) * exp");
    put("))");
    putLineEnd();
    put("#define TYPE(env, rec_count, exp_count, type_i) (*(struct type*)(");
    put("(char*)(env) + ");
    put("sizeof(struct environment) + ");
    put("sizeof(struct record*) * rec_count + ");
    put("sizeof(struct exponential*) * exp_count + ");
    put("sizeof(struct type) * type_i");
    put("))");
    putLineEnd();
    putBlankLine();

    // Utility macro for accessing the read, written and buffer fields of a record in the active
    // environment.
    putLine("#define READ(rec) RECORD(" + ENV + ", rec)->read");
    putLine("#define WRITTEN(rec) RECORD(" + ENV + ", rec)->written");
    putLine("#define BUFFER(rec) RECORD(" + ENV + ", rec)->buffer");
    putBlankLine();

    // Utility macros for pushing and popping values to/from the buffer of a record in the active
    // environment.
    put("#define PUSH_RAW(rec, type, ...) (*(type*)(");
    put("&(rec->buffer)[(rec->written += sizeof(type)) - sizeof(type)]");
    put(")) = __VA_ARGS__");
    putLineEnd();
    put("#define PUSH(rec, type, ...) (*(type*)(");
    put("&BUFFER(rec)[(WRITTEN(rec) += sizeof(type)) - sizeof(type)]");
    put(")) = __VA_ARGS__");
    putLineEnd();
    put("#define POP(rec, type) (*(type*)(");
    put("&BUFFER(rec)[(READ(rec) += sizeof(type)) - sizeof(type)]");
    put("))");
    putLineEnd();
    put("#define PEEK(rec, type) (*(type*)(");
    put("&BUFFER(rec)[READ(rec)]");
    put("))");
    putBlankLine();

    // Utility macros for pushing stuff to manager stacks.
    put("#define MANAGER_PUSH(what, ...) do { ");
    put(
        "if ("
            + MANAGER_STATE
            + "->what ## _count == "
            + MANAGER_STATE
            + "->what ## _capacity) { ");
    put(MANAGER_STATE + "->what ## _capacity = " + MANAGER_STATE + "->what ## _capacity * 2 + 1;");
    put(
        MANAGER_STATE
            + "->what = realloc("
            + MANAGER_STATE
            + "->what, "
            + MANAGER_STATE
            + "->what ## _capacity * sizeof(__VA_ARGS__)); ");
    put("} ");
    put(MANAGER_STATE + "->what[" + MANAGER_STATE + "->what ## _count++] = __VA_ARGS__; ");
    put("} while (0)");
    putLineEnd();
    put("#define MANAGER_PUSH_PAIR(what, first, second) do { ");
    put("MANAGER_PUSH(what, first); ");
    put("MANAGER_PUSH(what, second); ");
    put("} while (0)");
    putLineEnd();
    put("#define MANAGER_POP(what) do { ");
    put(MANAGER_STATE + "->what ## _count -= 1; ");
    put("} while (0)");
    putLineEnd();
    put("#define MANAGER_RESET() do { ");
    put(MANAGER_STATE + "->env_count = 0; ");
    put(MANAGER_STATE + "->record_count = 0; ");
    put(MANAGER_STATE + "->type_count = 0; ");
    put("} while (0)");
    putBlankLine();

    // Utility macros for calling environment managers.
    putLine("#define ENV_MANAGER_CALL_CLONE(what) (what)->manager(what, " + MANAGER_STATE + ", 1)");
    putLine("#define ENV_MANAGER_CALL_CLEAN(what) (what)->manager(what, " + MANAGER_STATE + ", 0)");
    putBlankLine();

    // Utility macros for finding values in manager stacks.
    put("#define MANAGER_FIND(what, value, index) do { ");
    put("for (index = 0; index < " + MANAGER_STATE + "->what ## _count; ++index) { ");
    put("if (" + MANAGER_STATE + "->what[index] == value) { ");
    put("break; ");
    put("} ");
    put("} ");
    put("} while (0)");
    putLineEnd();
    put("#define MANAGER_FIND_PAIR(what, first, second) do { ");
    put("int i; ");
    put("MANAGER_FIND(what, first, i); ");
    put("if (i < " + MANAGER_STATE + "->what ## _count - 1) { ");
    put("second = " + MANAGER_STATE + "->what[i + 1]; ");
    put("} else { ");
    put("second = NULL; ");
    put("} ");
    put("} while (0)");
    putLineEnd();
    put("#define MANAGER_CONTAINS(what, value, boolean) do { ");
    put("int i; ");
    put("MANAGER_FIND(what, value, i); ");
    put("boolean = i < " + MANAGER_STATE + "->what ## _count; ");
    put("} while (0)");
    putLineEnd();
    putBlankLine();

    // Functions used for operations on string expressions.
    putLine("char* string_create(const char* str) {");
    incIndent();
    putLine("char* clone = malloc(strlen(str) + 1);");
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
    putStatement("free(str)");
    if (profile) {
      putStatement("string_frees += 1");
    }
    decIndent();
    putLine("}");
    putBlankLine();
    putLine("char* string_concat(char* str1, char* str2) {");
    incIndent();
    putStatement("char* concat = malloc(strlen(str1) + strlen(str2) + 1)");
    putStatement("strcpy(concat, str1)");
    putStatement("strcat(concat, str2)");
    putStatement("free(str1)");
    putStatement("free(str2)");
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
    putStatement("free(str)");
    if (profile) {
      putStatement("string_frees += 1");
    }
    decIndent();
    putLine("}");
    putBlankLine();
    putLine("char* string_from_int(int value) {");
    incIndent();
    putStatement("char* str = malloc(12)");
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
    putStatement("free(str1)");
    putStatement("free(str2)");
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

    // Utility function for atomically setting an integer to the maximum of its current value and a given value.
    if (!disableConcurrency) {
      putBlankLine();
      putLine("void atomic_store_max(atomic_ulong* value, unsigned long new_value) {");
      incIndent();
      putStatement("unsigned long old_value = atomic_load(value)");
      putWhile("new_value > old_value", () -> {
          putIf("atomic_compare_exchange_weak(value, &old_value, new_value)", () -> {
            putStatement("break");
          });
      });
      decIndent();
      putLine("}");
    }

    // Generate environment managers and the main function to a different string, so that we can
    // insert record cloner and cleaner functions later.
    String headerCode = code;
    code = "";

    // Generate managers for all process' environments.
    // Each manager function receives the environment to be operated on and the manager's state.
    // If clone is false, then the manager will free the environment and all its records,
    // recursively. Otherwise, then the manager will clone the environment and all
    // its records, and return the new environment.
    inManager = true;
    for (Map.Entry<String, IRProcess> entry : ir.getProcesses().entrySet()) {
      String processName = entry.getKey();
      IRProcess process = entry.getValue();
      putLine(
          "struct environment* env_manager_"
              + processName
              + "(struct environment* env, struct manager_state* "
              + MANAGER_STATE
              + ", int clone) {");
      incIndent();

      // Start by pushing a new type variable frame to the manager.
      putManagerPushProcessTypes(process, "env");

      putIfElse(
          "clone",
          () -> {
            putStatement("struct environment* new_env");

            // The first thing we do is check if the environment has already been allocated.
            // If so, we just return it.
            putManagerFindEnvironmentPair("env", "new_env");
            putIf(
                "new_env != NULL",
                () -> {
                  putManagerPopProcessTypes(process);
                  putStatement("return new_env");
                });

            // Otherwise, we allocate it and put it in the manager.
            putAllocEnvironment("new_env", processName);
            putManagerPushEnvironmentPair("env", "new_env");
            putAssign(environmentEndPoints("new_env"), environmentEndPoints("env"));
            for (int i = 0; i < process.getRecordCount(); ++i) {
              putCloneRecord(process.getRecordType(i), record("env", i), record("new_env", i));
            }
            for (int i = 0; i < process.getExponentialCount(); ++i) {
              String oldExponential = exponential("env", process.getRecordCount(), i);
              String newExponential = exponential("new_env", process.getRecordCount(), i);
              switchTypeId(
                  process.getExponentialType(i),
                  () -> {
                    putAssign(oldExponential, newExponential);
                  },
                  () -> {
                    putAssign(oldExponential, newExponential);
                  },
                  () -> {
                    putCloneExponential(oldExponential, newExponential);
                  });
            }
            for (int i = 0; i < process.getTypeVariableCount(); ++i) {
              putAssign(
                  type("new_env", process.getRecordCount(), process.getExponentialCount(), i),
                  type("env", process.getRecordCount(), process.getExponentialCount(), i));
            }
            putManagerPopProcessTypes(process);
            putStatement("return new_env");
          },
          () -> {
            // Check if the environment has already been cleaned.
            // If so, return NULL.
            putLine("int found;");
            putManagerContainsEnvironment("env", "found");
            putIf(
                "found",
                () -> {
                  putManagerPopProcessTypes(process);
                  putStatement("return NULL");
                });

            // Mark it as cleaned.
            putManagerPushEnvironment("env");

            // Clean all records and exponentials in the environment.
            for (int i = 0; i < process.getRecordCount(); ++i) {
              putCleanRecord(process.getRecordType(i), record("env", i));
            }
            for (int i = 0; i < process.getExponentialCount(); ++i) {
              putCleanExponential(
                  process.getExponentialType(i), exponential("env", process.getRecordCount(), i));
            }

            // Free the environment's memory.
            putFreeEnvironment("env");
            putManagerPopProcessTypes(process);
          });
      decIndent();
      putLine("}");
      putBlankLine();
    }
    inManager = false;

    // Execution function.
    putLine("void* thread(void* entry);");
    putBlankLine();
    putLine("void executor(struct task* entry) {");
    incIndent();

    // Define registers.
    putStatement("struct task* " + TASK);
    putStatement("struct task* " + TMP_TASK);
    putStatement("struct environment* " + ENV);
    putStatement("struct environment* " + TMP_ENV);
    putStatement("void* " + TMP_CONT);
    putStatement("struct record* " + TMP_RECORD);
    putStatement("struct exponential* " + TMP_EXPONENTIAL);
    if (!disableConcurrency) {
      putStatement("pthread_t " + TMP_THREAD);
    }
    putStatement("struct cell* " + TMP_CELL);
    putStatement("struct manager_state* " + MANAGER_STATE);
    putBlankLine();

    // Initialize the manager.
    putAssign(MANAGER_STATE, "calloc(1, sizeof(struct manager_state))");
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
    putStatement("free(" + MANAGER_STATE + ")");

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

    // Generate the record buffer managers.
    // Since these might depend on other managers, we must forward declare them.
    String mainCode = code;
    String declarationsCode = "";
    String definitionsCode = "";
    Set<String> generatedRecordManagers = new HashSet<>();
    boolean generatedNewManagers = true;
    while (generatedNewManagers) {
      generatedNewManagers = false;

      Set<IRType> usedRecordManagers = this.usedRecordManagers;
      this.usedRecordManagers = new HashSet<>();

      for (IRType type : usedRecordManagers) {
        if (!generatedRecordManagers.add(recordBufferManagerName(type))) {
          continue;
        }

        code = declarationsCode;
        putRecordBufferManagerDeclaration(type);
        declarationsCode = code;

        code = definitionsCode;
        putRecordBufferManagerDefinition(type);
        putBlankLine();
        definitionsCode = code;

        generatedNewManagers = true;
      }
    }

    // Concatenate the sections.
    code = headerCode;
    code += declarationsCode;
    putBlankLine();
    code += definitionsCode;
    code += mainCode;
    return code;
  }

  // ================================= IR instruction visitors ==================================

  private void visitBlock(String procName, IRBlock block) {
    genLabelCountInBlock = 0;
    this.procName = procName;
    this.blockName = Optional.ofNullable(block.getLabel());

    for (IRInstruction instruction : block.getInstructions()) {
      visitInstruction(instruction);
    }
  }

  private void visitInstruction(IRInstruction instruction) {
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
              putIf(
                  decrementAtomic(environmentEndPoints()) + " == 0", () -> putFreeEnvironment(ENV));
            } else {
              entryCall = false;
            }
            putAllocEnvironment(ENV, instruction.getProcessName());
          } else {
            putAllocEnvironment(TMP_ENV, instruction.getProcessName());

            // Bind the arguments to the new environment
            for (LinearArgument arg : instruction.getLinearArguments()) {
              putAssign(record(TMP_ENV, arg.getTargetRecord()), record(ENV, arg.getSourceRecord()));
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
                  typeInitializer(arg.getSourceType()));
            }

            putIf(
                decrementAtomic(environmentEndPoints(ENV)) + " == 0",
                () -> putFreeEnvironment(ENV));
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
    if (optimizeTailCalls && instruction.getProcessName().equals(procName) && sameTypes) {
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
  public void visit(IRForward i) {
    // Copy the buffer from the negative record to the positive record.
    putStatement(
        "memcpy("
            + buffer(i.getPosRecord())
            + " + "
            + written(i.getPosRecord())
            + ", "
            + buffer(i.getNegRecord())
            + " + "
            + read(i.getNegRecord())
            + ", "
            + written(i.getNegRecord())
            + " - "
            + read(i.getNegRecord())
            + ")");
    putStatement(
        written(i.getPosRecord())
            + " += "
            + written(i.getNegRecord())
            + " - "
            + read(i.getNegRecord()));

    // Set the continuation of the positive record to the continuation of the negative record.
    putAssign(TMP_CONT, recordCont(i.getPosRecord()));
    putAssign(TMP_ENV, recordContEnv(i.getPosRecord()));
    putAssign(recordCont(i.getPosRecord()), recordCont(i.getNegRecord()));
    putAssign(recordContEnv(i.getPosRecord()), recordContEnv(i.getNegRecord()));
    putAssign(recordContRecord(i.getPosRecord()), recordContRecord(i.getNegRecord()));

    // Overwrite the negative record on its continuation environment with the positive record.
    // We only do this if the negative record has a continuation environment.
    putAssign(TMP_RECORD, record(i.getNegRecord()));
    putIf(
        recordContEnv(i.getNegRecord()) + " != NULL",
        () -> {
          putAssign(
              record(recordContEnv(i.getNegRecord()), recordContRecord(i.getNegRecord())),
              record(i.getPosRecord()));
        });

    // Decrement the end points and free the environment if necessary.
    putIfElse(
        decrementAtomic(environmentEndPoints()) + " == 0",
        () -> putFreeEnvironment(ENV),
        () -> {
          // If the environment was not freed, and if the records won't be used in this environment
          // anymore, we need to remove their bindings. This is necessary to prevent them from being
          // cloned or freed again later on.

          // The negative record will be deleted, but it's binding may now point to the positive
          // record. If it doesn't, then we set it to null.
          putIf(
              TMP_RECORD + " == " + record(i.getNegRecord()),
              () -> {
                putAssign(record(i.getNegRecord()), "NULL");
              });

          // The positive record will only be needed if either the environment we're going to jump
          // to or its continuation environment are the current environment. If not, we also need to
          // remove its binding from the current environment.
          putIf(
              ENV + " != " + recordContEnv(i.getPosRecord()) + " && " + ENV + " != " + TMP_ENV,
              () -> {
                putAssign(record(i.getPosRecord()), "NULL");
              });
        });

    // Finally, free the negative record and jump to the continuation.
    putFreeRecord(TMP_RECORD);
    putAssign(ENV, TMP_ENV);
    putComputedGoto(TMP_CONT);
  }

  @Override
  public void visit(IRFlipForward i) {
    // First of all, we need to check if we need to, and if so, expand the buffer of the negative
    // record.
    // This is a pessimistic check, as we don't know the actual size of the buffer at runtime.
    String currentSize = size(recordType(i.getNegRecord()));
    String desiredSize =
        currentSize + " + " + written(i.getPosRecord()) + " - " + read(i.getPosRecord());
    putIf(
        currentSize + " < " + desiredSize,
        () -> {
          putReallocRecord(record(i.getNegRecord()), desiredSize);
        });

    // Copy the unread data on the positive buffer into the negative record's buffer.
    putStatement(
        "memcpy("
            + buffer(i.getNegRecord())
            + " + "
            + written(i.getNegRecord())
            + ", "
            + buffer(i.getPosRecord())
            + " + "
            + read(i.getPosRecord())
            + ", "
            + written(i.getPosRecord())
            + " - "
            + read(i.getPosRecord())
            + ")");
    putStatement(
        written(i.getNegRecord())
            + " += "
            + written(i.getPosRecord())
            + " - "
            + read(i.getPosRecord()));

    // Set the continuation of the negative record to the continuation of the positive record.
    putAssign(TMP_CONT, recordCont(i.getNegRecord()));
    putAssign(TMP_ENV, recordContEnv(i.getNegRecord()));
    putAssign(recordCont(i.getNegRecord()), recordCont(i.getPosRecord()));
    putAssign(recordContEnv(i.getNegRecord()), recordContEnv(i.getPosRecord()));
    putAssign(recordContRecord(i.getNegRecord()), recordContRecord(i.getPosRecord()));

    // Overwrite the positive record on its continuation environment with the negative record.
    // We only do this if the positive record has a continuation environment.
    putAssign(TMP_RECORD, record(i.getPosRecord()));
    putIf(
        recordContEnv(i.getPosRecord()) + " != NULL",
        () -> {
          putAssign(
              record(recordContEnv(i.getPosRecord()), recordContRecord(i.getPosRecord())),
              record(i.getNegRecord()));
        });

    // Decrement the end points and free the environment if necessary.
    putIfElse(
        decrementAtomic(environmentEndPoints()) + " == 0",
        () -> putFreeEnvironment(ENV),
        () -> {
          // If the environment was not freed, and if the records won't be used in this environment
          // anymore, we need to remove their bindings. This is necessary to prevent them from being
          // cloned or freed again later on.

          // The positive record will be deleted, but it's binding may now point to the negative
          // record. If it doesn't, then we set it to null.
          putIf(
              TMP_RECORD + " == " + record(i.getPosRecord()),
              () -> {
                putAssign(record(i.getPosRecord()), "NULL");
              });

          // The negative record will only be needed if either the environment we're going to jump
          // to or its continuation environment are the current environment. If not, we also need to
          // remove its binding from the current environment.
          putIf(
              ENV + " != " + recordContEnv(i.getNegRecord()) + " && " + ENV + " != " + TMP_ENV,
              () -> {
                putAssign(record(i.getNegRecord()), "NULL");
              });
        });

    // Finally, free the positive record and jump to the continuation.
    putFreeRecord(TMP_RECORD);
    putAssign(ENV, TMP_ENV);
    putComputedGoto(TMP_CONT);
  }

  @Override
  public void visit(IRFlip i) {
    String label = makeLabel("flip");

    putAssign(TMP_CONT, recordCont(i.getRecord()));
    putAssign(TMP_ENV, recordContEnv(i.getRecord()));

    putAssign(recordCont(i.getRecord()), labelAddress(label));
    putAssign(recordContEnv(i.getRecord()), ENV);
    putAssign(recordContRecord(i.getRecord()), i.getRecord());

    putAssign(ENV, TMP_ENV);
    putComputedGoto(TMP_CONT);
    putLabel(label);
  }

  @Override
  public void visit(IRReturn i) {
    putAssign(TMP_CONT, recordCont(i.getRecord()));
    putAssign(TMP_ENV, recordContEnv(i.getRecord()));

    putAssign(recordContEnv(i.getRecord()), "NULL");

    // If the record won't be used anymore, we need to remove its binding from the current
    // environment.
    // This is necessary to prevent it from being cloned or freed again later on.
    //
    // The record can be unbound from the current environment if either:
    // - its continuation environment is not the current environment;
    // - or its continuation environment is the current environment, but its record is not the same.
    putIf(
        TMP_ENV + " != " + ENV + " || " + recordContRecord(i.getRecord()) + " != " + i.getRecord(),
        () -> {
          putAssign(record(ENV, i.getRecord()), "NULL");
        });

    putIf(decrementAtomic(environmentEndPoints()) + " == 0", () -> putFreeEnvironment(ENV));

    putAssign(ENV, TMP_ENV);
    putComputedGoto(TMP_CONT);
  }

  @Override
  public void visit(IRPopSession instruction) {
    putAssign(record(instruction.getArgRecord()), popRecord(instruction.getRecord()));
  }

  @Override
  public void visit(IRPushSession instruction) {
    putPushRecord(instruction.getRecord(), record(instruction.getArgRecord()));
  }

  @Override
  public void visit(IRPopTag instruction) {
    putLine("switch (" + popTag(instruction.getRecord()) + ") {");
    incIndent();

    // We'll take the end points of all other cases for each case, since these paths won't be taken
    Integer totalEndPoints = 0;
    for (Map.Entry<Integer, IRPopTag.Case> entry : instruction.getCases().entrySet()) {
      totalEndPoints += entry.getValue().getEndPoints();
    }

    for (Map.Entry<Integer, IRPopTag.Case> entry : instruction.getCases().entrySet()) {
      putLine("case " + entry.getKey() + ":");
      incIndent();
      putAssign(
          environmentEndPoints(),
          environmentEndPoints() + " - " + (totalEndPoints - entry.getValue().getEndPoints()));
      putConstantGoto(blockLabel(entry.getValue().getLabel()));
      decIndent();
    }

    decIndent();
    putLine("}");
  }

  @Override
  public void visit(IRPushTag instruction) {
    putPushTag(instruction.getRecord(), instruction.getTag());
  }

  @Override
  public void visit(IRPopClose instruction) {
    // Do nothing
  }

  @Override
  public void visit(IRPushClose instruction) {
    // Do nothing
  }

  @Override
  public void visit(IRNewSession instruction) {
    putAllocRecord(record(instruction.getRecord()), recordType(instruction.getRecord()));
    putAssign(
        recordCont(instruction.getRecord()), labelAddress(blockLabel(instruction.getLabel())));
    putAssign(recordContEnv(instruction.getRecord()), ENV);
    putAssign(recordContRecord(instruction.getRecord()), instruction.getRecord());
    putAssign(read(instruction.getRecord()), 0);
    putAssign(written(instruction.getRecord()), 0);
  }

  @Override
  public void visit(IRFreeSession instruction) {
    putFreeRecord(record(instruction.getRecord()));
    putAssign(record(instruction.getRecord()), "NULL");
  }

  @Override
  public void visit(IRNextTask instruction) {
    putIf(decrementAtomic(environmentEndPoints()) + " == 0", () -> putFreeEnvironment(ENV));
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
    putIfElse(
        expression(instruction.getExpression()),
        () -> {
          putAssign(
              environmentEndPoints(),
              environmentEndPoints() + " - " + instruction.getOtherwise().getEndPoints());
          putConstantGoto(blockLabel(instruction.getThen().getLabel()));
        },
        () -> {
          putAssign(
              environmentEndPoints(),
              environmentEndPoints() + " - " + instruction.getThen().getEndPoints());
          putConstantGoto(blockLabel(instruction.getOtherwise().getLabel()));
        });
  }

  @Override
  public void visit(IRPrint instruction) {
    generatePrint(instruction.getExpression(), instruction.hasNewLine());
  }

  @Override
  public void visit(IRScan instruction) {
    IRType irType = instruction.getType();
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
          "Unsupported type for IRScan: " + instruction.getType().getClass().getName());
    }

    if (promote && (!optimizePrimitiveExponentials || irType instanceof IRStringT)) {
      putAllocExponential(TMP_EXPONENTIAL);
      putAssign(exponentialRefCount(TMP_EXPONENTIAL), 1);
      putAllocRecord(exponentialRecord(TMP_EXPONENTIAL), irType);
      putAssign(read(exponentialRecord(TMP_EXPONENTIAL)), 0);
      putAssign(written(exponentialRecord(TMP_EXPONENTIAL)), 0);
      putAssign(recordContEnv(exponentialRecord(TMP_EXPONENTIAL)), "NULL");
      String recordBufferManagerName = recordBufferManagerName(irType);
      putAssign(
          exponentialManager(TMP_EXPONENTIAL),
          recordBufferManagerName.isEmpty() ? "NULL" : ("&" + recordBufferManagerName));

      putPush(exponentialRecord(TMP_EXPONENTIAL), cType, cValue);
      putPushExponential(instruction.getRecord(), TMP_EXPONENTIAL);
    } else {
      putPush(instruction.getRecord(), cType, cValue);
    }
  }

  @Override
  public void visit(IRPushExpression instruction) {
    if (instruction.isExponential()
        && (!optimizePrimitiveExponentials
            || instruction.getExpression().getType() instanceof IRStringT)) {
      putAllocExponential(TMP_EXPONENTIAL);
      putAssign(exponentialRefCount(TMP_EXPONENTIAL), 1);
      putAllocRecord(exponentialRecord(TMP_EXPONENTIAL), instruction.getExpression().getType());
      putAssign(read(exponentialRecord(TMP_EXPONENTIAL)), 0);
      putAssign(written(exponentialRecord(TMP_EXPONENTIAL)), 0);
      putAssign(recordContEnv(exponentialRecord(TMP_EXPONENTIAL)), "NULL");
      String recordBufferManagerName =
          recordBufferManagerName(instruction.getExpression().getType());
      putAssign(
          exponentialManager(TMP_EXPONENTIAL),
          recordBufferManagerName.isEmpty() ? "NULL" : ("&" + recordBufferManagerName));

      putPush(
          exponentialRecord(TMP_EXPONENTIAL),
          cType(instruction.getExpression().getType()),
          expression(instruction.getExpression()));
      putPushExponential(instruction.getRecord(), TMP_EXPONENTIAL);
    } else {
      putPush(
          instruction.getRecord(),
          cType(instruction.getExpression().getType()),
          expression(instruction.getExpression()));
    }
  }

  @Override
  public void visit(IRPushType instruction) {
    putPushType(instruction.getRecord(), instruction.getType());
    putPushPolarity(instruction.getRecord(), instruction.isPositive());
  }

  @Override
  public void visit(IRPopType instruction) {
    putAssign(type(instruction.getArgType()), popType(instruction.getRecord()));
    putIfElse(
        popPolarity(instruction.getRecord()),
        () -> putConstantGoto(blockLabel(instruction.getPositiveLabel())),
        () -> putConstantGoto(blockLabel(instruction.getNegativeLabel())));
  }

  @Override
  public void visit(IRNewExponential instruction) {
    Runnable forInt =
        () -> {
          putAssign(
              exponentialInteger(instruction.getExponential()),
              pop(instruction.getRecord(), "int"));
          putFreeRecord(record(instruction.getRecord()));
        };

    Runnable forBool =
        () -> {
          putAssign(
              exponentialBool(instruction.getExponential()),
              pop(instruction.getRecord(), "unsigned char"));
          putFreeRecord(record(instruction.getRecord()));
        };

    Runnable forOther =
        () -> {
          // Initialize a new exponential structure.
          putAllocExponential(exponential(instruction.getExponential()));
          putAssign(exponentialRefCount(instruction.getExponential()), 1);
          putAssign(
              exponentialRecord(instruction.getExponential()), record(instruction.getRecord()));
          String recordBufferManagerName =
              recordBufferManagerName(recordType(instruction.getRecord()));
          putAssign(
              exponentialManager(instruction.getExponential()),
              recordBufferManagerName.isEmpty() ? "NULL" : ("&" + recordBufferManagerName));
        };

    switchTypeId(recordType(instruction.getRecord()), forInt, forBool, forOther);
    putAssign(record(instruction.getRecord()), "NULL");
  }

  @Override
  public void visit(IRPushExponential instruction) {
    Runnable forInt =
        () -> {
          putPush(instruction.getRecord(), "int", exponentialInteger(instruction.getExponential()));
        };

    Runnable forBool =
        () -> {
          putPush(
              instruction.getRecord(),
              "unsigned char",
              exponentialBool(instruction.getExponential()));
        };

    Runnable forOther =
        () -> {
          putPushExponential(instruction.getRecord(), exponential(instruction.getExponential()));
        };

    switchTypeId(exponentialType(instruction.getExponential()), forInt, forBool, forOther);
  }

  @Override
  public void visit(IRPopExponential instruction) {
    Runnable forInt =
        () -> {
          putAssign(
              exponentialInteger(instruction.getArgExponential()),
              pop(instruction.getRecord(), "int"));
        };

    Runnable forBool =
        () -> {
          putAssign(
              exponentialBool(instruction.getArgExponential()),
              pop(instruction.getRecord(), "unsigned char"));
        };

    Runnable forOther =
        () -> {
          putAssign(
              exponential(instruction.getArgExponential()),
              popExponential(instruction.getRecord()));
        };

    switchTypeId(exponentialType(instruction.getArgExponential()), forInt, forBool, forOther);
  }

  @Override
  public void visit(IRCallExponential instruction) {
    IRType type = exponentialType(instruction.getExponential());

    Runnable forIntAndBool =
        () -> {
          putAllocRecord(record(instruction.getArgRecord()), type);
          putAssign(read(instruction.getArgRecord()), 0);
          putAssign(written(instruction.getArgRecord()), 0);
          putAssign(recordContEnv(instruction.getArgRecord()), "NULL");
        };

    Runnable forInt =
        () -> {
          forIntAndBool.run();
          putPush(
              instruction.getArgRecord(), "int", exponentialInteger(instruction.getExponential()));
        };

    Runnable forBool =
        () -> {
          forIntAndBool.run();
          putPush(
              instruction.getArgRecord(),
              "unsigned char",
              exponentialBool(instruction.getExponential()));
        };

    Runnable forOther =
        () -> {
          Runnable cloneRecord =
              () -> {
                putManagerReset();
                putManagerPushProcessTypes();
                putIfElse(
                    recordContEnv(exponentialRecord(instruction.getExponential())) + " != NULL",
                    () -> {
                      // The exponential record has a continuation environment, which means it
                      // hasn't been
                      // closed yet. In that case, just clone the whole environment, and fetch the
                      // new
                      // record from the new environment.
                      //
                      // The record's index in the environment is always 0.
                      putCloneEnvironment(
                          recordContEnv(exponentialRecord(instruction.getExponential())), TMP_ENV);
                      putAssign(record(instruction.getArgRecord()), record(TMP_ENV, 0));
                    },
                    () -> {
                      // Otherwise, the record has already been closed. In that case, we allocate a
                      // new
                      // record with a buffer big enough to accomodate all written data, and then,
                      // clone
                      // the buffer using the record buffer manager.
                      putAllocRecord(
                          record(instruction.getArgRecord()),
                          written(exponentialRecord(instruction.getExponential())));
                      putAssign(
                          read(instruction.getArgRecord()),
                          read(exponentialRecord(instruction.getExponential())));
                      putAssign(
                          written(instruction.getArgRecord()),
                          written(exponentialRecord(instruction.getExponential())));
                      putAssign(recordContEnv(instruction.getArgRecord()), "NULL");

                      putIfElse(
                          exponentialManager(instruction.getExponential()) + " != NULL",
                          () -> {
                            putCloneRecordBuffer(
                                exponentialManager(instruction.getExponential()),
                                exponentialRecord(instruction.getExponential()),
                                record(instruction.getArgRecord()));
                          },
                          () -> {
                            putCloneRecordBuffer(
                                "",
                                exponentialRecord(instruction.getExponential()),
                                record(instruction.getArgRecord()));
                          });
                    });
              };

          if (instruction.shouldDecreaseRefCount()) {
            putIfElse(
                decrementAtomic(exponentialRefCount(instruction.getExponential())) + " == 0",
                () -> {
                  // If the reference count is 1, and we would decrement it, we don't clone the
                  // record.
                  // Instead, we just use the record directly and deallocate the wrapping
                  // exponential.
                  putAssign(
                      record(instruction.getArgRecord()),
                      exponentialRecord(instruction.getExponential()));
                  putFreeExponential(exponential(instruction.getExponential()));
                },
                () -> {
                  cloneRecord.run();
                });
          } else {
            cloneRecord.run();
          }
        };

    switchTypeId(type, forInt, forBool, forOther);
  }

  @Override
  public void visit(IRIncRefExponential instruction) {
    switchTypeId(
        exponentialType(instruction.getExponential()),
        () -> {},
        () -> {},
        () -> {
          putIncrementAtomic(exponentialRefCount(instruction.getExponential()));
        });
  }

  @Override
  public void visit(IRDecRefExponential instruction) {
    switchTypeId(
        exponentialType(instruction.getExponential()),
        () -> {},
        () -> {},
        () -> {
          putDecRefExponential(
              exponential(instruction.getExponential()),
              exponentialType(instruction.getExponential()),
              true);
        });
  }

  @Override
  public void visit(IRDetachExponential instruction) {
    switchTypeId(
        exponentialType(instruction.getExponential()),
        () -> {},
        () -> {},
        () -> {
          putAssign(exponential(instruction.getExponential()), "NULL");
        });
  }

  @Override
  public void visit(IRIncRefCell instruction) {
    putIncrementAtomic(cellRefCount(peekCell(instruction.getRecord())));
  }

  @Override
  public void visit(IRDecRefCell instruction) {
    putIf(
        decrementAtomic(cellRefCount(peekCell(instruction.getRecord()))) + " == 0",
        () -> {
          putManagerReset();
          putManagerPushProcessTypes();
          putCleanRecord(recordType(instruction.getRecord()), record(instruction.getRecord()));
        });
  }

  @Override
  public void visit(IRPushUnfold instruction) {
    new IRFlip(instruction.getRecord()).accept(this);
    putAssign(written(instruction.getRecord()), 0);
  }

  @Override
  public void visit(IRPopUnfold instruction) {
    new IRFlip(instruction.getRecord()).accept(this);
    putAssign(read(instruction.getRecord()), 0);
  }

  @Override
  public void visit(IRCleanRecord instruction) {
    // Clean the remainder of the record.
    // The clean call will delete any environments found through links in the record.
    putManagerReset();
    putManagerPushProcessTypes();
    putCleanRecord(recordType(instruction.getRecord()), record(instruction.getRecord()));
  }

  @Override
  public void visit(IRPushCell instruction) {
    // Push a new cell structure and initialize its mutex.
    putAllocCell(TMP_CELL);
    putAssign(cellRefCount(TMP_CELL), 1);
    putAssign(cellRecord(TMP_CELL), record(instruction.getArgRecord()));
    putPushCell(instruction.getRecord(), TMP_CELL);
    putAssign(record(instruction.getArgRecord()), "NULL");
  }

  @Override
  public void visit(IRTakeCell instruction) {
    // Lock the mutex and extract the record.
    if (!disableConcurrency) {
      putStatement("pthread_mutex_lock(&" + cellMutex(peekCell(instruction.getRecord())) + ")");
    }
    putAssign(record(instruction.getArgRecord()), cellRecord(peekCell(instruction.getRecord())));
    putAssign(cellRecord(peekCell(instruction.getRecord())), "NULL");
  }

  @Override
  public void visit(IRPutCell instruction) {
    // Put the record into the cell and unlock the mutex.
    putAssign(cellRecord(peekCell(instruction.getRecord())), record(instruction.getArgRecord()));
    putAssign(record(instruction.getArgRecord()), "NULL");
    if (!disableConcurrency) {
      putStatement("pthread_mutex_unlock(&" + cellMutex(peekCell(instruction.getRecord())) + ")");
    }
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

  private String environmentManager(String env) {
    return env + "->manager";
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
    if (inManager) {
      return managerType(type);
    } else {
      return type(ENV, type);
    }
  }

  private String typeInitializer(IRType type) {
    String name = recordBufferManagerName(type);
    String managerValue = name.isEmpty() ? "NULL" : "&" + name;
    return "(struct type){.size = "
        + size(type)
        + (optimizePrimitiveExponentials ? ", .id = " + typeId(type) : "")
        + ", .manager = "
        + managerValue
        + "}";
  }

  private String typeManager(String type) {
    return type + ".manager";
  }

  private String typeSize(String type) {
    return type + ".size";
  }

  private String typeId(IRType type) {
    if (type instanceof IRVarT) {
      return typeId(type(((IRVarT) type).getType()));
    } else if (type instanceof IRIntT) {
      return Integer.toString(TYPE_ID_INT);
    } else if (type instanceof IRBoolT) {
      return Integer.toString(TYPE_ID_BOOL);
    } else {
      return Integer.toString(TYPE_ID_OTHER);
    }
  }

  private String typeId(String type) {
    return type + ".id";
  }

  private void switchTypeId(IRType type, Runnable forInt, Runnable forBool, Runnable forOther) {
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

  private String read(String record) {
    return "(" + record + ")->read";
  }

  private String read(int record) {
    return "READ(" + record + ")";
  }

  private String written(String record) {
    return "(" + record + ")->written";
  }

  private String written(int record) {
    return "WRITTEN(" + record + ")";
  }

  private String buffer(int record) {
    return "BUFFER(" + record + ")";
  }

  private String buffer(String record) {
    return "(" + record + ")->buffer";
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

  private IRType recordType(int record) {
    return ir.getProcesses().get(procName).getRecordType(record);
  }

  private String recordCont(String record) {
    return "(" + record + ")" + "->cont";
  }

  private String recordCont(String env, int record) {
    return recordCont(record(env, record));
  }

  private String recordCont(int record) {
    return recordCont(ENV, record);
  }

  private String recordContEnv(String record) {
    return "(" + record + ")" + "->cont_env";
  }

  private String recordContEnv(String env, int record) {
    return recordContEnv(record(env, record));
  }

  private String recordContEnv(int record) {
    return recordContEnv(ENV, record);
  }

  private String recordContRecord(String record) {
    return "(" + record + ")" + "->cont_record";
  }

  private String recordContRecord(String env, int record) {
    return recordContRecord(record(env, record));
  }

  private String recordContRecord(int record) {
    return recordContRecord(ENV, record);
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

  private IRType exponentialType(int exponential) {
    return ir.getProcesses().get(procName).getExponentialType(exponential);
  }

  private String exponentialRecord(String exponential) {
    return exponential + "->record";
  }

  private String exponentialInteger(int exponential) {
    // Hacky, but works as long as sizeof(int) < sizeof(struct exponential*)
    // We're just casting the address of the exponential to an int
    return "*(int*)(&" + exponential(exponential) + ")";
  }

  private String exponentialBool(int exponential) {
    // Hacky, but works as long as sizeof(unsigned char) < sizeof(struct exponential*)
    // We're just casting the address of the exponential to an unsigned char
    return "*(unsigned char*)(&" + exponential(exponential) + ")";
  }

  private String exponentialRecord(int exponential) {
    return exponentialRecord(exponential(exponential));
  }

  private String exponentialRefCount(String exponential) {
    return exponential + "->ref_count";
  }

  private String exponentialRefCount(int exponential) {
    return exponentialRefCount(exponential(exponential));
  }

  private String exponentialManager(String exponential) {
    return exponential + "->manager";
  }

  private String exponentialManager(int exponential) {
    return exponentialManager(exponential(exponential));
  }

  private String managerType(int index) {
    return MANAGER_STATE + "->type[" + MANAGER_STATE + "->type_count - 1 - " + index + "]";
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

  private String access(String buffer, String type) {
    return "(*(" + type + "*)(" + buffer + "))";
  }

  private String pop(int index, String type) {
    return "POP(" + index + ", " + type + ")";
  }

  private String pop(int index, IRType type) {
    return pop(index, cType(type));
  }

  private String popTag(int index) {
    return pop(index, "unsigned char");
  }

  private String popRecord(int index) {
    return pop(index, "struct record*");
  }

  private String popExponential(int index) {
    return pop(index, "struct exponential*");
  }

  private String popPolarity(int index) {
    return pop(index, "unsigned char");
  }

  private String popType(int index) {
    return pop(index, "struct type");
  }

  private String peek(int index, String type) {
    return "PEEK(" + index + ", " + type + ")";
  }

  private String peekCell(int index) {
    return peek(index, "struct cell*");
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

  private void putAllocEnvironment(
      String var, String manager, String recordCount, String exponentialCount, String typeCount) {
    putAssign(
        var,
        "calloc(1, sizeof(struct environment) + "
            + recordCount
            + " * sizeof(struct record*) + "
            + exponentialCount
            + " * sizeof(struct exponential*) + "
            + typeCount
            + " * sizeof(struct type))");
    putAssign(environmentManager(var), "&" + manager);
    if (profile) {
      putIncrementAtomic("env_allocs");
      putIncrementAtomic("env_current");
      putAssignMaxAtomic("env_peak", "env_current");
    }
  }

  private void putAllocEnvironment(
      String var, String manager, int recordCount, int exponentialCount, int typeCount) {
    putAllocEnvironment(
        var,
        manager,
        Integer.toString(recordCount),
        Integer.toString(exponentialCount),
        Integer.toString(typeCount));
  }

  private void putAllocEnvironment(String var, String processName) {
    IRProcess process = ir.getProcesses().get(processName);
    putAllocEnvironment(
        var,
        "env_manager_" + processName,
        process.getRecordCount(),
        process.getExponentialCount(),
        process.getTypeVariableCount());
  }

  private void putFreeEnvironment(String var) {
    if (trace && procName != null) {
      putDebugLn("[endCall(" + procName + ")]");
    }
    putLine("free(" + var + ");");
    if (profile) {
      putIncrementAtomic("env_frees");
      putDecrementAtomic("env_current");
    }
  }

  private void putAllocRecord(String var, String bufferSize) {
    putAssign(var, "malloc(sizeof(struct record) + " + bufferSize + ")");
    if (profile) {
      putIncrementAtomic("record_allocs");
      putIncrementAtomic("record_current");
      putAssignMaxAtomic("record_peak", "record_current");
    }
  }

  private void putAllocRecord(String var, IRType type) {
    putAllocRecord(var, size(type));
  }

  private void putReallocRecord(String var, String bufferSize) {
    putAssign(var, "realloc(" + var + ", sizeof(struct record) + " + bufferSize + ")");
    if (profile) {
      putIncrementAtomic("record_reallocs");
    }
  }

  private void putFreeRecord(String var) {
    putLine("free(" + var + ");");
    if (profile) {
      putIncrementAtomic("record_frees");
      putDecrementAtomic("record_current");
    }
  }

  private void putAllocTask(String var) {
    putAssign(var, "malloc(sizeof(struct task))");
    if (profile) {
      putIncrementAtomic("task_allocs");
    }
  }

  private void putFreeTask(String var) {
    putLine("free(" + var + ");");
    if (profile) {
      putIncrementAtomic("task_frees");
    }
  }

  private void putAllocExponential(String var) {
    putAssign(var, "malloc(sizeof(struct exponential))");
    if (profile) {
      putIncrementAtomic("exponential_allocs");
    }
  }

  private void putFreeExponential(String var) {
    if (trace) {
      putDebugLn("[freeExponential(" + var + ")]");
    }
    putLine("free(" + var + ");");
    if (profile) {
      putIncrementAtomic("exponential_frees");
    }
  }

  private void putDecRefExponential(String var, IRType type, boolean initManager) {
    putIf(
        decrementAtomic(exponentialRefCount(var)) + " == 0",
        () -> {
          if (initManager) {
            putManagerReset();
            putManagerPushProcessTypes();
          }

          // If the exponential has a continuation environment, call its manager.
          // Otherwise, we just call the exponential's manager on its buffer.
          putIfElse(
              recordContEnv(exponentialRecord(var)) + " != NULL",
              () -> {
                putCleanEnvironment(recordContEnv(exponentialRecord(var)));
              },
              () -> {
                putIf(
                    exponentialManager(var) + " != NULL",
                    () -> {
                      putCleanRecordBuffer(exponentialManager(var), exponentialRecord(var));
                    });
                putFreeRecord(exponentialRecord(var));
              });

          putFreeExponential(var);
        });
  }

  private void putCloneEnvironment(String oldEnv, String newEnv) {
    putIfElse(
        oldEnv + " == NULL",
        () -> {
          putAssign(newEnv, "NULL");
        },
        () -> {
          // We call the environment manager to clone the environment.
          putAssign(newEnv, "ENV_MANAGER_CALL_CLONE(" + oldEnv + ")");
        });
  }

  private void putCleanEnvironment(String env) {
    // We call the environment manager to clean the environment.
    putIf(
        env + " != NULL",
        () -> {
          putStatement("ENV_MANAGER_CALL_CLEAN(" + env + ")");
        });
  }

  private void putCloneRecord(IRType recordType, String oldRecord, String newRecord) {
    // If the old record is null, the new record should also be null.
    putIfElse(
        oldRecord + " == NULL",
        () -> {
          putAssign(newRecord, "NULL");
        },
        () -> {
          // Otherwise, we check if the record has already been cloned.
          putManagerFindRecordPair(oldRecord, newRecord);
          putIf(
              newRecord + " == NULL",
              () -> {
                // If not, we allocate a new record, and add it to the manager.
                putAllocRecord(newRecord, recordType);
                putManagerPushRecordPair(oldRecord, newRecord);

                // Copy the state of the old record to the new one.
                putAssign(recordCont(newRecord), recordCont(oldRecord));
                putAssign(recordContRecord(newRecord), recordContRecord(oldRecord));
                putAssign(read(newRecord), read(oldRecord));
                putAssign(written(newRecord), written(oldRecord));
                putCloneEnvironment(recordContEnv(oldRecord), recordContEnv(newRecord));
                putCloneRecordBuffer(recordType, oldRecord, newRecord);
              });
        });
  }

  private void putCloneExponential(String oldExponential, String newExponential) {
    putAssign(newExponential, oldExponential);
    putIf(
        newExponential + " != NULL",
        () -> {
          putIncrementAtomic(exponentialRefCount(newExponential));
        });
  }

  private void putCleanRecord(IRType recordType, String record) {
    putIf(
        record + " != NULL",
        () -> {
          // Check if the record has already been cleaned.
          putLine("int found;");
          putManagerContainsRecord(record, "found");
          putIf(
              "!found",
              () -> {
                // If not, mark it as cleaned.
                putManagerPushRecord(record);

                // Clean its environment.
                putCleanEnvironment(recordContEnv(record));

                // Clean any records or exponentials on its buffer.
                putCleanRecordBuffer(recordType, record);

                // Release the record's memory.
                putFreeRecord(record);
              });
        });
  }

  private void putCleanExponential(IRType recordType, String exponential) {
    putIf(
        exponential + " != NULL",
        () -> {
          putDecRefExponential(exponential, recordType, false);
        });
  }

  private void putAllocCell(String var) {
    putAssign(var, "malloc(sizeof(struct cell))");

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
    putStatement("free(" + var + ")");
    if (profile) {
      putIncrementAtomic("cell_frees");
    }
  }

  private void putLabel(String label) {
    put(label + ":");
    putLineEnd();
  }

  private void putPush(int record, String type, String value) {
    putStatement("PUSH(" + record + ", " + type + ", " + value + ")");
  }

  private void putPush(String record, String type, String value) {
    putStatement("PUSH_RAW(" + record + ", " + type + ", " + value + ")");
  }

  private void putPushTag(int record, int value) {
    putPush(record, "unsigned char", Integer.toString(value));
  }

  private void putPushRecord(int record, String value) {
    putPush(record, "struct record*", value);
  }

  private void putPushExponential(int record, String value) {
    putPush(record, "struct exponential*", value);
  }

  private void putPushPolarity(int record, boolean isPositive) {
    putPush(record, "unsigned char", isPositive ? "1" : "0");
  }

  private void putPushType(int record, IRType type) {
    putPush(record, "struct type", typeInitializer(type));
  }

  private void putPushCell(int record, String cell) {
    putPush(record, "struct cell*", cell);
  }

  private void putAssign(String var, String value) {
    putStatement(var + " = " + value);
  }

  private void putAssign(String var, int value) {
    putStatement(var + " = " + value);
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

  private void putManagerPushRecordPair(String first, String second) {
    putStatement("MANAGER_PUSH_PAIR(record, " + first + ", " + second + ")");
  }

  private void putManagerPushEnvironmentPair(String first, String second) {
    putStatement("MANAGER_PUSH_PAIR(env, " + first + ", " + second + ")");
  }

  private void putManagerFindRecordPair(String first, String second) {
    putStatement("MANAGER_FIND_PAIR(record, " + first + ", " + second + ")");
  }

  private void putManagerFindEnvironmentPair(String first, String second) {
    putStatement("MANAGER_FIND_PAIR(env, " + first + ", " + second + ")");
  }

  private void putManagerPushRecord(String record) {
    putStatement("MANAGER_PUSH(record, " + record + ")");
  }

  private void putManagerPushEnvironment(String env) {
    putStatement("MANAGER_PUSH(env, " + env + ")");
  }

  private void putManagerPushType(String type) {
    putStatement("MANAGER_PUSH(type, " + type + ")");
  }

  private void putManagerPopType() {
    putStatement("MANAGER_POP(type)");
  }

  private void putManagerContainsRecord(String record, String bool) {
    putStatement("MANAGER_CONTAINS(record, " + record + ", " + bool + ")");
  }

  private void putManagerContainsEnvironment(String env, String bool) {
    putStatement("MANAGER_CONTAINS(env, " + env + ", " + bool + ")");
  }

  private void putManagerReset() {
    putStatement("MANAGER_RESET()");
  }

  private void putManagerPushProcessTypes(IRProcess process, String env) {
    // We push them in reverse order, as they will be accessed in reverse order.
    for (int i = process.getTypeVariableCount() - 1; i >= 0; --i) {
      putManagerPushType(type(env, i));
    }
  }

  private void putManagerPopProcessTypes(IRProcess process) {
    for (int i = 0; i < process.getTypeVariableCount(); ++i) {
      putManagerPopType();
    }
  }

  private void putManagerPushProcessTypes() {
    putManagerPushProcessTypes(ir.getProcesses().get(procName), ENV);
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

  private String makeLabel(String type) {
    if (blockName.isEmpty()) {
      return type + "_" + genLabelCountInBlock++ + "_proc_" + procName;
    } else {
      return type + "_" + genLabelCountInBlock++ + "_block_" + procName + "_" + blockName.get();
    }
  }

  // ========================== Type visitor used to determine type size ==========================

  private String size(IRType type) {
    SizeCalculator calc = new SizeCalculator();
    type.accept(calc);
    return calc.size;
  }

  private class SizeCalculator extends IRTypeVisitor {
    private String size = "";
    private int definedTypes = 0;

    @Override
    public void visit(IRType type) {
      throw new UnsupportedOperationException("Unsupported type: " + type.getClass().getName());
    }

    @Override
    public void visit(IRCloseT type) {
      size += "0";
    }

    @Override
    public void visit(IRSessionT type) {
      size += "sizeof(struct record*) + ";
      type.getCont().accept(this);
    }

    @Override
    public void visit(IRTagT type) {
      size += "sizeof(unsigned char)";
      for (IRType choice : type.getChoices()) {
        size += " + (";
        choice.accept(this);
        size += ")";
      }
    }

    @Override
    public void visit(IRRecT type) {
      definedTypes += 1;
      type.getInner().accept(this);
      definedTypes -= 1;
    }

    @Override
    public void visit(IRVarT type) {
      if (type.getType() >= definedTypes) {
        // The variable must refer to a type bound in the environment or manager.
        size += typeSize(type(type.getType()));
      } else {
        // The variable refers to a type bound in the current type, e.g. a recursive type.
        // In that case, since recursive types reset the buffer, we consider their size to be 0.
        size += "0";
      }
    }

    @Override
    public void visit(IRIntT type) {
      size += "sizeof(int)";
    }

    @Override
    public void visit(IRBoolT type) {
      size += "sizeof(unsigned char)";
    }

    @Override
    public void visit(IRStringT type) {
      size += "sizeof(char*)";
    }

    @Override
    public void visit(IRTypeT type) {
      size += "sizeof(struct record*) + sizeof(struct type) + sizeof(unsigned char)";
    }

    @Override
    public void visit(IRExponentialT type) {
      if (optimizePrimitiveExponentials) {
        String id = typeId(type.getInner());
        size += "(" + id + " == " + TYPE_ID_OTHER + " ? " + "sizeof(struct exponential*)" + " : ";
        size += id + " == " + TYPE_ID_INT + " ? sizeof(int) : ";
        size += "sizeof(unsigned char))"; // for TYPE_ID_BOOL
      } else {
        size += "sizeof(struct exponential*)";
      }
    }

    @Override
    public void visit(IRAffineT type) {
      type.getInner().accept(this);
    }

    @Override
    public void visit(IRCellT type) {
      size += "sizeof(struct cell*)";
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
      code += pop(expr.getRecord(), expr.getType());
    }

    @Override
    public void visit(IRExponentialVar expr) {
      String dataRef;

      if (optimizePrimitiveExponentials && (expr.getType() instanceof IRIntT)) {
        dataRef = exponentialInteger(expr.getExponential());
      } else if (optimizePrimitiveExponentials && (expr.getType() instanceof IRBoolT)) {
        dataRef = exponentialBool(expr.getExponential());
      } else {
        // We simply access the exponential data buffer directly.
        final String record = exponentialRecord(expr.getExponential());
        final String dataPtr = record + "->buffer + " + record + "->read";
        dataRef = "*((" + cType(expr.getType()) + "*)(" + dataPtr + "))";
      }

      if (expr.getType() instanceof IRStringT) {
        // If the exponential is a string, it must be cloned, as it will be consumed.
        code += "string_create(" + dataRef + ")";
      } else {
        code += dataRef;
      }
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
      code = "struct record*";
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

  // ========================= Visitor which names record buffer managers =========================

  private String recordBufferManagerName(IRType type) {
    // If the type doesn't require a buffer manager, an empty string is returned.
    RecordBufferManagerNamer namer = new RecordBufferManagerNamer();
    type.accept(namer);
    if (namer.name.isEmpty()) {
      return "";
    }
    usedRecordManagers.add(type);
    return "record_buffer_manager_" + namer.name;
  }

  private class RecordBufferManagerNamer extends IRTypeVisitor {
    private String name = "";

    private void separate() {
      if (!name.isEmpty()) {
        name += "_";
      }
    }

    @Override
    public void visit(IRType type) {
      throw new UnsupportedOperationException(
          "Unsupported type for buffer record manager namer: " + type.getClass().getName());
    }

    @Override
    public void visit(IRIntT type) {}

    @Override
    public void visit(IRBoolT type) {}

    @Override
    public void visit(IRStringT type) {
      separate();
      name += "string";
    }

    @Override
    public void visit(IRSessionT type) {
      separate();
      name += "session_arg";
      type.getArg().accept(this);
      name += "_cont";
      type.getCont().accept(this);
    }

    @Override
    public void visit(IRExponentialT type) {
      if (optimizePrimitiveExponentials
          && (type.getInner() instanceof IRIntT || type.getInner() instanceof IRBoolT)) {
        return; // Primitive exponentials do not require a buffer manager.
      } else if (optimizePrimitiveExponentials && type.getInner() instanceof IRVarT) {
        separate();
        name += "exponential_var" + ((IRVarT) type.getInner()).getType();
      } else {
        separate();
        name += "exponential";
      }
    }

    @Override
    public void visit(IRTagT type) {
      separate();
      name += "tag";
      for (int i = 0; i < type.getChoices().size(); ++i) {
        name += "_choice_" + i;
        type.getChoices().get(i).accept(this);
        name += "_end";
      }
    }

    @Override
    public void visit(IRTypeT type) {
      separate();
      name += "type";
      type.getCont().accept(this);
    }

    @Override
    public void visit(IRRecT type) {
      separate();
      name += "rec";
      type.getInner().accept(this);
    }

    @Override
    public void visit(IRVarT type) {
      separate();
      name += "var" + type.getType();
    }

    @Override
    public void visit(IRAffineT type) {
      type.getInner().accept(this);
    }

    @Override
    public void visit(IRCloseT type) {}

    @Override
    public void visit(IRCellT type) {
      separate();
      name += "cell";
      type.getInner().accept(this);
    }
  }

  // ====================== Visitors used to generate record buffer managers ======================

  private void putRecordBufferManagerDeclaration(IRType type) {
    String name = recordBufferManagerName(type);
    if (name.isEmpty()) {
      // If the name is empty, we don't need a record manAger.
      return;
    }

    putLine(
        "void "
            + name
            + "(const char* old, char* new, int written, int read, struct manager_state* "
            + MANAGER_STATE
            + ");");
  }

  private void putRecordBufferManagerDefinition(IRType type) {
    String name = recordBufferManagerName(type);
    if (name.isEmpty()) {
      // If the name is empty, we don't need a record cloner.
      return;
    }

    inManager = true;
    putLine(
        "void "
            + name
            + "(const char* old, char* new, int written, int read, struct manager_state* "
            + MANAGER_STATE
            + ") {");
    incIndent();
    putIfElse(
        "new == NULL",
        () -> {
          type.accept(new RecordBufferCleaner("old", "written", "read"));
        },
        () -> {
          type.accept(new RecordBufferCloner("old", "new", "written", "read"));
        });
    decIndent();
    putLine("}");
    inManager = false;
  }

  private void putCallRecordBufferManager(
      String manager, String oldBuffer, String newBuffer, String written, String read) {
    if (!manager.isEmpty()) {
      putStatement(
          manager
              + "("
              + oldBuffer
              + ", "
              + newBuffer
              + ", "
              + written
              + ", "
              + read
              + ", "
              + MANAGER_STATE
              + ")");
    }
  }

  private void putCleanRecordBufferCall(
      String manager, String buffer, String written, String read) {
    putCallRecordBufferManager(manager, buffer, "NULL", written, read);
  }

  private void putCleanRecordBufferCall(IRType type, String buffer, String written, String read) {
    putCleanRecordBufferCall(recordBufferManagerName(type), buffer, written, read);
  }

  private void putCleanRecordBuffer(String manager, String record) {
    putCleanRecordBufferCall(manager, buffer(record), written(record), read(record));
  }

  private void putCleanRecordBuffer(IRType type, String record) {
    putCleanRecordBuffer(recordBufferManagerName(type), record);
  }

  private void putCloneRecordBufferCall(
      String manager, String oldBuffer, String newBuffer, String written, String read) {
    putCallRecordBufferManager(manager, oldBuffer, newBuffer, written, read);
  }

  private void putCloneRecordBufferCall(
      IRType type, String oldBuffer, String newBuffer, String written, String read) {
    putCloneRecordBufferCall(recordBufferManagerName(type), oldBuffer, newBuffer, written, read);
  }

  private void putCloneRecordBuffer(String manager, String oldRecord, String newRecord) {
    putStatement(
        "memcpy("
            + buffer(newRecord)
            + " + "
            + read(oldRecord)
            + ", "
            + buffer(oldRecord)
            + " + "
            + read(oldRecord)
            + ", "
            + written(oldRecord)
            + " - "
            + read(oldRecord)
            + ")");
    putCloneRecordBufferCall(
        manager, buffer(oldRecord), buffer(newRecord), written(oldRecord), read(oldRecord));
  }

  private void putCloneRecordBuffer(IRType type, String oldRecord, String newRecord) {
    putCloneRecordBuffer(recordBufferManagerName(type), oldRecord, newRecord);
  }

  private class RecordBufferCloner extends IRTypeVisitor {
    private String oldBuffer;
    private String newBuffer;
    private String written;
    private String read;

    public RecordBufferCloner(String oldBuffer, String newBuffer, String written, String read) {
      this.oldBuffer = oldBuffer;
      this.newBuffer = newBuffer;
      this.written = written;
      this.read = read;
    }

    private void putIfWritten(Runnable action) {
      putIf(written + " > 0", action);
    }

    private void putIfUnread(Runnable action) {
      putIf(read + " <= 0", action);
    }

    private void putIfWrittenAndUnread(Runnable action) {
      putIf(written + " > 0 && " + read + " <= 0", action);
    }

    private void recurse(String manager, String offset) {
      putCloneRecordBufferCall(
          manager,
          oldBuffer + " + " + offset,
          newBuffer + " + " + offset,
          written + " - " + offset,
          read + " - " + offset);
    }

    private void recurse(IRType type, String offset) {
      putCloneRecordBufferCall(
          type,
          oldBuffer + " + " + offset,
          newBuffer + " + " + offset,
          written + " - " + offset,
          read + " - " + offset);
    }

    @Override
    public void visit(IRType type) {
      throw new UnsupportedOperationException(
          "Unsupported type for buffer record cloning: " + type.getClass().getName());
    }

    @Override
    public void visit(IRIntT type) {}

    @Override
    public void visit(IRBoolT type) {}

    @Override
    public void visit(IRStringT type) {
      putIfWrittenAndUnread(
          () -> {
            putAssign(
                access(newBuffer, "char*"), "string_create(" + access(oldBuffer, "char*") + ")");
          });
    }

    @Override
    public void visit(IRSessionT type) {
      putIfWritten(
          () -> {
            putIfUnread(
                () -> {
                  putCloneRecord(
                      type.getArg(),
                      access(oldBuffer, "struct record*"),
                      access(newBuffer, "struct record*"));
                });
            recurse(type.getCont(), "sizeof(struct record*)");
          });
    }

    @Override
    public void visit(IRExponentialT type) {
      putIfWrittenAndUnread(
          () -> {
            switchTypeId(
                type.getInner(),
                () -> {},
                () -> {},
                () -> {
                  putCloneExponential(
                      access(oldBuffer, "struct exponential*"),
                      access(newBuffer, "struct exponential*"));
                });
          });
    }

    @Override
    public void visit(IRTagT type) {
      putIfWritten(
          () -> {
            String tag = access(oldBuffer, "unsigned char");

            putLine("switch (" + tag + ") {");
            incIndent();

            for (int i = 0; i < type.getChoices().size(); ++i) {
              putLine("case " + i + ":");
              incIndent();
              recurse(type.getChoices().get(i), "sizeof(unsigned char)");
              putStatement("break");
              decIndent();
            }

            decIndent();
            putLine("}");
          });
    }

    @Override
    public void visit(IRTypeT type) {
      putIfWrittenAndUnread(
          () -> {
            putManagerPushType(access(oldBuffer + " + sizeof(struct record*)", "struct type"));
            putCloneRecord(
                type.getCont(),
                access(oldBuffer, "struct record*"),
                access(newBuffer, "struct record*"));
            putManagerPopType();
          });
    }

    @Override
    public void visit(IRRecT type) {
      putIfWritten(
          () -> {
            putManagerPushType(typeInitializer(type.getInner()));
            type.getInner().accept(this);
            putManagerPopType();
          });
    }

    @Override
    public void visit(IRVarT type) {
      // Recurse into the manager of the type variable.
      String manager = typeManager(managerType(type.getType()));
      putIf(
          manager + " != NULL",
          () -> {
            recurse(manager, "0");
          });
    }

    @Override
    public void visit(IRCloseT type) {}

    @Override
    public void visit(IRAffineT type) {
      type.getInner().accept(this);
    }

    @Override
    public void visit(IRCellT type) {
      putIfWritten(
          () -> {
            putAllocCell(access(newBuffer, "struct cell*"));
            putAssign(
                cellRefCount(access(newBuffer, "struct cell*")),
                cellRefCount(access(oldBuffer, "struct cell*")));
            putCloneRecord(
                type.getInner(),
                cellRecord(access(oldBuffer, "struct cell*")),
                cellRecord(access(newBuffer, "struct cell*")));
          });
    }
  }

  private class RecordBufferCleaner extends IRTypeVisitor {
    private String buffer;
    private String written;
    private String read;

    private RecordBufferCleaner(String buffer, String written, String read) {
      this.buffer = buffer;
      this.written = written;
      this.read = read;
    }

    private void putIfWritten(Runnable action) {
      putIf(written + " > 0", action);
    }

    private void putIfUnread(Runnable action) {
      putIf(read + " <= 0", action);
    }

    private void putIfWrittenAndUnread(Runnable action) {
      putIf(written + " > 0 && " + read + " <= 0", action);
    }

    private void recurse(String manager, String offset) {
      putCleanRecordBufferCall(
          manager, buffer + " + " + offset, written + " - " + offset, read + " - " + offset);
    }

    private void recurse(IRType type, String offset) {
      putCleanRecordBufferCall(
          type, buffer + " + " + offset, written + " - " + offset, read + " - " + offset);
    }

    @Override
    public void visit(IRType type) {
      throw new UnsupportedOperationException(
          "Unsupported type for buffer record cleaning: " + type.getClass().getName());
    }

    @Override
    public void visit(IRIntT type) {}

    @Override
    public void visit(IRBoolT type) {}

    @Override
    public void visit(IRStringT type) {
      putIfWrittenAndUnread(
          () -> {
            putStatement("string_drop(" + access(buffer, "char*") + ")");
          });
    }

    @Override
    public void visit(IRSessionT type) {
      putIfWritten(
          () -> {
            putIfUnread(
                () -> {
                  putCleanRecord(type.getArg(), access(buffer, "struct record*"));
                });
            recurse(type.getCont(), "sizeof(struct record*)");
          });
    }

    @Override
    public void visit(IRExponentialT type) {
      putIfWrittenAndUnread(
          () -> {
            switchTypeId(
                type.getInner(),
                () -> {},
                () -> {},
                () -> {
                  putCleanExponential(type.getInner(), access(buffer, "struct exponential*"));
                });
          });
    }

    @Override
    public void visit(IRTagT type) {
      putIfWritten(
          () -> {
            String tag = access(buffer, "unsigned char");

            putLine("switch (" + tag + ") {");
            incIndent();

            for (int i = 0; i < type.getChoices().size(); ++i) {
              putLine("case " + i + ":");
              incIndent();
              recurse(type.getChoices().get(i), "sizeof(unsigned char)");
              putStatement("break");
              decIndent();
            }

            decIndent();
            putLine("}");
          });
    }

    @Override
    public void visit(IRTypeT type) {
      putIfWrittenAndUnread(
          () -> {
            putManagerPushType(access(buffer + " + sizeof(struct record*)", "struct type"));
            putCleanRecord(type.getCont(), access(buffer, "struct record*"));
            putManagerPopType();
          });
    }

    @Override
    public void visit(IRRecT type) {
      putIfWritten(
          () -> {
            putManagerPushType(typeInitializer(type.getInner()));
            type.getInner().accept(this);
            putManagerPopType();
          });
    }

    @Override
    public void visit(IRVarT type) {
      // Recurse into the manager of the type variable.
      String manager = typeManager(managerType(type.getType()));
      putIf(
          manager + " != NULL",
          () -> {
            recurse(manager, "0");
          });
    }

    @Override
    public void visit(IRCloseT type) {}

    @Override
    public void visit(IRAffineT type) {
      type.getInner().accept(this);
    }

    @Override
    public void visit(IRCellT type) {
      putIfWritten(
          () -> {
            if (!disableConcurrency) {
              putStatement(
                  "pthread_mutex_destroy(&" + cellMutex(access(buffer, "struct cell*")) + ");");
            }
            putCleanRecord(type.getInner(), cellRecord(access(buffer, "struct cell*")));
            putFreeCell(access(buffer, "struct cell*"));
          });
    }
  }
}
