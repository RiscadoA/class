package pt.inescid.cllsj.compiler;

import java.util.ArrayList;
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

  private IRProgram ir;
  private String code = "";
  private int indentLevel = 0;
  private int genLabelCountInBlock;
  private String procName;
  private int recordCount;
  private int exponentialCount;
  private Optional<String> blockName;
  private boolean trace;
  private boolean entryCall = true;
  private boolean profile;
  private boolean inManager = false;

  private Set<IRType> usedRecordManagers = new HashSet<>();

  public static String generate(IRProgram ir, String entryProcess, boolean trace, boolean profile) {
    final CGenerator gen = new CGenerator(ir, trace, profile);

    // Add the necessary includes.
    gen.putLine("#define _POSIX_C_SOURCE 199309L");
    gen.putLine("#include <stdio.h>");
    gen.putLine("#include <stdlib.h>");
    gen.putLine("#include <string.h>");
    gen.putLine("#include <threads.h>");
    gen.putLine("#include <stdatomic.h>");
    gen.putLine("#include <time.h>");
    gen.putBlankLine();

    // Initialize the profiling variables.
    gen.putStatement("cnd_t thread_stops_cond_var");
    gen.putStatement("mtx_t thread_stops_mutex");
    gen.putStatement("atomic_ulong thread_inits = 1");
    gen.putStatement("atomic_ulong thread_stops = 0");
    if (profile) {
      gen.putStatement("atomic_ulong env_allocs = 0");
      gen.putStatement("atomic_ulong env_frees = 0");
      gen.putStatement("atomic_ulong record_allocs = 0");
      gen.putStatement("atomic_ulong record_frees = 0");
      gen.putStatement("atomic_ulong exponential_allocs = 0");
      gen.putStatement("atomic_ulong exponential_frees = 0");
      gen.putStatement("atomic_ulong task_allocs = 0");
      gen.putStatement("atomic_ulong task_frees = 0");
      gen.putStatement("atomic_ulong string_allocs = 0");
      gen.putStatement("atomic_ulong string_frees = 0");
      gen.putStatement("atomic_ulong cell_allocs = 0");
      gen.putStatement("atomic_ulong cell_frees = 0");
      gen.putBlankLine();
    }

    // Define the record struct.
    gen.putLine("struct record {");
    gen.incIndent();
    gen.putLine("void* cont;");
    gen.putLine("struct environment* cont_env;");
    gen.putLine("int cont_record;");
    gen.putLine("unsigned char read;");
    gen.putLine("unsigned char written;");
    gen.putLine("char buffer[];");
    gen.decIndent();
    gen.putLine("};");
    gen.putBlankLine();

    // Define the manager stack struct.
    gen.putLine("struct manager_state {");
    gen.incIndent();
    gen.putLine("struct environment** env;");
    gen.putLine("struct record** record;");
    gen.putLine("struct type* type;");
    gen.putLine("int env_capacity;");
    gen.putLine("int record_capacity;");
    gen.putLine("int type_capacity;");
    gen.putLine("int env_count;");
    gen.putLine("int record_count;");
    gen.putLine("int type_count;");
    gen.decIndent();
    gen.putLine("};");
    gen.putBlankLine();

    // Define the exponential struct.
    gen.putLine("struct exponential {");
    gen.incIndent();
    gen.putLine("atomic_int ref_count;");
    gen.putLine("struct record* record;");
    gen.putLine(
        "void(*manager)(const char* old, char* new, int written, int read, struct manager_state* "
            + MANAGER_STATE
            + ");");
    gen.decIndent();
    gen.putLine("};");
    gen.putBlankLine();

    // Define the type struct.
    gen.putLine("struct type {");
    gen.incIndent();
    gen.putLine("int size;");
    gen.putLine(
        "void(*manager)(const char* old, char* new, int written, int read, struct manager_state* "
            + MANAGER_STATE
            + ");");
    gen.decIndent();
    gen.putLine("};");
    gen.putBlankLine();

    // Define the environment struct.
    gen.putLine("struct environment {");
    gen.incIndent();
    gen.putLine(
        "struct environment*(*manager)(struct environment* env, struct manager_state* "
            + MANAGER_STATE
            + ", int clone);");
    gen.putLine("atomic_int end_points;");
    gen.decIndent();
    gen.putLine("};");
    gen.putBlankLine();

    // Define the task struct.
    gen.putLine("struct task {");
    gen.incIndent();
    gen.putLine("struct task* next;");
    gen.putLine("void* cont;");
    gen.putLine("struct environment* cont_env;");
    gen.decIndent();
    gen.putLine("};");
    gen.putBlankLine();

    // Define the cell struct.
    gen.putLine("struct cell {");
    gen.incIndent();
    gen.putLine("mtx_t mutex;");
    gen.putLine("atomic_int ref_count;");
    gen.putLine("struct record* record;");
    gen.decIndent();
    gen.putLine("};");
    gen.putBlankLine();

    // Utility macros for accessing records and exponentials on a given environment, and on
    // exponentials.
    gen.put("#define RECORD(env, rec) (*(struct record**)(");
    gen.put("(char*)(env) + ");
    gen.put("sizeof(struct environment) + ");
    gen.put("sizeof(struct record*) * rec");
    gen.put("))");
    gen.putLineEnd();
    gen.put("#define EXPONENTIAL(env, rec_count, exp) (*(struct exponential**)(");
    gen.put("(char*)(env) + ");
    gen.put("sizeof(struct environment) + ");
    gen.put("sizeof(struct record*) * rec_count + ");
    gen.put("sizeof(struct exponential*) * exp");
    gen.put("))");
    gen.putLineEnd();
    gen.put("#define TYPE(env, rec_count, exp_count, type_i) (*(struct type*)(");
    gen.put("(char*)(env) + ");
    gen.put("sizeof(struct environment) + ");
    gen.put("sizeof(struct record*) * rec_count + ");
    gen.put("sizeof(struct exponential*) * exp_count + ");
    gen.put("sizeof(struct type) * type_i");
    gen.put("))");
    gen.putLineEnd();
    gen.putBlankLine();

    // Utility macro for accessing the read, written and buffer fields of a record in the active
    // environment.
    gen.putLine("#define READ(rec) RECORD(" + ENV + ", rec)->read");
    gen.putLine("#define WRITTEN(rec) RECORD(" + ENV + ", rec)->written");
    gen.putLine("#define BUFFER(rec) RECORD(" + ENV + ", rec)->buffer");
    gen.putBlankLine();

    // Utility macros for pushing and popping values to/from the buffer of a record in the active
    // environment.
    gen.put("#define PUSH_RAW(rec, type, ...) (*(type*)(");
    gen.put("&(rec->buffer)[(rec->written += sizeof(type)) - sizeof(type)]");
    gen.put(")) = __VA_ARGS__");
    gen.putLineEnd();
    gen.put("#define PUSH(rec, type, ...) (*(type*)(");
    gen.put("&BUFFER(rec)[(WRITTEN(rec) += sizeof(type)) - sizeof(type)]");
    gen.put(")) = __VA_ARGS__");
    gen.putLineEnd();
    gen.put("#define POP(rec, type) (*(type*)(");
    gen.put("&BUFFER(rec)[(READ(rec) += sizeof(type)) - sizeof(type)]");
    gen.put("))");
    gen.putLineEnd();
    gen.put("#define PEEK(rec, type) (*(type*)(");
    gen.put("&BUFFER(rec)[READ(rec)]");
    gen.put("))");
    gen.putBlankLine();

    // Utility macros for pushing stuff to manager stacks.
    gen.put("#define MANAGER_PUSH(what, ...) do { ");
    gen.put(
        "if ("
            + MANAGER_STATE
            + "->what ## _count == "
            + MANAGER_STATE
            + "->what ## _capacity) { ");
    gen.put(
        MANAGER_STATE + "->what ## _capacity = " + MANAGER_STATE + "->what ## _capacity * 2 + 1;");
    gen.put(
        MANAGER_STATE
            + "->what = realloc("
            + MANAGER_STATE
            + "->what, "
            + MANAGER_STATE
            + "->what ## _capacity * sizeof(__VA_ARGS__)); ");
    gen.put("} ");
    gen.put(MANAGER_STATE + "->what[" + MANAGER_STATE + "->what ## _count++] = __VA_ARGS__; ");
    gen.put("} while (0)");
    gen.putLineEnd();
    gen.put("#define MANAGER_PUSH_PAIR(what, first, second) do { ");
    gen.put("MANAGER_PUSH(what, first); ");
    gen.put("MANAGER_PUSH(what, second); ");
    gen.put("} while (0)");
    gen.putLineEnd();
    gen.put("#define MANAGER_POP(what) do { ");
    gen.put(MANAGER_STATE + "->what ## _count -= 1; ");
    gen.put("} while (0)");
    gen.putLineEnd();
    gen.put("#define MANAGER_RESET() do { ");
    gen.put(MANAGER_STATE + "->env_count = 0; ");
    gen.put(MANAGER_STATE + "->record_count = 0; ");
    gen.put(MANAGER_STATE + "->type_count = 0; ");
    gen.put("} while (0)");
    gen.putBlankLine();

    // Utility macros for calling environment managers.
    gen.putLine(
        "#define ENV_MANAGER_CALL_CLONE(what) (what)->manager(what, " + MANAGER_STATE + ", 1)");
    gen.putLine(
        "#define ENV_MANAGER_CALL_CLEAN(what) (what)->manager(what, " + MANAGER_STATE + ", 0)");
    gen.putBlankLine();

    // Utility macros for finding values in manager stacks.
    gen.put("#define MANAGER_FIND(what, value, index) do { ");
    gen.put("for (index = 0; index < " + MANAGER_STATE + "->what ## _count; ++index) { ");
    gen.put("if (" + MANAGER_STATE + "->what[index] == value) { ");
    gen.put("break; ");
    gen.put("} ");
    gen.put("} ");
    gen.put("} while (0)");
    gen.putLineEnd();
    gen.put("#define MANAGER_FIND_PAIR(what, first, second) do { ");
    gen.put("int i; ");
    gen.put("MANAGER_FIND(what, first, i); ");
    gen.put("if (i < " + MANAGER_STATE + "->what ## _count - 1) { ");
    gen.put("second = " + MANAGER_STATE + "->what[i + 1]; ");
    gen.put("} else { ");
    gen.put("second = NULL; ");
    gen.put("} ");
    gen.put("} while (0)");
    gen.putLineEnd();
    gen.put("#define MANAGER_CONTAINS(what, value, boolean) do { ");
    gen.put("int i; ");
    gen.put("MANAGER_FIND(what, value, i); ");
    gen.put("boolean = i < " + MANAGER_STATE + "->what ## _count; ");
    gen.put("} while (0)");
    gen.putLineEnd();
    gen.putBlankLine();

    // Functions used for operations on string expressions.
    gen.putLine("char* string_create(const char* str) {");
    gen.incIndent();
    gen.putLine("char* clone = malloc(strlen(str) + 1);");
    gen.putStatement("strcpy(clone, str)");
    if (gen.profile) {
      gen.putStatement("string_allocs += 1");
    }
    gen.putStatement("return clone");
    gen.decIndent();
    gen.putLine("}");
    gen.putBlankLine();
    gen.putLine("void string_drop(char* str) {");
    gen.incIndent();
    gen.putStatement("free(str)");
    if (gen.profile) {
      gen.putStatement("string_frees += 1");
    }
    gen.decIndent();
    gen.putLine("}");
    gen.putBlankLine();
    gen.putLine("char* string_concat(char* str1, char* str2) {");
    gen.incIndent();
    gen.putStatement("char* concat = malloc(strlen(str1) + strlen(str2) + 1)");
    gen.putStatement("strcpy(concat, str1)");
    gen.putStatement("strcat(concat, str2)");
    gen.putStatement("free(str1)");
    gen.putStatement("free(str2)");
    if (gen.profile) {
      gen.putStatement("string_allocs += 1");
      gen.putStatement("string_frees += 2");
    }
    gen.putStatement("return concat");
    gen.decIndent();
    gen.putLine("}");
    gen.putBlankLine();
    gen.putLine("void string_print(const char* fmt, char* str) {");
    gen.incIndent();
    gen.putStatement("printf(fmt, str)");
    gen.putStatement("free(str)");
    if (gen.profile) {
      gen.putStatement("string_frees += 1");
    }
    gen.decIndent();
    gen.putLine("}");
    gen.putBlankLine();
    gen.putLine("char* string_from_int(int value) {");
    gen.incIndent();
    gen.putStatement("char* str = malloc(12)");
    gen.putStatement("sprintf(str, \"%d\", value)");
    if (gen.profile) {
      gen.putStatement("string_allocs += 1");
    }
    gen.putStatement("return str");
    gen.decIndent();
    gen.putLine("}");
    gen.putBlankLine();
    gen.putLine("int string_equal(char* str1, char* str2) {");
    gen.incIndent();
    gen.putStatement("int result = strcmp(str1, str2) == 0");
    gen.putStatement("free(str1)");
    gen.putStatement("free(str2)");
    if (gen.profile) {
      gen.putStatement("string_frees += 2");
    }
    gen.putStatement("return result");
    gen.decIndent();
    gen.putLine("}");
    gen.putBlankLine();

    // Functions used for reading primitives from the standard input.
    gen.putLine("int int_scan() {");
    gen.incIndent();
    gen.putLine("int value;");
    gen.putIf(
        "scanf(\"%d\", &value) == 1",
        () -> {
          gen.putStatement("return value");
        });
    gen.putStatement("return 0");
    gen.decIndent();
    gen.putLine("}");
    gen.putBlankLine();

    gen.putLine("int bool_scan() {");
    gen.incIndent();
    gen.putLine("char buffer[6];");
    gen.putIf(
        "scanf(\"%5s\", buffer) == 1",
        () -> {
          gen.putStatement("return strcmp(buffer, \"true\") == 0");
        });
    gen.putStatement("return 0");
    gen.decIndent();
    gen.putLine("}");
    gen.putBlankLine();

    gen.putLine("char* string_scan() {");
    gen.incIndent();
    gen.putLine("char buffer[256];");
    gen.putLine("char c;");
    gen.putLine("int i = 0;");
    gen.putWhile(
        "(c = getchar()) != \'\\n\' && c != EOF",
        () -> {
          gen.putIf(
              "i < sizeof(buffer) - 1",
              () -> {
                gen.putStatement("buffer[i++] = c");
              });
        });
    gen.putStatement("buffer[i] = '\\0'");
    gen.putStatement("return string_create(buffer)");
    gen.decIndent();
    gen.putLine("}");
    gen.putBlankLine();

    // Utility function for sleeping a given number of milliseconds.
    gen.putLine("void sleep_msecs(int msecs) {");
    gen.incIndent();
    gen.putStatement("struct timespec ts");
    gen.putStatement("ts.tv_sec = msecs / 1000");
    gen.putStatement("ts.tv_nsec = (msecs % 1000) * 1000000");
    gen.putStatement("nanosleep(&ts, NULL)");
    gen.decIndent();
    gen.putLine("}");

    // Generate environment managers and the main function to a different string, so that we can
    // insert record cloner and cleaner functions later.
    String headerCode = gen.code;
    gen.code = "";

    // Generate managers for all process' environments.
    // Each manager function receives the environment to be operated on and the manager's state.
    // If clone is false, then the manager will free the environment and all its records,
    // recursively. Otherwise, then the manager will clone the environment and all
    // its records, and return the new environment.
    gen.inManager = true;
    for (Map.Entry<String, IRProcess> entry : ir.getProcesses().entrySet()) {
      String processName = entry.getKey();
      IRProcess process = entry.getValue();
      gen.putLine(
          "struct environment* env_manager_"
              + processName
              + "(struct environment* env, struct manager_state* "
              + MANAGER_STATE
              + ", int clone) {");
      gen.incIndent();

      // Start by pushing a new type variable frame to the manager.
      gen.putManagerPushProcessTypes(process, "env");

      gen.putBlankLine();
      gen.putIfElse(
          "clone",
          () -> {
            gen.putStatement("struct environment* new_env");

            // The first thing we do is check if the environment has already been allocated.
            // If so, we just return it.
            gen.putManagerFindEnvironmentPair("env", "new_env");
            gen.putIf(
                "new_env != NULL",
                () -> {
                  gen.putManagerPopProcessTypes(process);
                  gen.putStatement("return new_env");
                });

            // Otherwise, we allocate it and put it in the manager.
            gen.putAllocEnvironment("new_env", processName);
            gen.putManagerPushEnvironmentPair("env", "new_env");
            gen.putAssign(gen.environmentEndPoints("new_env"), gen.environmentEndPoints("env"));
            for (int i = 0; i < process.getRecordCount(); ++i) {
              gen.putCloneRecord(
                  process.getRecordType(i), gen.record("env", i), gen.record("new_env", i));
            }
            for (int i = 0; i < process.getExponentialCount(); ++i) {
              gen.putCloneExponential(
                  gen.exponential("env", process.getRecordCount(), i),
                  gen.exponential("new_env", process.getRecordCount(), i));
            }
            for (int i = 0; i < process.getTypeVariableCount(); ++i) {
              gen.putAssign(
                  gen.type("new_env", process.getRecordCount(), process.getExponentialCount(), i),
                  gen.type("env", process.getRecordCount(), process.getExponentialCount(), i));
            }
            gen.putManagerPopProcessTypes(process);
            gen.putStatement("return new_env");
          },
          () -> {
            // Check if the environment has already been cleaned.
            // If so, return NULL.
            gen.putLine("int found;");
            gen.putManagerContainsEnvironment("env", "found");
            gen.putIf(
                "found",
                () -> {
                  gen.putManagerPopProcessTypes(process);
                  gen.putStatement("return NULL");
                });

            // Mark it as cleaned.
            gen.putManagerPushEnvironment("env");

            // Clean all records and exponentials in the environment.
            for (int i = 0; i < process.getRecordCount(); ++i) {
              gen.putCleanRecord(process.getRecordType(i), gen.record("env", i));
            }
            for (int i = 0; i < process.getExponentialCount(); ++i) {
              gen.putCleanExponential(
                  process.getExponentialType(i),
                  gen.exponential("env", process.getRecordCount(), i));
            }

            // Free the environment's memory.
            gen.putFreeEnvironment("env");
            gen.putManagerPopProcessTypes(process);
          });
      gen.decIndent();
      gen.putLine("}");
      gen.putBlankLine();
    }
    gen.inManager = false;

    // Execution function.
    gen.putLine("int thread(void* entry);");
    gen.putLine("void executor(struct task* entry) {");
    gen.incIndent();

    // Define registers.
    gen.putStatement("struct task* " + TASK);
    gen.putStatement("struct task* " + TMP_TASK);
    gen.putStatement("struct environment* " + ENV);
    gen.putStatement("struct environment* " + TMP_ENV);
    gen.putStatement("void* " + TMP_CONT);
    gen.putStatement("struct record* " + TMP_RECORD);
    gen.putStatement("struct exponential* " + TMP_EXPONENTIAL);
    gen.putStatement("thrd_t " + TMP_THREAD);
    gen.putStatement("struct cell* " + TMP_CELL);
    gen.putStatement("struct manager_state* " + MANAGER_STATE);
    gen.putBlankLine();

    // Initialize the manager.
    gen.putAssign(MANAGER_STATE, "calloc(1, sizeof(struct manager_state))");
    gen.putBlankLine();

    // Initialize the task list.
    gen.putAllocTask(TASK);
    gen.putAssign(gen.taskCont(TASK), gen.labelAddress("end"));
    gen.putBlankLine();

    // Jump to the entry process.
    gen.putIfElse(
        "entry == NULL",
        () -> {
          if (!ir.getProcesses().containsKey(entryProcess)) {
            throw new RuntimeException("Entry process not found: " + entryProcess);
          }
          if (ir.getProcesses().get(entryProcess).hasArguments()) {
            throw new RuntimeException("Entry process cannot have arguments: " + entryProcess);
          }
          gen.visitInstruction(
              new IRCallProcess(
                  entryProcess, new ArrayList<>(), new ArrayList<>(), new ArrayList<>()));
        },
        () -> {
          gen.putAssign(ENV, gen.taskContEnv("entry"));
          gen.putAssign(TMP_CONT, gen.taskCont("entry"));
          gen.putFreeTask("entry");
          gen.putComputedGoto(TMP_CONT);
        });

    // Generate code for each process.
    for (Map.Entry<String, IRProcess> procEntry : ir.getProcesses().entrySet()) {
      gen.recordCount = procEntry.getValue().getRecordCount();
      gen.exponentialCount = procEntry.getValue().getExponentialCount();

      String label = "proc_" + procEntry.getKey();
      gen.putBlankLine();
      gen.putLabel(label);
      gen.visitBlock(procEntry.getKey(), procEntry.getValue().getEntry());

      for (IRBlock block : procEntry.getValue().getBlocks()) {
        label = "block_" + procEntry.getKey() + "_" + block.getLabel();
        gen.putLabel(label);
        gen.visitBlock(procEntry.getKey(), block);
      }
    }

    gen.putBlankLine();
    gen.putLabel("end");
    gen.putStatement("mtx_lock(&thread_stops_mutex)");
    gen.putIncrement("thread_stops");
    gen.putStatement("cnd_signal(&thread_stops_cond_var)");
    gen.putStatement("mtx_unlock(&thread_stops_mutex)");
    gen.putStatement("free(" + MANAGER_STATE + ")");

    gen.decIndent();
    gen.putLine("}");
    gen.putBlankLine();
    gen.putLine("int thread(void* entry) {");
    gen.incIndent();
    gen.putStatement("executor((struct task*)entry)");
    gen.putLine("return 0;");
    gen.decIndent();
    gen.putLine("}");
    gen.putBlankLine();

    gen.putLine("int main() {");
    gen.incIndent();
    gen.putStatement("cnd_init(&thread_stops_cond_var)");
    gen.putStatement("mtx_init(&thread_stops_mutex, mtx_plain)");
    gen.putBlankLine();
    gen.putStatement("thread(NULL)");
    gen.putBlankLine();
    gen.putStatement("mtx_lock(&thread_stops_mutex)");
    gen.putWhile(
        "thread_stops != thread_inits",
        () -> {
          gen.putStatement("cnd_wait(&thread_stops_cond_var, &thread_stops_mutex)");
        });
    gen.putStatement("mtx_unlock(&thread_stops_mutex)");
    gen.putStatement("mtx_destroy(&thread_stops_mutex)");
    gen.putStatement("cnd_destroy(&thread_stops_cond_var)");
    gen.putBlankLine();
    if (profile) {
      gen.putDebugLn("Profiling results:");
      gen.putDebugLn("  Thread inits: %ld", "thread_inits");
      gen.putDebugLn("  Thread stops: %ld", "thread_stops");
      gen.putDebugLn("  Environment allocations: %ld", "env_allocs");
      gen.putDebugLn("  Environment frees: %ld", "env_frees");
      gen.putDebugLn("  Record allocations: %ld", "record_allocs");
      gen.putDebugLn("  Record frees: %ld", "record_frees");
      gen.putDebugLn("  Exponential allocations: %ld", "exponential_allocs");
      gen.putDebugLn("  Exponential frees: %ld", "exponential_frees");
      gen.putDebugLn("  Task allocations: %ld", "task_allocs");
      gen.putDebugLn("  Task frees: %ld", "task_frees");
      gen.putDebugLn("  String allocations: %ld", "string_allocs");
      gen.putDebugLn("  String frees: %ld", "string_frees");
      gen.putDebugLn("  Cell allocations: %ld", "cell_allocs");
      gen.putDebugLn("  Cell frees: %ld", "cell_frees");
      gen.putIf(
          "env_allocs != env_frees",
          () -> {
            gen.putDebugLn("Environment leak detected!");
            gen.putLine("return 1;");
          });
      gen.putIf(
          "record_allocs != record_frees",
          () -> {
            gen.putDebugLn("Record leak detected!");
            gen.putLine("return 1;");
          });
      gen.putIf(
          "exponential_allocs != exponential_frees",
          () -> {
            gen.putDebugLn("Exponential leak detected!");
            gen.putLine("return 1;");
          });
      gen.putIf(
          "task_allocs != task_frees",
          () -> {
            gen.putDebugLn("Task leak detected!");
            gen.putLine("return 1;");
          });
      gen.putIf(
          "string_allocs != string_frees",
          () -> {
            gen.putDebugLn("String leak detected!");
            gen.putLine("return 1;");
          });
      gen.putIf(
          "cell_allocs != cell_frees",
          () -> {
            gen.putDebugLn("Cell leak detected!");
            gen.putLine("return 1;");
          });
    }

    gen.putLine("return 0;");
    gen.decIndent();
    gen.putLine("}");

    // Generate the record buffer managers.
    // Since these might depend on other managers, we must forward declare them.
    String mainCode = gen.code;
    String declarationsCode = "";
    String definitionsCode = "";
    Set<String> generatedRecordManagers = new HashSet<>();
    boolean generatedNewManagers = true;
    while (generatedNewManagers) {
      generatedNewManagers = false;

      Set<IRType> usedRecordManagers = gen.usedRecordManagers;
      gen.usedRecordManagers = new HashSet<>();

      for (IRType type : usedRecordManagers) {
        if (!generatedRecordManagers.add(gen.recordBufferManagerName(type))) {
          continue;
        }

        gen.code = declarationsCode;
        gen.putRecordBufferManagerDeclaration(type);
        declarationsCode = gen.code;

        gen.code = definitionsCode;
        gen.putRecordBufferManagerDefinition(type);
        gen.putBlankLine();
        definitionsCode = gen.code;

        generatedNewManagers = true;
      }
    }

    // Concatenate the sections.
    gen.code = headerCode;
    gen.code += declarationsCode;
    gen.putBlankLine();
    gen.code += definitionsCode;
    gen.code += mainCode;
    return gen.code;
  }

  private CGenerator(IRProgram ir, boolean trace, boolean profile) {
    this.ir = ir;
    this.trace = trace;
    this.profile = profile;
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

    if (instruction.getLinearArguments().isEmpty()
        && instruction.getExponentialArguments().isEmpty()
        && instruction.getTypeArguments().isEmpty()) {
      if (!entryCall) {
        putIf(decrementAtomic(environmentEndPoints()) + " == 0", () -> putFreeEnvironment(ENV));
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

      putIf(decrementAtomic(environmentEndPoints(ENV)) + " == 0", () -> putFreeEnvironment(ENV));
      putAssign(ENV, TMP_ENV);
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
    putAllocTask(TMP_TASK);
    putAssign(taskCont(TMP_TASK), labelAddress(blockLabel(instruction.getLabel())));
    putAssign(taskContEnv(TMP_TASK), ENV);
    putIncrement("thread_inits");
    putStatement("thrd_create(&" + TMP_THREAD + ", thread, (void*)" + TMP_TASK + ")");
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
    String type = cType(instruction.getType());
    if (instruction.getType() instanceof IRIntT) {
      putPush(instruction.getRecord(), type, "int_scan()");
    } else if (instruction.getType() instanceof IRBoolT) {
      putPush(instruction.getRecord(), type, "bool_scan()");
    } else if (instruction.getType() instanceof IRStringT) {
      putPush(instruction.getRecord(), type, "string_scan()");
    } else {
      throw new UnsupportedOperationException(
          "Unsupported type for IRScan: " + instruction.getType().getClass().getName());
    }
  }

  @Override
  public void visit(IRPushExpression instruction) {
    if (instruction.isExponential()) {
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
    // Initialize a new exponential structure.
    putAllocExponential(exponential(instruction.getExponential()));
    putAssign(exponentialRefCount(instruction.getExponential()), 1);
    putAssign(exponentialRecord(instruction.getExponential()), record(instruction.getRecord()));
    String recordBufferManagerName = recordBufferManagerName(recordType(instruction.getRecord()));
    putAssign(
        exponentialManager(instruction.getExponential()),
        recordBufferManagerName.isEmpty() ? "NULL" : ("&" + recordBufferManagerName));
    putAssign(record(instruction.getRecord()), "NULL");
  }

  @Override
  public void visit(IRPushExponential instruction) {
    putPushExponential(instruction.getRecord(), exponential(instruction.getExponential()));
  }

  @Override
  public void visit(IRPopExponential instruction) {
    putAssign(
        exponential(instruction.getArgExponential()), popExponential(instruction.getRecord()));
  }

  @Override
  public void visit(IRCallExponential instruction) {
    Runnable cloneRecord =
        () -> {
          putManagerReset();
          putManagerPushProcessTypes();
          putIfElse(
              recordContEnv(exponentialRecord(instruction.getExponential())) + " != NULL",
              () -> {
                // The exponential record has a continuation environment, which means it hasn't been
                // closed yet. In that case, just clone the whole environment, and fetch the new
                // record from the new environment.
                //
                // The record's index in the environment is always 0.
                putCloneEnvironment(
                    recordContEnv(exponentialRecord(instruction.getExponential())), TMP_ENV);
                putAssign(record(instruction.getArgRecord()), record(TMP_ENV, 0));
              },
              () -> {
                // Otherwise, the record has already been closed. In that case, we allocate a new
                // record with a buffer big enough to accomodate all written data, and then, clone
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
            // If the reference count is 1, and we would decrement it, we don't clone the record.
            // Instead, we just use the record directly and deallocate the wrapping exponential.
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
  }

  @Override
  public void visit(IRIncRefExponential instruction) {
    putIncrement(exponentialRefCount(instruction.getExponential()));
  }

  @Override
  public void visit(IRDecRefExponential instruction) {
    putDecRefExponential(
        exponential(instruction.getExponential()),
        exponentialType(instruction.getExponential()),
        true);
  }

  @Override
  public void visit(IRDetachExponential instruction) {
    putAssign(exponential(instruction.getExponential()), "NULL");
  }

  @Override
  public void visit(IRIncRefCell instruction) {
    putIncrement(cellRefCount(peekCell(instruction.getRecord())));
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
    putStatement("mtx_init(&" + cellMutex(TMP_CELL) + ", mtx_plain)");
    putAssign(cellRefCount(TMP_CELL), 1);
    putAssign(cellRecord(TMP_CELL), record(instruction.getArgRecord()));
    putPushCell(instruction.getRecord(), TMP_CELL);
    putAssign(record(instruction.getArgRecord()), "NULL");
  }

  @Override
  public void visit(IRTakeCell instruction) {
    // Lock the mutex and extract the record.
    putStatement("mtx_lock(&" + cellMutex(peekCell(instruction.getRecord())) + ")");
    putAssign(record(instruction.getArgRecord()), cellRecord(peekCell(instruction.getRecord())));
    putAssign(cellRecord(peekCell(instruction.getRecord())), "NULL");
  }

  @Override
  public void visit(IRPutCell instruction) {
    // Put the record into the cell and unlock the mutex.
    putAssign(cellRecord(peekCell(instruction.getRecord())), record(instruction.getArgRecord()));
    putAssign(record(instruction.getArgRecord()), "NULL");
    putStatement("mtx_unlock(&" + cellMutex(peekCell(instruction.getRecord())) + ")");
  }

  @Override
  public void visit(IRSleep instruction) {
    putStatement("sleep_msecs(" + instruction.getMsecs() + ")");
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
    return "(struct type){.size = " + size(type) + ", .manager = " + managerValue + "}";
  }

  private String typeManager(String type) {
    return type + ".manager";
  }

  private String typeSize(String type) {
    return type + ".size";
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

  private String decrementAtomic(String var) {
    return "atomic_fetch_sub(&" + var + ", 1) - 1";
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
      putIncrement("env_allocs");
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
      putIncrement("env_frees");
    }
  }

  private void putAllocRecord(String var, String bufferSize) {
    putAssign(var, "malloc(sizeof(struct record) + " + bufferSize + ")");
    if (profile) {
      putIncrement("record_allocs");
    }
  }

  private void putAllocRecord(String var, IRType type) {
    putAllocRecord(var, size(type));
  }

  private void putFreeRecord(String var) {
    putLine("free(" + var + ");");
    if (profile) {
      putIncrement("record_frees");
    }
  }

  private void putAllocTask(String var) {
    putAssign(var, "malloc(sizeof(struct task))");
    if (profile) {
      putIncrement("task_allocs");
    }
  }

  private void putFreeTask(String var) {
    putLine("free(" + var + ");");
    if (profile) {
      putIncrement("task_frees");
    }
  }

  private void putAllocExponential(String var) {
    putAssign(var, "malloc(sizeof(struct exponential))");
    if (profile) {
      putIncrement("exponential_allocs");
    }
  }

  private void putFreeExponential(String var) {
    if (trace) {
      putDebugLn("[freeExponential(" + var + ")]");
    }
    putLine("free(" + var + ");");
    if (profile) {
      putIncrement("exponential_frees");
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
          putIncrement(exponentialRefCount(newExponential));
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
    putStatement("mtx_init(&" + cellMutex(var) + ", mtx_plain)");
    if (profile) {
      putIncrement("cell_allocs");
    }
  }

  private void putFreeCell(String var) {
    if (trace) {
      putDebugLn("[freeCell(" + var + ")]");
    }
    putStatement("mtx_destroy(&" + cellMutex(var) + ")");
    putStatement("free(" + var + ")");
    if (profile) {
      putIncrement("cell_frees");
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

  private void putIncrement(String var) {
    putStatement("++" + var);
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
      size += "sizeof(struct exponential*)";
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
      // We simply access the exponential data buffer directly.
      final String record = exponentialRecord(expr.getExponential());
      final String dataPtr = record + "->buffer + " + record + "->read";
      final String dataRef = "*((" + cType(expr.getType()) + "*)(" + dataPtr + "))";

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

  private static class RecordBufferManagerNamer extends IRTypeVisitor {
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
      separate();
      name += "exponential";
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
            putCloneExponential(
                access(oldBuffer, "struct exponential*"), access(newBuffer, "struct exponential*"));
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
            putCleanExponential(type.getInner(), access(buffer, "struct exponential*"));
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
            putStatement("mtx_destroy(&" + cellMutex(access(buffer, "struct cell*")) + ");");
            putCleanRecord(type.getInner(), cellRecord(access(buffer, "struct cell*")));
            putFreeCell(access(buffer, "struct cell*"));
          });
    }
  }
}
