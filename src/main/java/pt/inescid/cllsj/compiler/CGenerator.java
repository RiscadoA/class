package pt.inescid.cllsj.compiler;

import java.util.ArrayList;
import java.util.Map;
import java.util.Optional;
import pt.inescid.cllsj.compiler.ir.*;
import pt.inescid.cllsj.compiler.ir.expressions.*;
import pt.inescid.cllsj.compiler.ir.instructions.*;
import pt.inescid.cllsj.compiler.ir.instructions.IRCallProcess.ExponentialArgument;
import pt.inescid.cllsj.compiler.ir.instructions.IRCallProcess.LinearArgument;
import pt.inescid.cllsj.compiler.ir.instructions.IRPushExponential.InheritedExponential;
import pt.inescid.cllsj.compiler.ir.type.*;

public class CGenerator extends IRInstructionVisitor {
  private static final String TMP_TASK = "tmp_task";
  private static final String TMP_ENV = "tmp_env";
  private static final String TMP_CONT = "tmp_cont";
  private static final String TMP_RECORD = "tmp_record";
  private static final String TMP_EXPONENTIAL = "tmp_exponential";

  private static final String TASK = "task";
  private static final String ENV = "env";
  private static final String MANAGER_STATE = "manager_state";

  private IRProgram ir;
  private String code = "";
  private int indentLevel = 0;
  private int genLabelCountInBlock;
  private String procName;
  private int recordCount;
  private Optional<String> blockName;
  private boolean trace;
  private boolean entryCall = true;
  private boolean profile;

  public static String generate(IRProgram ir, String entryProcess, boolean trace, boolean profile) {
    final CGenerator gen = new CGenerator(ir, trace, profile);

    // Add the necessary includes.
    gen.putLine("#include <stdlib.h>");
    gen.putLine("#include <stdio.h>");
    gen.putLine("#include <string.h>");
    gen.putBlankLine();

    // Initialize the profiling variables.
    if (profile) {
      gen.putStatement("unsigned long env_allocs = 0");
      gen.putStatement("unsigned long env_frees = 0");
      gen.putStatement("unsigned long record_allocs = 0");
      gen.putStatement("unsigned long record_frees = 0");
      gen.putStatement("unsigned long exponential_allocs = 0");
      gen.putStatement("unsigned long exponential_frees = 0");
      gen.putStatement("unsigned long task_allocs = 0");
      gen.putStatement("unsigned long task_frees = 0");
      gen.putStatement("unsigned long string_allocs = 0");
      gen.putStatement("unsigned long string_frees = 0");
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

    // Define the exponential struct.
    gen.putLine("struct exponential {");
    gen.incIndent();
    gen.putLine("int ref_count;");
    gen.putLine("struct record* record;");
    gen.decIndent();
    gen.putLine("};");
    gen.putBlankLine();

    // Define the manager stack struct.
    gen.putLine("struct manager_state {");
    gen.incIndent();
    gen.putLine("struct environment** env;");
    gen.putLine("struct record** record;");
    gen.putLine("int env_capacity;");
    gen.putLine("int record_capacity;");
    gen.putLine("int env_count;");
    gen.putLine("int record_count;");
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
    gen.putLine("int end_points;");
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
    gen.putBlankLine();

    // Utility macro for accessing the read, written and buffer fields of a record in the active
    // environment.
    gen.putLine("#define READ(rec) RECORD(" + ENV + ", rec)->read");
    gen.putLine("#define WRITTEN(rec) RECORD(" + ENV + ", rec)->written");
    gen.putLine("#define BUFFER(rec) RECORD(" + ENV + ", rec)->buffer");
    gen.putBlankLine();

    // Utility macros for pushing and popping values to/from the buffer of a record in the active
    // environment.
    gen.put("#define PUSH(rec, type, value) (*(type*)(");
    gen.put("&BUFFER(rec)[(WRITTEN(rec) += sizeof(type)) - sizeof(type)]");
    gen.put(")) = value");
    gen.putLineEnd();
    gen.put("#define POP(rec, type) (*(type*)(");
    gen.put("&BUFFER(rec)[(READ(rec) += sizeof(type)) - sizeof(type)]");
    gen.put("))");
    gen.putLineEnd();
    gen.put("#define ACCESS(ptr, type, offset) (*(type*)(ptr->buffer + offset))");
    gen.putLineEnd();
    gen.putBlankLine();

    // Utility macros for pushing stuff to manager stacks.\
    gen.put("#define MANAGER_PUSH(what, value) do { ");
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
            + "->what ## _capacity * sizeof(struct what*)); ");
    gen.put("} ");
    gen.put(MANAGER_STATE + "->what[" + MANAGER_STATE + "->what ## _count++] = value; ");
    gen.put("} while (0)");
    gen.putLineEnd();
    gen.put("#define MANAGER_PUSH_PAIR(what, first, second) do { ");
    gen.put("MANAGER_PUSH(what, first); ");
    gen.put("MANAGER_PUSH(what, second); ");
    gen.put("} while (0)");
    gen.putLineEnd();
    gen.put("#define MANAGER_RESET() do { ");
    gen.put(MANAGER_STATE + "->env_count = 0; ");
    gen.put(MANAGER_STATE + "->record_count = 0; ");
    gen.put("} while (0)");
    gen.putBlankLine();

    // Utility macros for calling environment managers.
    gen.putLine("#define MANAGER_CALL_CLONE(env) (env)->manager(env, " + MANAGER_STATE + ", 1)");
    gen.putLine("#define MANAGER_CALL_CLEAN(env) (env)->manager(env, " + MANAGER_STATE + ", 0)");
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
    gen.putLine("string_drop(char* str) {");
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

    // Generate managers for all process' environments.
    // Each manager function receives the environment to be operated on and the manager's state.
    // If clone is false, then the manager will free the environment and all its records,
    // recursively. Otherwise, then the manager will clone the environment and all
    // its records, and return the new environment.
    for (Map.Entry<String, IRProcess> entry : ir.getProcesses().entrySet()) {
      String processName = entry.getKey();
      IRProcess process = entry.getValue();
      gen.putLine(
          "struct environment* manager_"
              + processName
              + "(struct environment* env, struct manager_state* "
              + MANAGER_STATE
              + ", int clone) {");
      gen.incIndent();
      gen.putIfElse(
          "clone",
          () -> {
            gen.putStatement("struct environment* new_env");

            // The first thing we do is check if the environment has already been allocated.
            // If so, we just return it.
            gen.putManagerFindEnvironmentPair("env", "new_env");
            gen.putIf("new_env != NULL", () -> gen.putStatement("return new_env"));

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
          });
      gen.decIndent();
      gen.putLine("}");
      gen.putBlankLine();
    }

    // Main function.
    gen.putLine("int main() {");
    gen.incIndent();

    // Define registers.
    gen.putStatement("struct task* " + TASK);
    gen.putStatement("struct task* " + TMP_TASK);
    gen.putStatement("struct environment* " + ENV);
    gen.putStatement("struct environment* " + TMP_ENV);
    gen.putStatement("void* " + TMP_CONT);
    gen.putStatement("struct record* " + TMP_RECORD);
    gen.putStatement("struct exponential* " + TMP_EXPONENTIAL);
    gen.putStatement("struct manager_state* " + MANAGER_STATE);
    gen.putBlankLine();

    // Initialize the cloner.
    gen.putAssign(MANAGER_STATE, "malloc(sizeof(struct manager_state))");
    gen.putAssign(MANAGER_STATE + "->env", "NULL");
    gen.putAssign(MANAGER_STATE + "->record", "NULL");
    gen.putAssign(MANAGER_STATE + "->env_capacity", 0);
    gen.putAssign(MANAGER_STATE + "->record_capacity", 0);
    gen.putAssign(MANAGER_STATE + "->env_count", 0);
    gen.putAssign(MANAGER_STATE + "->record_count", 0);
    gen.putBlankLine();

    // Initialize the task list.
    gen.putAllocTask(TASK);
    gen.putAssign(gen.taskCont(TASK), gen.labelAddress("end"));
    gen.putBlankLine();

    // Jump to the entry process.
    if (!ir.getProcesses().containsKey(entryProcess)) {
      throw new RuntimeException("Entry process not found: " + entryProcess);
    }
    if (ir.getProcesses().get(entryProcess).hasArguments()) {
      throw new RuntimeException("Entry process cannot have arguments: " + entryProcess);
    }
    gen.visitInstruction(new IRCallProcess(entryProcess, new ArrayList<>(), new ArrayList<>()));

    // Generate code for each process.
    for (Map.Entry<String, IRProcess> procEntry : ir.getProcesses().entrySet()) {
      gen.recordCount = procEntry.getValue().getRecordCount();

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
    if (profile) {
      gen.putDebugLn("Profiling results:");
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
    }
    gen.putStatement("free(" + MANAGER_STATE + ")");
    gen.putLine("return 0;");

    gen.decIndent();
    gen.putLine("}");

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
        && instruction.getExponentialArguments().isEmpty()) {
      if (!entryCall) {
        putIf(decrement(environmentEndPoints()) + " == 0", () -> putFreeEnvironment(ENV));
      } else {
        entryCall = false;
      }
      putAllocEnvironment(ENV, instruction.getProcessName());
    } else {
      putAssign(TMP_ENV, ENV);
      putAllocEnvironment(ENV, instruction.getProcessName());

      // Bind the arguments to the new environment
      for (LinearArgument arg : instruction.getLinearArguments()) {
        putAssign(record(ENV, arg.getTargetRecord()), record(TMP_ENV, arg.getSourceRecord()));
      }
      for (ExponentialArgument arg : instruction.getExponentialArguments()) {
        putAssign(
            exponential(ENV, process.getRecordCount(), arg.getTargetExponential()),
            exponential(TMP_ENV, recordCount, arg.getSourceExponential()));
      }

      putIf(decrement(environmentEndPoints(TMP_ENV)) + " == 0", () -> putFreeEnvironment(TMP_ENV));
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
        decrement(environmentEndPoints()) + " == 0",
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
  public void visit(IRForwardExponential instruction) {
    putPushExponential(instruction.getRecord(), exponential(instruction.getExponential()));
    new IRReturn(instruction.getRecord()).accept(this);
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

    putIfElse(
        decrement(environmentEndPoints()) + " == 0",
        () -> {
          putFreeEnvironment(ENV);
          putAssign(ENV, TMP_ENV);
          putComputedGoto(TMP_CONT);
        },
        () -> {
          putAssign(ENV, TMP_ENV);
          putComputedGoto(TMP_CONT);
        });
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
    putIf(decrement(environmentEndPoints()) + " == 0", () -> putFreeEnvironment(ENV));
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
  public void visit(IRPushExpression instruction) {
    putPush(
        instruction.getRecord(),
        type(instruction.getExpression().getType()),
        expression(instruction.getExpression()));
  }

  @Override
  public void visit(IRPushType instruction) {
    putPush(instruction.getRecord(), "unsigned char", instruction.isPositive() ? "1" : "0");
  }

  @Override
  public void visit(IRPopType instruction) {
    putIfElse(
        pop(instruction.getRecord(), "unsigned char"),
        () -> putConstantGoto(blockLabel(instruction.getPositiveLabel())),
        () -> putConstantGoto(blockLabel(instruction.getNegativeLabel())));
  }

  @Override
  public void visit(IRPushExponential instruction) {
    IRProcess process = ir.getProcesses().get(instruction.getProcessName());

    // Initialize the record we'll be promoting
    putAllocExponential(TMP_EXPONENTIAL);
    putAssign(exponentialRefCount(TMP_EXPONENTIAL), 1);
    putAllocRecord(exponentialRecord(TMP_EXPONENTIAL), process.getRecordType(0));
    putAssign(read(exponentialRecord(TMP_EXPONENTIAL)), 0);
    putAssign(written(exponentialRecord(TMP_EXPONENTIAL)), 0);

    // Send it to the channel
    putPushExponential(instruction.getRecord(), TMP_EXPONENTIAL);

    // Initialize the environment of the new record
    putAllocEnvironment(TMP_ENV, instruction.getProcessName());
    putAssign(environmentEndPoints(TMP_ENV), process.getEndPoints());
    putAssign(record(TMP_ENV, 0), exponentialRecord(TMP_EXPONENTIAL));
    for (InheritedExponential inherited : instruction.getInheritedExponentials()) {
      putAssign(
          exponential(TMP_ENV, process.getRecordCount(), inherited.getTargetExponential()),
          exponential(inherited.getSourceExponential()));
    }

    // Initialize the continuation of the linear record to be promoted
    if (instruction.shouldExecute()) {
      // If we should do a partial execution f the record before pushing it,
      // we need to set the continuation of it so that it returns here.
      String label = makeLabel("pushExponential");
      putAssign(recordCont(exponentialRecord(TMP_EXPONENTIAL)), labelAddress(label));
      putAssign(recordContEnv(exponentialRecord(TMP_EXPONENTIAL)), ENV);
      putAssign(ENV, TMP_ENV);
      putConstantGoto("proc_" + instruction.getProcessName());
      putLabel(label);
    } else {
      // If we don't need to do a partial execution, we set the continuation to call the process
      // directly.
      putAssign(
          recordCont(exponentialRecord(TMP_EXPONENTIAL)),
          labelAddress("proc_" + instruction.getProcessName()));
      putAssign(recordContEnv(exponentialRecord(TMP_EXPONENTIAL)), TMP_ENV);
    }
  }

  @Override
  public void visit(IRPopExponential instruction) {
    putAssign(
        exponential(instruction.getArgExponential()), popExponential(instruction.getRecord()));
  }

  @Override
  public void visit(IRCallExponential instruction) {
    Runnable cloneRecord = () -> {
      putCloneRecord(
        recordType(instruction.getArgRecord()),
        exponentialRecord(instruction.getExponential()),
        record(instruction.getArgRecord()));
      putManagerReset(); // Must reset the manager to ensure the next clone is fresh.
    };

    if (instruction.shouldDecreaseRefCount()) {
      putIfElse(exponentialRefCount(instruction.getExponential()) + " == 1",
          () -> {
            // If the reference count is 1, and we would decrement it, we don't clone the record.
            // Instead, we just use the record directly and deallocate the wrapping exponential.
            putAssign(record(instruction.getArgRecord()), exponentialRecord(instruction.getExponential()));
            putFreeExponential(exponential(instruction.getExponential()));
          },
          () -> {
            cloneRecord.run();
            new IRDecRefExponential(instruction.getExponential()).accept(this);
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
        exponential(instruction.getExponential()), exponentialType(instruction.getExponential()));
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

  private String exponentialRecordContEnv(String exponential) {
    return exponentialRecord(exponential) + "->cont_env";
  }

  private String pop(int index, String type) {
    return "POP(" + index + ", " + type + ")";
  }

  private String pop(int index, IRType type) {
    return pop(index, type(type));
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

  private String labelAddress(String label) {
    return "&&" + label;
  }

  private String blockLabel(String label) {
    return "block_" + procName + "_" + label;
  }

  private String decrement(String var) {
    return "--" + var;
  }

  // ================================= Statement building helpers =================================

  private void putAllocEnvironment(
      String var, String manager, String recordCount, String exponentialCount) {
    putAssign(
        var,
        "calloc(1, sizeof(struct environment) + "
            + recordCount
            + " * sizeof(struct record*) + "
            + exponentialCount
            + " * sizeof(struct exponential*))");
    putAssign(environmentManager(var), "&" + manager);
    if (profile) {
      putIncrement("env_allocs");
    }
  }

  private void putAllocEnvironment(
      String var, String manager, int recordCount, int exponentialCount) {
    putAllocEnvironment(
        var, manager, Integer.toString(recordCount), Integer.toString(exponentialCount));
  }

  private void putAllocEnvironment(String var, String processName) {
    IRProcess process = ir.getProcesses().get(processName);
    putAllocEnvironment(
        var, "manager_" + processName, process.getRecordCount(), process.getExponentialCount());
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

  private void putAllocRecord(String var, IRType type) {
    putAssign(var, "malloc(sizeof(struct record) + " + size(type) + ")");
    if (profile) {
      putIncrement("record_allocs");
    }
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

  private void putDecRefExponential(String var, IRType type) {
    putIf(
        decrement(exponentialRefCount(var)) + " == 0",
        () -> {
          putCleanRecord(type, exponentialRecord(var));
          putManagerReset(); // Must reset the manager to ensure the next clean is fresh.
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
          putAssign(newEnv, "MANAGER_CALL_CLONE(" + oldEnv + ")");
        });
  }

  private void putCleanEnvironment(String env) {
    // We call the environment manager to clean the environment.
    putIf(
        env + " != NULL",
        () -> {
          putStatement("MANAGER_CALL_CLEAN(" + env + ")");
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
                putCloneRecordBufferContents(recordType, oldRecord, newRecord);
              });
        });
  }

  private void putCloneExponential(String oldExponential, String newExponential) {
    putAssign(newExponential, oldExponential);
    putIncrement(exponentialRefCount(newExponential));
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
                recordType.accept(new BufferRecordCleaner(record));

                // Release the record's memory.
                putFreeRecord(record);
              });
        });
  }

  private void putCleanExponential(IRType recordType, String exponential) {
    putIf(
        exponential + " != NULL",
        () -> {
          putDecRefExponential(exponential, recordType);
        });
  }

  private void putLabel(String label) {
    put(label + ":");
    putLineEnd();
  }

  private void putPush(int record, String type, String value) {
    putStatement("PUSH(" + record + ", " + type + ", " + value + ")");
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

  private void putManagerContainsRecord(String record, String bool) {
    putStatement("MANAGER_CONTAINS(record, " + record + ", " + bool + ")");
  }

  private void putManagerContainsEnvironment(String env, String bool) {
    putStatement("MANAGER_CONTAINS(env, " + env + ", " + bool + ")");
  }

  private void putManagerReset() {
    putStatement("MANAGER_RESET()");
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
      type.getInner().accept(this);
    }

    @Override
    public void visit(IRVarT type) {
      size += "0";
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
      size += "sizeof(unsigned char) + ";
      type.getCont().accept(this);
    }

    @Override
    public void visit(IRExponentialT type) {
      size += "sizeof(struct exponential*)";
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

  private String type(IRType type) {
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

  // ========================== Visitor used to clone records in buffers ==========================

  private void putCloneRecordBufferContents(IRType type, String oldRecord, String newRecord) {
    putStatement(
        "memcpy(" + buffer(newRecord) + ", " + buffer(oldRecord) + ", " + written(oldRecord) + ")");
    BufferRecordCloner visitor = new BufferRecordCloner(oldRecord, newRecord);
    type.accept(visitor);
  }

  private class BufferRecordCloner extends IRTypeVisitor {
    private String oldRecord;
    private String newRecord;
    private String offset = "0";

    public BufferRecordCloner(String oldRecord, String newRecord) {
      this.oldRecord = oldRecord;
      this.newRecord = newRecord;
    }

    private String access(String record, String type) {
      return "ACCESS(" + record + ", " + type + ", " + offset + ")";
    }

    private void putIfWrittenAndUnread(Runnable action) {
      putIf(
          written(oldRecord) + " > " + offset + " && " + read(oldRecord) + " <= " + offset, action);
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
      putIfWrittenAndUnread(() -> {
        putAssign(
            access(newRecord, "char*"),
            "string_create(" + access(oldRecord, "char*") + ")");
      });
    }

    @Override
    public void visit(IRSessionT type) {
      putIfWrittenAndUnread(
          () -> {
            putCloneRecord(
                type.getArg(),
                access(oldRecord, "struct record*"),
                access(newRecord, "struct record*"));
            offset += " + sizeof(struct record*)";
            type.getCont().accept(this);
          });
    }

    @Override
    public void visit(IRExponentialT type) {
      putIfWrittenAndUnread(
          () -> {
            putCloneExponential(
                access(oldRecord, "struct exponential*"), access(newRecord, "struct exponential*"));
          });
    }

    @Override
    public void visit(IRTagT type) {
      putIfWrittenAndUnread(
          () -> {
            String tag = access(oldRecord, "unsigned char");
            String offsetStart = offset + " + sizeof(unsigned char)";

            putLine("switch (" + tag + ") {");
            incIndent();

            for (int i = 0; i < type.getChoices().size(); ++i) {
              putLine("case " + i + ":");
              incIndent();
              offset = offsetStart;
              type.getChoices().get(i).accept(this);
              putStatement("break");
              decIndent();
            }

            decIndent();
            putLine("}");
          });
    }

    @Override
    public void visit(IRTypeT type) {
      type.getCont().accept(this);
    }

    @Override
    public void visit(IRRecT type) {
      type.getInner().accept(this);
    }

    @Override
    public void visit(IRVarT type) {}

    @Override
    public void visit(IRCloseT type) {}
  }

  // ============================= Visitor used to free record buffers =============================

  private class BufferRecordCleaner extends IRTypeVisitor {
    private String record;
    private String offset = "0";

    private BufferRecordCleaner(String record) {
      this.record = record;
    }

    private String access(String type) {
      return "ACCESS(" + record + ", " + type + ", " + offset + ")";
    }

    private void putIfWrittenAndUnread(Runnable action) {
      putIf(written(record) + " > " + offset + " && " + read(record) + " <= " + offset, action);
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
            putStatement("string_drop(" + access("char*") + ")");
          });
    }

    @Override
    public void visit(IRSessionT type) {
      putIfWrittenAndUnread(
          () -> {
            putCleanRecord(type.getArg(), access("struct record*"));
            offset += " + sizeof(struct record*)";
            type.getCont().accept(this);
          });
    }

    @Override
    public void visit(IRExponentialT type) {
      putIfWrittenAndUnread(
          () -> {
            putCleanExponential(type.getInner(), access("struct exponential*"));
          });
    }

    @Override
    public void visit(IRTagT type) {
      putIfWrittenAndUnread(
          () -> {
            String tag = access("unsigned char");
            String offsetStart = offset + " + sizeof(unsigned char)";

            putLine("switch (" + tag + ") {");
            incIndent();

            for (int i = 0; i < type.getChoices().size(); ++i) {
              putLine("case " + i + ":");
              incIndent();
              offset = offsetStart;
              type.getChoices().get(i).accept(this);
              putStatement("break");
              decIndent();
            }

            decIndent();
            putLine("}");
          });
    }

    @Override
    public void visit(IRTypeT type) {
      type.getCont().accept(this);
    }

    @Override
    public void visit(IRRecT type) {
      type.getInner().accept(this);
    }

    @Override
    public void visit(IRVarT type) {}

    @Override
    public void visit(IRCloseT type) {}
  }
}
