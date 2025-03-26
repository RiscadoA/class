package pt.inescid.cllsj.compiler;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import pt.inescid.cllsj.compiler.ir.*;
import pt.inescid.cllsj.compiler.ir.expressions.*;
import pt.inescid.cllsj.compiler.ir.instructions.*;
import pt.inescid.cllsj.compiler.ir.instructions.IRCallProcess.LinearArgument;
import pt.inescid.cllsj.compiler.ir.type.*;

public class CGenerator extends IRInstructionVisitor {
  private static final String TMP_TASK = "tmp_task";
  private static final String TMP_ENV = "tmp_env";
  private static final String TMP_CONT = "tmp_cont";
  private static final String TMP_RECORD = "tmp_record";
  private static final String TMP_EXPONENTIAL = "tmp_exponential";

  private static final String TASK = "task";
  private static final String ENV = "env";

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
    gen.putLine("void* cont;");
    gen.putLine("int record_count;");
    gen.putLine("int exponential_count;");
    gen.putLine("int end_points;");
    gen.putLine("int ref_count;");
    gen.decIndent();
    gen.putLine("};");
    gen.putBlankLine();

    // Define the environment struct.
    gen.putLine("struct environment {");
    gen.incIndent();
    gen.putLine("struct exponential* exp;");
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
    gen.put("#define EXPONENTIAL_EXPONENTIAL(exp, exp2) (*(struct exponential**)(");
    gen.put("(char*)(exp) + ");
    gen.put("sizeof(struct exponential) + ");
    gen.put("sizeof(struct exponential*) * exp2");
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
    gen.putBlankLine();

    // Functions used for operations on string expressions.
    gen.putLine("char* string_create(const char* str) {");
    gen.incIndent();
    gen.putLine("char* clone = malloc(strlen(str) + 1);");
    gen.putStatement("strcpy(clone, str)");
    gen.putStatement("return clone");
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
    gen.putStatement("return concat");
    gen.decIndent();
    gen.putLine("}");
    gen.putBlankLine();
    gen.putLine("void string_print(const char* fmt, char* str) {");
    gen.incIndent();
    gen.putStatement("printf(fmt, str)");
    gen.putStatement("free(str)");
    gen.decIndent();
    gen.putLine("}");
    gen.putBlankLine();
    gen.putLine("char* string_from_int(int value) {");
    gen.incIndent();
    gen.putStatement("char* str = malloc(12)");
    gen.putStatement("sprintf(str, \"%d\", value)");
    gen.putStatement("return str");
    gen.decIndent();
    gen.putLine("}");
    gen.putBlankLine();
    gen.putLine("int string_equal(char* str1, char* str2) {");
    gen.incIndent();
    gen.putStatement("int result = strcmp(str1, str2) == 0");
    gen.putStatement("free(str1)");
    gen.putStatement("free(str2)");
    gen.putStatement("return result");
    gen.decIndent();
    gen.putLine("}");
    gen.putBlankLine();

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
    }
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

    if (instruction.getLinearArguments().isEmpty()) {
      if (!entryCall) {
        putIf("--" + endPoints() + " == 0", () -> putFreeEnvironment(ENV));
      } else {
        entryCall = false;
      }
      putAllocEnvironment(ENV, process);
    } else {
      putAssign(TMP_ENV, ENV);
      putAllocEnvironment(ENV, process);

      // Bind the linear arguments to the new environment
      for (LinearArgument arg : instruction.getLinearArguments()) {
        putAssign(record(ENV, arg.getTargetRecord()), record(TMP_ENV, arg.getSourceRecord()));
      }

      putIf("--" + endPoints(TMP_ENV) + " == 0", () -> putFreeEnvironment(TMP_ENV));
    }

    putAssign(endPoints(), process.getEndPoints());
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
    putAssign(TMP_RECORD, record(i.getNegRecord()));
    putAssign(
        record(recordContEnv(i.getNegRecord()), recordContRecord(i.getNegRecord())),
        record(i.getPosRecord()));

    // Decrement the end points and free the environment if necessary.
    putIf("--" + endPoints() + " == 0", () -> putFreeEnvironment(ENV));

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
    putIfElse(
        "--" + endPoints() + " == 0",
        () -> {
          putAssign(TMP_ENV, recordContEnv(i.getRecord()));
          putFreeEnvironment(ENV);
          putAssign(ENV, TMP_ENV);
          putComputedGoto(TMP_CONT);
        },
        () -> {
          putAssign(ENV, recordContEnv(i.getRecord()));
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
          endPoints(), endPoints() + " - " + (totalEndPoints - entry.getValue().getEndPoints()));
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
    putAllocRecord(record(instruction.getRecord()), instruction.getType());
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
  }

  @Override
  public void visit(IRNextTask instruction) {
    putIf("--" + endPoints() + " == 0", () -> putFreeEnvironment(ENV));
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
    ExpressionEvaluation eval = evaluateExpression(instruction.getExpression());
    putIfElse(
        eval.getCode(),
        () -> {
          eval.freeUsedRecords();
          putAssign(endPoints(), endPoints() + " - " + instruction.getOtherwise().getEndPoints());
          putConstantGoto(blockLabel(instruction.getThen().getLabel()));
        },
        () -> {
          eval.freeUsedRecords();
          putAssign(endPoints(), endPoints() + " - " + instruction.getThen().getEndPoints());
          putConstantGoto(blockLabel(instruction.getOtherwise().getLabel()));
        });
  }

  @Override
  public void visit(IRPrint instruction) {
    generatePrint(instruction.getExpression(), instruction.hasNewLine());
  }

  @Override
  public void visit(IRPushExpression instruction) {
    ExpressionEvaluation eval = evaluateExpression(instruction.getExpression());
    putPush(instruction.getRecord(), type(instruction.getExpression().getType()), eval.getCode());
    eval.freeUsedRecords();
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
    putAllocExponential(TMP_EXPONENTIAL, process.getExponentialCount());
    putAssign(
        exponentialCont(TMP_EXPONENTIAL), labelAddress("proc_" + instruction.getProcessName()));
    putAssign(exponentialRecordCount(TMP_EXPONENTIAL), process.getRecordCount());
    putAssign(exponentialExponentialCount(TMP_EXPONENTIAL), process.getExponentialCount());
    putAssign(exponentialEndPoints(TMP_EXPONENTIAL), process.getEndPoints());
    putAssign(exponentialRefCount(TMP_EXPONENTIAL), 1);
    putPushExponential(instruction.getRecord(), TMP_EXPONENTIAL);
  }

  @Override
  public void visit(IRPopExponential instruction) {
    putAssign(
        exponential(instruction.getArgExponential()), popExponential(instruction.getRecord()));
  }

  @Override
  public void visit(IRCallExponential instruction) {
    putAllocRecord(record(instruction.getArgRecord()), instruction.getArgType());

    // Initialize the environment of the new record
    String env = recordContEnv(instruction.getArgRecord());
    putAllocEnvironment(
        env,
        exponentialRecordCount(instruction.getExponential()),
        exponentialExponentialCount(instruction.getExponential()));
    putAssign(record(env, 0), record(instruction.getArgRecord()));
    putLine(
        "for (int i = 0; i < "
            + exponentialExponentialCount(instruction.getExponential())
            + "; i++) {");
    incIndent();
    String exp = exponentialExponential(instruction.getExponential(), "i");
    putAssign(exponential(env, exponentialRecordCount(instruction.getExponential()), "i"), exp);
    putStatement(exponentialRefCount(exp) + " += 1");
    decIndent();
    putLine("}");
    putAssign(endPoints(env), exponentialEndPoints(instruction.getExponential()));

    // Initialize other fields
    putAssign(
        recordCont(instruction.getArgRecord()), exponentialCont(instruction.getExponential()));
    putAssign(recordContRecord(instruction.getArgRecord()), 0);
    putAssign(read(instruction.getArgRecord()), 0);
    putAssign(written(instruction.getArgRecord()), 0);
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

  private String endPoints(String env) {
    return env + "->end_points";
  }

  private String endPoints() {
    return endPoints(ENV);
  }

  private String read(int record) {
    return "READ(" + record + ")";
  }

  private String written(int record) {
    return "WRITTEN(" + record + ")";
  }

  private String buffer(int record) {
    return "BUFFER(" + record + ")";
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

  private String recordCont(String env, int record) {
    return record(env, record) + "->cont";
  }

  private String recordCont(int record) {
    return recordCont(ENV, record);
  }

  private String recordContEnv(String env, int record) {
    return record(env, record) + "->cont_env";
  }

  private String recordContEnv(int record) {
    return recordContEnv(ENV, record);
  }

  private String recordContRecord(String env, int record) {
    return record(env, record) + "->cont_record";
  }

  private String recordContRecord(int record) {
    return recordContRecord(ENV, record);
  }

  private String exponential(String env, String recordCount, String exponential) {
    return "EXPONENTIAL(" + env + ", " + recordCount + ", " + exponential + ")";
  }

  private String exponential(int exponential) {
    return exponential(ENV, Integer.toString(recordCount), Integer.toString(exponential));
  }

  private String exponentialCont(String exponential) {
    return exponential + "->cont";
  }

  private String exponentialCont(int exponential) {
    return exponentialCont(exponential(exponential));
  }

  private String exponentialRecordCount(String exponential) {
    return exponential + "->record_count";
  }

  private String exponentialRecordCount(int exponential) {
    return exponentialRecordCount(exponential(exponential));
  }

  private String exponentialExponentialCount(String exponential) {
    return exponential + "->exponential_count";
  }

  private String exponentialExponentialCount(int exponential) {
    return exponentialExponentialCount(exponential(exponential));
  }

  private String exponentialEndPoints(String exponential) {
    return exponential + "->end_points";
  }

  private String exponentialEndPoints(int exponential) {
    return exponentialEndPoints(exponential(exponential));
  }

  private String exponentialRefCount(String exponential) {
    return exponential + "->ref_count";
  }

  private String exponentialExponential(String exponential, String exponential2) {
    return "EXPONENTIAL_EXPONENTIAL(" + exponential + ", " + exponential2 + ")";
  }

  private String exponentialExponential(int exponential, String exponential2) {
    return exponentialExponential(exponential(exponential), exponential2);
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

  // ================================= Statement building helpers =================================

  private void putAllocEnvironment(String var, String recordCount, String exponentialCount) {
    putAssign(
        var,
        "malloc(sizeof(struct environment) + "
            + recordCount
            + " * sizeof(struct record*) + "
            + exponentialCount
            + " * sizeof(struct exponential*))");
    if (profile) {
      putStatement("++env_allocs");
    }
  }

  private void putAllocEnvironment(String var, int recordCount, int exponentialCount) {
    putAllocEnvironment(var, Integer.toString(recordCount), Integer.toString(exponentialCount));
  }

  private void putAllocEnvironment(String var, IRProcess process) {
    putAllocEnvironment(var, process.getRecordCount(), process.getExponentialCount());
  }

  private void putFreeEnvironment(String var) {
    if (trace) {
      putDebugLn("[endCall(" + procName + ")]");
    }
    putLine("free(" + var + ");");
    if (profile) {
      putStatement("++env_frees");
    }
  }

  private void putAllocRecord(String var, IRType type) {
    putAssign(var, "malloc(sizeof(struct record) + " + size(type) + ")");
    if (profile) {
      putStatement("++record_allocs");
    }
  }

  private void putFreeRecord(String var) {
    putLine("free(" + var + ");");
    if (profile) {
      putStatement("++record_frees");
    }
  }

  private void putAllocTask(String var) {
    putAssign(var, "malloc(sizeof(struct task))");
    if (profile) {
      putStatement("++task_allocs");
    }
  }

  private void putFreeTask(String var) {
    putLine("free(" + var + ");");
    if (profile) {
      putStatement("++task_frees");
    }
  }

  private void putAllocExponential(String var, int exponentialCount) {
    putAssign(
        var,
        "malloc(sizeof(struct exponential) + "
            + exponentialCount
            + " * sizeof(struct exponential*))");
    if (profile) {
      putStatement("++exponential_allocs");
    }
  }

  private void putFreeExponential(String var) {
    putLine("free(" + var + ");");
    if (profile) {
      putStatement("++exponential_frees");
    }
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

  private void putPrintLn(String message) {
    putPrint(message + "\\n");
  }

  private void putPrint(String message) {
    message = message.replace("\"", "\\\"");
    putStatement("printf(\"" + message + "\")");
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

  private class ExpressionEvaluation {
    private final String code;
    private final Set<Integer> usedRecords;

    public ExpressionEvaluation(String code, Set<Integer> usedRecords) {
      this.code = code;
      this.usedRecords = usedRecords;
    }

    public String getCode() {
      return code;
    }

    public Set<Integer> getUsedRecords() {
      return usedRecords;
    }

    public void freeUsedRecords() {
      for (int record : usedRecords) {
        putFreeRecord(record(record));
      }
    }
  }
  ;

  private ExpressionEvaluation evaluateExpression(IRExpression expr) {
    ExpressionGenerator gen = new ExpressionGenerator();
    expr.accept(gen);
    return new ExpressionEvaluation(gen.code, gen.usedRecords);
  }

  private ExpressionEvaluation evaluateExpressionToString(IRExpression expr) {
    if (expr.getType() instanceof IRIntT) {
      ExpressionEvaluation eval = evaluateExpression(expr);
      return new ExpressionEvaluation(
          "string_from_int(" + eval.getCode() + ")", eval.getUsedRecords());
    } else if (expr.getType() instanceof IRBoolT) {
      ExpressionEvaluation eval = evaluateExpression(expr);
      return new ExpressionEvaluation(
          "string_create(" + eval.getCode() + " ? \"true\" : \"false\")", eval.getUsedRecords());
    } else {
      return evaluateExpression(expr);
    }
  }

  private class ExpressionGenerator extends IRExpressionVisitor {
    private String code = "";
    private Set<Integer> usedRecords = new HashSet<>();

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
      usedRecords.add(expr.getRecord());
    }

    @Override
    public void visit(IRAdd expr) {
      if (expr.getType() instanceof IRStringT) {
        ExpressionEvaluation lhs = evaluateExpressionToString(expr.getLhs());
        ExpressionEvaluation rhs = evaluateExpressionToString(expr.getRhs());

        code += "string_concat(" + lhs.code + ", " + rhs.code + ")";

        usedRecords.addAll(lhs.getUsedRecords());
        usedRecords.addAll(rhs.getUsedRecords());
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
        ExpressionEvaluation lhs = evaluateExpressionToString(expr.getLhs());
        ExpressionEvaluation rhs = evaluateExpressionToString(expr.getRhs());

        code += "string_equal(" + lhs.code + ", " + rhs.code + ")";

        usedRecords.addAll(lhs.getUsedRecords());
        usedRecords.addAll(rhs.getUsedRecords());
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
      ExpressionEvaluation eval = evaluateExpression(expr);
      putStatement("printf(\"%d" + nl() + "\", " + eval.getCode() + ")");
      eval.freeUsedRecords();
    }

    @Override
    public void visit(IRBoolT type) {
      ExpressionEvaluation eval = evaluateExpression(expr);
      putStatement("printf(\"%s" + nl() + "\", " + eval.getCode() + " ? \"true\" : \"false\")");
      eval.freeUsedRecords();
    }

    @Override
    public void visit(IRStringT type) {
      ExpressionEvaluation eval = evaluateExpression(expr);
      putStatement("string_print(\"%s" + nl() + "\", " + eval.getCode() + ")");
      eval.freeUsedRecords();
    }
  }
}
