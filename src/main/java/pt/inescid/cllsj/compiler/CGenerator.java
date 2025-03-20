package pt.inescid.cllsj.compiler;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import pt.inescid.cllsj.compiler.ir.IRBlock;
import pt.inescid.cllsj.compiler.ir.IRProcess;
import pt.inescid.cllsj.compiler.ir.IRProgram;
import pt.inescid.cllsj.compiler.ir.IRVisitor;
import pt.inescid.cllsj.compiler.ir.instructions.IRBranchOnPolarity;
import pt.inescid.cllsj.compiler.ir.instructions.IRCall;
import pt.inescid.cllsj.compiler.ir.instructions.IRFlip;
import pt.inescid.cllsj.compiler.ir.instructions.IRForward;
import pt.inescid.cllsj.compiler.ir.instructions.IRFreeSession;
import pt.inescid.cllsj.compiler.ir.instructions.IRInstruction;
import pt.inescid.cllsj.compiler.ir.instructions.IRJump;
import pt.inescid.cllsj.compiler.ir.instructions.IRNewSession;
import pt.inescid.cllsj.compiler.ir.instructions.IRNewTask;
import pt.inescid.cllsj.compiler.ir.instructions.IRNextTask;
import pt.inescid.cllsj.compiler.ir.instructions.IRPopClose;
import pt.inescid.cllsj.compiler.ir.instructions.IRPopSession;
import pt.inescid.cllsj.compiler.ir.instructions.IRPopTag;
import pt.inescid.cllsj.compiler.ir.instructions.IRPushClose;
import pt.inescid.cllsj.compiler.ir.instructions.IRPushSession;
import pt.inescid.cllsj.compiler.ir.instructions.IRPushTag;
import pt.inescid.cllsj.compiler.ir.instructions.IRCall.LinearArgument;
import pt.inescid.cllsj.compiler.ir.instructions.IRCall.TypeArgument;

public class CGenerator extends IRVisitor {
  private static final String TMP_TASK = "tmp_task";
  private static final String TMP_ENV = "tmp_env";
  private static final String TMP_CONT = "tmp_cont";
  private static final String TMP_RECORD = "tmp_record";

  private static final String TASK = "task";
  private static final String ENV = "env";

  private IRProgram ir;
  private String code = "";
  private int indentLevel = 0;
  private int genLabelCountInBlock;
  private String procName;
  private int envSize;
  private Optional<String> blockName;
  private boolean trace;

  public static String generate(IRProgram ir, String entryProcess, boolean trace) {
    final CGenerator gen = new CGenerator(ir, trace);

    // Add the necessary includes.
    gen.putLine("#include <stdlib.h>");
    gen.putLine("#include <stdio.h>");
    gen.putLine("#include <string.h>");
    gen.putBlankLine();

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

    // Define the type struct.
    gen.putLine("struct type {");
    gen.incIndent();
    gen.putLine("char polarity;");
    gen.decIndent();
    gen.putLine("};");
    gen.putBlankLine();

    // Define the environment struct.
    gen.putLine("struct environment {");
    gen.incIndent();
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

    // Utility macro for accessing records on a given environment.
    gen.put("#define RECORD(env, rec) (*(struct record**)(");
    gen.put("(char*)(env) + ");
    gen.put("sizeof(struct environment) + ");
    gen.put("sizeof(struct record*) * rec");
    gen.put("))");
    gen.putLineEnd();
    gen.putBlankLine();

    // Utility macro for accessing types on a given environment.
    gen.put("#define TYPE(env, size, ty) (*(struct type*)(");
    gen.put("(char*)(env) + ");
    gen.put("sizeof(struct environment) + ");
    gen.put("sizeof(struct record*) * size +");
    gen.put("sizeof(struct type) * ty");
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
    gen.put("&BUFFER(rec)[WRITTEN(rec) += sizeof(type)]");
    gen.put(")) = value");
    gen.putLineEnd();
    gen.put("#define POP(rec, type) (*(type*)(");
    gen.put("&BUFFER(rec)[READ(rec) += sizeof(type)]");
    gen.put("))");
    gen.putLineEnd();
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
    (new IRCall(entryProcess, new ArrayList<>(), new ArrayList<>())).accept(gen);

    // Generate code for each process.
    for (Map.Entry<String, IRProcess> procEntry : ir.getProcesses().entrySet()) {
      gen.envSize = procEntry.getValue().getRecordCount();

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
    gen.putLine("return 0;");

    gen.decIndent();
    gen.putLine("}");

    return gen.code;
  }

  private CGenerator(IRProgram ir, boolean trace) {
    this.ir = ir;
    this.trace = trace;
  }

  // ================================= IR instruction visitors ==================================

  private void visitBlock(String procName, IRBlock block) {
    genLabelCountInBlock = 0;
    this.procName = procName;
    this.blockName = Optional.ofNullable(block.getLabel());

    for (IRInstruction instruction : block.getInstructions()) {
      if (trace) {
        putPrintLn(instruction.toString());
      } else {
        putLine("/* " + instruction.toString() + " */");
      }
      instruction.accept(this);
    }
  }

  @Override
  public void visit(IRInstruction instruction) {
    throw new UnsupportedOperationException(
        "Unsupported instruction type: " + instruction.getClass().getName());
  }

  @Override
  public void visit(IRCall instruction) {
    IRProcess process = ir.getProcesses().get(instruction.getProcessName());

    if (instruction.getLinearArguments().isEmpty()) {
      putAllocEnvironment(ENV, process);
    } else {
      putAssign(TMP_ENV, ENV);
      putAllocEnvironment(ENV, process);

      // Bind the linear arguments to the new environment
      for (LinearArgument arg : instruction.getLinearArguments()) {
        putAssign(record(ENV, arg.getTargetRecord()), record(TMP_ENV, arg.getSourceRecord()));
      }

      // Store the polarities of the type arguments in the new environment
      for (TypeArgument arg : instruction.getTypeArguments()) {
        if (arg.getSourceType().isPresent()) {
          String op = arg.isDual() ? "!" : "";
          putAssign(typePolarity(ENV, arg.getTargetType()), op + typePolarity(TMP_ENV, arg.getSourceType().get()));
        } else {
          String lit = arg.getSourcePolarity() ? "1" : "0";
          putAssign(typePolarity(ENV, arg.getTargetType()), lit);
        }
      }
    }

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

    for (Map.Entry<Integer, String> entry : instruction.getLabels().entrySet()) {
      putLine("case " + entry.getKey() + ":");
      incIndent();
      putConstantGoto(blockLabel(entry.getValue()));
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
    putAllocRecord(record(instruction.getRecord()), instruction.getSize());
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
  public void visit(IRBranchOnPolarity instruction) {
    putLine("if (" + typePolarity(ENV, instruction.getType()) + ") {");
    incIndent();
    putConstantGoto(blockLabel(instruction.getPosLabel()));
    decIndent();
    putLine("} else {");
    incIndent();
    putConstantGoto(blockLabel(instruction.getNegLabel()));
    decIndent();
    putLine("}");
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

  private String typePolarity(String env, int ty) {
    return type(env, ty) + ".polarity";
  }

  private String type(String env, int ty) {
    return "TYPE(" + env + ", " + envSize + ", " + ty + ")";
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

  private String pop(int index, String type) {
    return "POP(" + index + ", " + type + ")";
  }

  private String popTag(int index) {
    return pop(index, "unsigned char");
  }

  private String popRecord(int index) {
    return pop(index, "struct record*");
  }

  private String labelAddress(String label) {
    return "&&" + label;
  }

  private String blockLabel(String label) {
    return "block_" + procName + "_" + label;
  }

  // ================================ Statement building helpers ================================

  private void putAllocEnvironment(String var, IRProcess process) {
    putAssign(
        var,
        "malloc(sizeof(struct environment) + "
            + process.getRecordCount()
            + " * sizeof(struct record*) + "
            + process.getTypeCount()
            + " * sizeof(struct type))");
  }

  private void putFreeEnvironment(String var) {
    putLine("free(" + var + ");");
  }

  private void putAllocRecord(String var, int size) {
    putAssign(var, "malloc(sizeof(struct record) + " + size + ")");
  }

  private void putFreeRecord(String var) {
    putLine("free(" + var + ");");
  }

  private void putAllocTask(String var) {
    putAssign(var, "malloc(sizeof(struct task))");
  }

  private void putFreeTask(String var) {
    putLine("free(" + var + ");");
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

  private void putConstantGoto(String label) {
    putStatement("goto " + label);
  }

  private void putComputedGoto(String address) {
    putStatement("goto *" + address);
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
}
