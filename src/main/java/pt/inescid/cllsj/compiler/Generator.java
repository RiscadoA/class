package pt.inescid.cllsj.compiler;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.Stack;
import pt.inescid.cllsj.Env;
import pt.inescid.cllsj.EnvEntry;
import pt.inescid.cllsj.TypeEntry;
import pt.inescid.cllsj.ast.ASTNodeVisitor;
import pt.inescid.cllsj.ast.nodes.ASTBang;
import pt.inescid.cllsj.ast.nodes.ASTCall;
import pt.inescid.cllsj.ast.nodes.ASTCase;
import pt.inescid.cllsj.ast.nodes.ASTClose;
import pt.inescid.cllsj.ast.nodes.ASTCoClose;
import pt.inescid.cllsj.ast.nodes.ASTCut;
import pt.inescid.cllsj.ast.nodes.ASTEmpty;
import pt.inescid.cllsj.ast.nodes.ASTFwd;
import pt.inescid.cllsj.ast.nodes.ASTId;
import pt.inescid.cllsj.ast.nodes.ASTMix;
import pt.inescid.cllsj.ast.nodes.ASTNode;
import pt.inescid.cllsj.ast.nodes.ASTPrintLn;
import pt.inescid.cllsj.ast.nodes.ASTProcDef;
import pt.inescid.cllsj.ast.nodes.ASTProgram;
import pt.inescid.cllsj.ast.nodes.ASTRecv;
import pt.inescid.cllsj.ast.nodes.ASTSelect;
import pt.inescid.cllsj.ast.nodes.ASTSend;
import pt.inescid.cllsj.ast.nodes.ASTString;
import pt.inescid.cllsj.ast.nodes.ASTUnfold;
import pt.inescid.cllsj.ast.nodes.ASTWhy;
import pt.inescid.cllsj.ast.types.ASTIdT;
import pt.inescid.cllsj.ast.types.ASTNotT;
import pt.inescid.cllsj.ast.types.ASTType;

public class Generator extends ASTNodeVisitor {
  private String generatedCode = "";
  private int indentLevel = 0;
  private int labelIndex = 0;
  private Stack<Environment> environments = new Stack<>();
  private Stack<String> wrappingComments = new Stack<>();
  private Map<String, Environment> procDefEnvs = new HashMap<>();
  private boolean trace = false;

  public static String generate(
      String entryProcess, Env<EnvEntry> ep, ASTProgram program, boolean trace) {
    Generator generator = new Generator();
    generator.trace = trace;

    generator.putLine("#include <stdlib.h>");
    generator.putLine("#include <stdio.h>");
    generator.putLine("#include <string.h>");
    generator.putLine("");
    generator.putLine("struct record {");
    generator.indentLevel++;
    generator.putLine("void* cont;");
    generator.putLine("struct environment* env;");
    generator.putLine("unsigned char* read;");
    generator.putLine("unsigned char* write;");
    generator.putLine("int index;");
    generator.indentLevel--;
    generator.putLine("};");
    generator.putLine("");
    generator.putLine("struct type_var {");
    generator.indentLevel++;
    generator.putLine("int size;");
    generator.putLine("unsigned char polarity;");
    generator.indentLevel--;
    generator.putLine("};");
    generator.putLine("");
    generator.putLine("struct task {");
    generator.indentLevel++;
    generator.putLine("void* cont;");
    generator.putLine("struct environment* env;");
    generator.putLine("struct task* next;");
    generator.indentLevel--;
    generator.putLine("};");
    generator.putLine("");
    generator.putLine("struct environment {");
    generator.indentLevel++;
    generator.putLine("struct environment* parent;");
    generator.indentLevel--;
    generator.putLine("};");
    generator.putLine("");
    generator.putLine(
        "#define ENV_RECORD(env, i) (*(struct record**)(((unsigned char*)env) + sizeof(struct environment) + sizeof(struct record*) * i))");
    generator.putLine(
        "#define ENV_TYPE_VAR(env, r, i) (*(struct type_var*)(((unsigned char*)env) + sizeof(struct environment) + sizeof(struct record*) * r + sizeof(struct type_var) * i))");
    generator.putLine("");
    generator.putLine("int main() {");
    generator.indentLevel++;
    generator.putLine("struct record* tmp_session;");
    generator.putLine("struct environment* env = NULL;");
    generator.putLine("struct environment* tmp_env;");
    generator.putLine("void* tmp_cont;");
    generator.putLine("struct task* next_task = malloc(sizeof(struct task));");
    generator.putLine("next_task->cont = &&end;");
    generator.putLine("struct task* tmp_task;");
    generator.putLine("");
    generator.putLine("goto run;");

    Set<String> definedProcs = new HashSet<>();
    for (ASTProcDef procDef : program.getProcDefs()) {
      if (!definedProcs.add(procDef.getId())) {
        throw new RuntimeException("Duplicate process definition: " + procDef.getId());
      }

      if (procDef.getId().equals(entryProcess)
          && procDef.getArgs().size() + procDef.getGArgs().size() > 0) {
        throw new RuntimeException("Entry process \"" + entryProcess + "\" cannot have arguments");
      }

      Env<EnvEntry> procEp = ep;
      for (String arg : procDef.getTArgs()) {
        procEp = procEp.assoc(arg, new TypeEntry(new ASTIdT(arg)));
      }

      // Setup process environment
      Environment env = new Environment(procEp);
      for (String arg : procDef.getTArgs()) {
        env.insertTypeVar(arg);
      }
      for (int i = 0; i < procDef.getArgs().size(); i++) {
        env.insert(procDef.getArgs().get(i), procDef.getArgTypes().get(i));
      }
      for (int i = 0; i < procDef.getGArgs().size(); i++) {
        env.insert(procDef.getGArgs().get(i), procDef.getGArgTypes().get(i));
      }
      env.insertFromNode(procDef.getRhs());
      generator.procDefEnvs.put(procDef.getId(), env);

      generator.environments.push(env);
      generator.putLine("");
      generator.putLabel("proc_" + procDef.getId());
      procDef.getRhs().accept(generator);
      generator.environments.pop();
    }

    if (!definedProcs.contains(entryProcess)) {
      throw new RuntimeException("Entry process process \"" + entryProcess + "\" not found");
    }

    Environment env = generator.procDefEnvs.get(entryProcess);
    generator.putLine("");
    generator.putLabel("run");
    generator.putLine(
        "env = " + generator.allocEnvironment(env.getSize(), env.getTypeVarCount()) + ";");
    generator.putLine("goto proc_" + entryProcess + ";");
    generator.putLabel("end");
    generator.putLine("return 0;");
    generator.indentLevel--;
    generator.putLine("}");

    return generator.generatedCode;
  }

  @Override
  public void visit(ASTNode node) {
    throw new UnsupportedOperationException(
        "Nodes of type "
            + node.getClass().getSimpleName()
            + " are not yet supported by the generator");
  }

  @Override
  public void visit(ASTBang node) {
    this.pushWrappingComment("!(" + node.getChr() + "):" + node.lineno);
    this.putTrace("!(" + node.getChr() + "):" + node.lineno);

    String replRhs = this.makeLabel("repl_" + node.getChr() + "_" + node.getChi() + "_rhs");

    // We need a new, fresh environment for the replicated right hand side.
    // This new environment keeps a pointer to the parent environment, so that it can access any
    // exponential variables in the parent environment.
    Environment env = new Environment(this.environment().getEp(), this.environment());
    env.insert(node.getChi(), node.getType());
    env.insertFromNode(node.getRhs());
    assert env.getIndex(node.getChi()) == 0
        : "Replicated session must have index 0 in it's environment";

    // We send the continuation to the session queue and flip to the other side of the session.
    this.pushBang(node.getChr(), env.getSize(), "env", replRhs);
    this.finalFlip(node.getChr());

    // We jump to the right hand side, so that we can generate the left hand side code here.
    this.pushWrappingComment("closure!(" + node.getChi() + "):" + node.lineno);
    this.putLabel(replRhs);
    this.environments.push(env);
    node.getRhs().accept(this);
    this.environments.pop();
    this.popWrappingComment();

    this.popWrappingComment();
  }

  @Override
  public void visit(ASTCall node) {
    this.pushWrappingComment("call(" + node.getChr() + ", " + node.getChi() + "):" + node.lineno);
    this.putTrace("call(" + node.getChr() + ", " + node.getChi() + "):" + node.lineno);

    // The bang node has previously sent an environment size and a continuation to the session
    // queue. We first allocate a new environment with that size.
    this.putLine(
        "tmp_env = " + allocEnvironment(peekBangEnvironmentSize(node.getChr()), "0") + ";");
    this.putLine("tmp_env->parent = " + peekBangEnvironmentParent(node.getChr()) + ";");

    // Then, we initialize the session with the new environment and continuation.
    String chiPtr = this.sessionPointer("tmp_env", 0, node.getChi());
    this.initSession(
        chiPtr, SizeCalculator.calculate(new Env<>(), node.getType(), new HashMap<>()));
    this.putLine(sessionEnv(chiPtr) + " = tmp_env;");
    this.putLine(sessionCont(chiPtr) + " = " + peekBangContinuation(node.getChr()) + ";");
    this.putLine(sessionIndex(chiPtr) + " = 0;");

    // We also set the record pointer of the current environment to point to the new session.
    this.putLine(sessionPointer(node.getChi()) + " = " + chiPtr + ";");

    // If we're reading from the channel, we pass control to the other side first.
    this.branchOnTypePolarity(
        node.getType(),
        () -> {},
        () -> {
          // If we'll be reading from the channel, we pass control to the other side first.
          this.flip(node.getChi(), "polarity");
        });

    // Finally, we generate the continuation, as usual.
    node.getRhs().accept(this);

    this.popWrappingComment();
  }

  @Override
  public void visit(ASTCase node) {
    this.pushWrappingComment("case(" + node.getCh() + "):" + node.lineno);

    // We pop a label from the session queue and use it as the switch expression.
    this.putLine("switch (" + this.popLabel(node.getCh()) + ") {");
    this.indentLevel++;

    for (int i = 0; i < node.getCaseCount(); i++) {
      String label = node.getCaseLabelFromIndex(i);
      this.putLine("case " + i + ":");
      this.indentLevel++;

      this.pushWrappingComment("case" + label + "(" + node.getCh() + "):" + node.lineno);
      this.putTrace("case" + label + "(" + node.getCh() + "):" + node.lineno);
      node.getCase(label).accept(this);
      this.popWrappingComment();

      this.indentLevel--;
    }

    this.indentLevel--;
    this.putLine("}");

    this.popWrappingComment();
  }

  @Override
  public void visit(ASTClose node) {
    this.pushWrappingComment("close(" + node.getCh() + "):" + node.lineno);
    this.putTrace("close(" + node.getCh() + "):" + node.lineno);

    // Push a close token onto the session queue and flip to the other side of the session.
    this.pushClose(node.getCh());
    this.finalFlip(node.getCh());

    this.popWrappingComment();
  }

  @Override
  public void visit(ASTCoClose node) {
    this.pushWrappingComment("coclose(" + node.getCh() + "):" + node.lineno);
    this.putTrace("coclose(" + node.getCh() + "):" + node.lineno);

    // Pop a close token from the session queue, terminate the session, and generate the
    // continuation.
    this.popClose(node.getCh());
    this.putLine("free(" + this.sessionPointer(node.getCh()) + ");");
    node.getRhs().accept(this);

    this.popWrappingComment();
  }

  @Override
  public void visit(ASTCut node) {
    this.pushWrappingComment("cut(" + node.getCh() + "):" + node.lineno);
    this.putTrace("cut(" + node.getCh() + "):" + node.lineno);

    // Initialize the cut session, with initial continuation pointing to the right hand side.
    String cutLhs = this.makeLabel("cut_lhs_" + node.getCh());
    String cutRhs = this.makeLabel("cut_rhs_" + node.getCh());
    String chPtr = this.sessionPointer(node.getCh());
    this.initSession(chPtr, environment().getSessionCSize("env", node.getCh()));
    this.putLine(sessionEnv(chPtr) + " = env;");
    this.putLine(sessionIndex(chPtr) + " = " + environment().getIndex(node.getCh()) + ";");

    // The first code to be executed for a given session must be positive.
    // Thus, if the session is positive, we execute first the right hand side.
    // Otherwise, we execute first the left hand side.
    this.branchOnTypePolarity(
        node.getChType(),
        () -> {
          this.putLine(sessionCont(chPtr) + " = &&" + cutLhs + ";");
          this.putLine("goto " + cutRhs + ";");
        },
        () -> {
          this.putLine(sessionCont(chPtr) + " = &&" + cutRhs + ";");
          this.putLine("goto " + cutLhs + ";");
        });

    // The first code to be executed for a given session must be positive.
    this.putLabel(cutLhs);
    node.getLhs().accept(this);

    // The right hand side code will only execute after the left hand side jumps to it.
    this.putLabel(cutRhs);
    node.getRhs().accept(this);

    this.popWrappingComment();
  }

  @Override
  public void visit(ASTEmpty node) {
    this.pushWrappingComment("empty:" + node.lineno);

    this.putTrace("empty():" + node.lineno);
    putLine("tmp_task = next_task->next;");
    putLine("env = next_task->env;");
    putLine("tmp_cont = next_task->cont;");
    putLine("free(next_task);");
    putLine("next_task = tmp_task;");
    putLine("goto *tmp_cont;");

    this.popWrappingComment();
  }

  private void fwdUtil(ASTFwd node, String positive, String negative) {
    String negativePtr = this.sessionPointer(negative);
    String positivePtr = this.sessionPointer(positive);

    // Since the negative session has a negative type, we must have previously flipped to it, and
    // are now reading the data it has written.
    //
    // Since the positive session has a positive type, we might have already written data to it.
    //
    // Thus, we should jump to the positive session's endpoint, and make sure that it first reads
    // the data we wrote to it, and after that, it reads the data already in the negative session's
    // buffer.

    this.putTrace("fwd(-" + negative + ", +" + positive + "):" + node.lineno);
    this.putLine("tmp_env = " + sessionEnv(positivePtr) + ";");
    this.putLine("tmp_cont = " + sessionCont(positivePtr) + ";");

    // Append the negative session's buffer to the positive session's buffer.
    this.beginLine("memcpy(" + sessionWrite(positivePtr) + ", ");
    this.continueLine(sessionRead(negativePtr) + ", ");
    this.endLine(sessionWrite(negativePtr) + " - " + sessionRead(negativePtr) + ");");
    this.beginLine(sessionWrite(positivePtr) + " += ");
    this.endLine(sessionWrite(negativePtr) + " - " + sessionRead(negativePtr) + ";");

    // Set the continuation of the positive channel to the continuation of the negative channel.
    this.putLine(sessionEnv(positivePtr) + " = " + sessionEnv(negativePtr) + ";");
    this.putLine(sessionCont(positivePtr) + " = " + sessionCont(negativePtr) + ";");
    this.putLine(sessionIndex(positivePtr) + " = " + sessionIndex(negativePtr) + ";");

    // We make the negative channel bindings point to the positive channel, and
    // terminate the
    // old negative channel.
    this.putLine("tmp_session = " + negativePtr + ";");
    this.putLine(
        "ENV_RECORD("
            + sessionEnv(negativePtr)
            + ", "
            + sessionIndex(negativePtr)
            + ") = "
            + positivePtr
            + ";");
    this.putLine("free(tmp_session);");

    this.putLine("env = tmp_env;");
    this.putLine("goto *tmp_cont;");
  }

  @Override
  public void visit(ASTFwd node) {
    this.pushWrappingComment("fwd(" + node.getCh1() + ", " + node.getCh2() + "):" + node.lineno);

    this.branchOnTypePolarity(
        node.getCh2Type(),
        () -> {
          fwdUtil(node, node.getCh2(), node.getCh1());
        },
        () -> {
          fwdUtil(node, node.getCh1(), node.getCh2());
        });

    this.popWrappingComment();
  }

  @Override
  public void visit(ASTId node) {
    this.pushWrappingComment("id(" + node.getId() + "):" + node.lineno);
    this.putTrace("id(" + node.getId() + "):" + node.lineno);

    // We first initialize a new environment for the process.
    Environment env = this.procDefEnvs.get(node.getId());
    this.putLine("tmp_env = " + allocEnvironment(env.getSize(), env.getTypeVarCount()) + ";");

    // Add the sizes of the type variables to the environment.
    Map<String, String> typeVarSizes = new HashMap<>();
    Environment parent = environment();
    while (parent != null) {
      for (Map.Entry<String, Integer> var : environment().getTypeVarIndices().entrySet()) {
        if (!typeVarSizes.containsKey(var.getKey())) {
          typeVarSizes.put(var.getKey(), typeVar(var.getKey()) + ".size");
        }
      }
      parent = parent.getParent();
    }
    for (int i = 0; i < node.getTPars().size(); ++i) {
      ASTType type = node.getTPars().get(i);
      this.beginLine("ENV_TYPE_VAR(tmp_env, " + env.getSize() + ", " + i + ").size = ");
      this.endLine(SizeCalculator.calculate(environment().getEp(), type, typeVarSizes) + ";");
      this.beginLine("ENV_TYPE_VAR(tmp_env, " + env.getSize() + ", " + i + ").polarity = ");

      if (type instanceof ASTIdT && typeVarSizes.containsKey(((ASTIdT) type).getid())) {
        String id = ((ASTIdT) type).getid();
        this.endLine(typeVar(id) + ".polarity;");
      } else if (type.isPosCatch(environment().getEp())) {
        this.endLine("1;");
      } else {
        this.endLine("0;");
      }
    }

    // Add the linear arguments to the environment.
    for (int i = 0; i < node.getPars().size(); ++i) {
      int envI = i;
      String ptr = sessionPointer(node.getPars().get(i));
      this.putLine(sessionPointer("tmp_env", envI, env.getName(envI)) + " = " + ptr + ";");
    }

    // Add the exponential arguments to the environment.
    for (int i = 0; i < node.getGPars().size(); ++i) {
      int envI = i + node.getPars().size();
      this.putLine(
          sessionPointer("tmp_env", envI, env.getName(envI))
              + " = "
              + sessionPointer(node.getGPars().get(i))
              + ";");
    }

    // Then we switch to that environment and jump to the process code.
    this.putLine("env = tmp_env;");
    this.putLine("goto proc_" + node.getId() + ";");

    this.popWrappingComment();
  }

  @Override
  public void visit(ASTMix node) {
    this.pushWrappingComment("mix:" + node.lineno);
    this.putTrace("mix(lhs):" + node.lineno);

    // We push the continuation for the right hand side to the global stack,
    // and generate code for the left hand side.
    String mixRhs = this.makeLabel("mix_rhs");
    this.putLine("tmp_task = malloc(sizeof(struct task));");
    this.putLine("tmp_task->cont = &&" + mixRhs + ";");
    this.putLine("tmp_task->env = env;");
    this.putLine("tmp_task->next = next_task;");
    this.putLine("next_task = tmp_task;");
    node.getLhs().accept(this);

    // The right hand side code won't run until an empty node pops it from the stack.
    this.putLabel(mixRhs);
    this.putTrace("mix(rhs):" + node.lineno);
    node.getRhs().accept(this);

    this.popWrappingComment();
  }

  @Override
  public void visit(ASTPrintLn node) {
    this.pushWrappingComment("println:" + node.lineno);

    if (!(node.getExpr() instanceof ASTString)) {
      throw new UnsupportedOperationException(
          "Only string literals are supported in println for now");
    }

    ASTString string = (ASTString) node.getExpr();
    if (this.trace) {
      if (node.withNewLine()) {
        this.putTrace("println(\"" + string.getV() + "\"):" + node.lineno);
      } else {
        this.putTrace("print(\"" + string.getV() + "\"):" + node.lineno);
      }
    } else {
      this.putPrint(string.getV(), node.withNewLine());
    }
    node.getRhs().accept(this);

    this.popWrappingComment();
  }

  @Override
  public void visit(ASTRecv node) {
    this.pushWrappingComment("recv(" + node.getChr() + ", " + node.getChi() + "):" + node.lineno);
    this.putTrace("recv(" + node.getChr() + ", " + node.getChi() + "):" + node.lineno);

    // We simply pop the record from the queue and store it in the environment.
    this.putLine(this.sessionPointer(node.getChi()) + " = " + this.popRecord(node.getChr()) + ";");
    this.branchOnTypePolarity(
        node.getChiType(),
        () -> {},
        () -> {
          // If we'll be reading from the channel, we pass control to the other side first.
          this.flip(node.getChi(), "polarity");
        });

    node.getRhs().accept(this);

    this.popWrappingComment();
  }

  @Override
  public void visit(ASTSelect node) {
    this.pushWrappingComment("select" + node.getLabel() + "(" + node.getCh() + "):" + node.lineno);
    this.putTrace("select" + node.getLabel() + "(" + node.getCh() + "):" + node.lineno);

    // Push the label's index onto the session queue.
    this.pushLabel(node.getCh(), node.getLabelIndex(), node.getLabel());
    this.branchOnTypePolarity(
        node.getRhsType(),
        () -> {},
        () -> {
          this.flip(node.getCh(), "polarity");
        });
    node.getRhs().accept(this);

    this.popWrappingComment();
  }

  @Override
  public void visit(ASTSend node) {
    this.pushWrappingComment("send(" + node.getChs() + "):" + node.lineno);
    this.putTrace("send(" + node.getChs() + "):" + node.lineno);

    String sendLhs = this.makeLabel("send_" + node.getChs() + "_" + node.getCho() + "_lhs");
    String sendRhs = this.makeLabel("send_" + node.getChs() + "_" + node.getCho() + "_rhs");

    // We initialize a new session record.
    String choPtr = this.sessionPointer(node.getCho());
    this.initSession(choPtr, environment().getSessionCSize("env", node.getCho()));
    this.putLine(sessionCont(choPtr) + " = &&" + sendLhs + ";");
    this.putLine(sessionEnv(choPtr) + " = env;");
    this.putLine(sessionIndex(choPtr) + " = " + environment().getIndex(node.getCho()) + ";");

    // We send the session record to the session queue.
    this.pushRecord(node.getChs(), choPtr);

    // We jump to the right hand side, so that we can generate the left hand side code here.
    this.pushWrappingComment("closure(" + node.getCho() + "):" + node.lineno);
    this.putLine("goto " + sendRhs + ";");
    this.putLabel(sendLhs);
    node.getLhs().accept(this);
    this.popWrappingComment();

    // In the right hand side, we simply continue as usual.
    this.putLabel(sendRhs);
    this.branchOnTypePolarity(
        node.getRhsType(),
        () -> {},
        () -> {
          this.flip(node.getChs(), "polarity");
        });
    node.getRhs().accept(this);

    this.popWrappingComment();
  }

  @Override
  public void visit(ASTUnfold node) {
    String unfoldType = node.rec ? "unfold-send" : "unfold-recv";

    this.pushWrappingComment(unfoldType + "(" + node.getCh() + "):" + node.lineno);
    this.putTrace(unfoldType + "(" + node.getCh() + "):" + node.lineno);

    // We either push or pop an unfold token onto the session queue, depending on the side we're on.
    if (node.rec) {
      this.pushUnfold(node.getCh());

      // If we're writing, we must give a chance for the other side to read anything we've written
      // before.
      this.flip(node.getCh(), "unfold-send");
    } else {
      this.popUnfold(node.getCh());

      // Pass control back to the writer.
      this.flip(node.getCh(), "unfold-recv");
    }

    // Now, we reset our buffer pointers, since we're recursing.
    String ptr = this.sessionPointer(node.getCh());
    if (node.rec) {
      this.putLine(sessionWrite(ptr) + " = " + sessionBuffer(ptr) + ";");
    } else {
      this.putLine(sessionRead(ptr) + " = " + sessionBuffer(ptr) + ";");
    }

    // If we are the positive side and the next operation will be negative, we must flip.
    if (node.rec) {
      this.branchOnTypePolarity(
          node.getRhsType(),
          () -> {},
          () -> {
            this.flip(node.getCh(), "polarity");
          });
    }

    node.getRhs().accept(this);

    this.popWrappingComment();
  }

  @Override
  public void visit(ASTWhy node) {
    this.pushWrappingComment("?(" + node.getCh() + "):" + node.lineno);
    this.putTrace("?(" + node.getCh() + "):" + node.lineno);

    // This node doesn't really do anything - the continuation sent by the bang node will never be
    // popped.
    // Future call nodes will simply peek the continuation from the queue.
    node.getRhs().accept(this);

    this.popWrappingComment();
  }

  private void pushClose(String ch) {
    // The close token is zero-sized, so we don't actually do anything.
  }

  private void popClose(String ch) {
    // The close token is zero-sized, so we don't actually do anything.
  }

  private void pushUnfold(String ch) {
    // The unfold token is zero-sized, so we don't actually do anything.
  }

  private void popUnfold(String ch) {
    // The unfold token is zero-sized, so we don't actually do anything.
  }

  // Creates a new statement which pushes a label onto the session queue.
  private void pushLabel(String ch, int labelIndex, String label) {
    pushLiteral(ch, "unsigned char", Integer.toString(labelIndex) + " /* " + label + " */");
  }

  // Creates a new expression which pops a label from the session queue.
  private String popLabel(String ch) {
    return popLiteral(ch, "unsigned char");
  }

  // Creates a new statement which pushes a record pointer onto the session queue.
  private void pushRecord(String ch, String expression) {
    pushLiteral(ch, "struct record*", expression);
  }

  // Creates a new expression which pops a record pointer from the session queue.
  private String popRecord(String ch) {
    return popLiteral(ch, "struct record*");
  }

  // Creates a new statement which pushes an environment size, parent and a continuation onto the
  // session
  // queue.
  private void pushBang(String ch, int envSize, String parent, String cont) {
    pushLiteral(ch, "int", Integer.toString(envSize));
    pushLiteral(ch, "struct environment*", parent);
    pushLiteral(ch, "void*", "&&" + cont);
  }

  // Creates a new expression which peeks a bang environment size from the session queue.
  private String peekBangEnvironmentSize(String ch) {
    return peekLiteral(ch, "int", "0");
  }

  // Creates a new expression which peeks a bang environment parent from the session queue.
  private String peekBangEnvironmentParent(String ch) {
    return peekLiteral(ch, "struct environment*", "sizeof(int)");
  }

  // Creates a new expression which peeks a bang continuation from the session queue.
  private String peekBangContinuation(String ch) {
    return peekLiteral(ch, "void*", "sizeof(int) + sizeof(struct environment*)");
  }

  // Creates statements which pushes a literal onto the session queue.
  private void pushLiteral(String ch, String cType, String literal) {
    putLine("*(" + cType + "*)" + sessionWrite(sessionPointer(ch)) + " = " + literal + ";");
    putLine(sessionWrite(sessionPointer(ch)) + " += sizeof(" + cType + ");");
  }

  // Returns a new expression which pops a literal from the session queue.
  private String popLiteral(String ch, String cType) {
    return "*(("
        + cType
        + "*)(("
        + sessionRead(sessionPointer(ch))
        + " += sizeof("
        + cType
        + ")) - sizeof("
        + cType
        + ")))";
  }

  // Returns a new expression which peeks a literal from the session queue.
  private String peekLiteral(String ch, String cType, String offset) {
    return "*(" + cType + "*)(" + sessionRead(sessionPointer(ch)) + " + " + offset + ")";
  }

  private void flip(String ch, String purpose) {
    this.pushWrappingComment("flip(" + ch + "):" + purpose);
    putTrace("flip(" + ch + "):" + purpose);

    String ptr = sessionPointer(ch);
    String label = makeLabel("flip_" + ch);
    putLine("tmp_env = " + sessionEnv(ptr) + ";");
    putLine("tmp_cont = " + sessionCont(ptr) + ";");
    putLine(sessionCont(ptr) + " = &&" + label + ";");
    putLine(sessionEnv(ptr) + " = env;");
    putLine(sessionIndex(ptr) + " = " + environment().getIndex(ch) + ";");
    putLine("env = tmp_env;");
    putLine("goto *tmp_cont;");
    putLabel(label);

    this.popWrappingComment();
  }

  private void finalFlip(String ch) {
    this.pushWrappingComment("finalFlip(" + ch + ")");
    putTrace("finalFlip(" + ch + ")");

    putLine("tmp_session = " + sessionPointer(ch) + ";");
    putLine("env = " + sessionEnv("tmp_session") + ";");
    putLine("goto *" + sessionCont("tmp_session") + ";");

    this.popWrappingComment();
  }

  private String allocEnvironment(int recordCount, int typeVarCount) {
    return this.allocEnvironment(Integer.toString(recordCount), Integer.toString(typeVarCount));
  }

  private String allocEnvironment(String envSize, String typeVarCount) {
    return "malloc(sizeof(struct environment) + sizeof(struct record*) * ("
        + envSize
        + ") + sizeof(struct type_var) * ("
        + typeVarCount
        + "))";
  }

  private void initSession(String ptr, String size) {
    putLine(ptr + " = malloc(sizeof(struct record) + (" + size + "));");
    putLine(sessionRead(ptr) + " = " + sessionWrite(ptr) + " = " + sessionBuffer(ptr) + ";");
  }

  private String sessionBuffer(String ptr) {
    return "((unsigned char*)" + ptr + " + sizeof(struct record))";
  }

  private String sessionPointer(String ch) {
    Environment env = environment();
    String envRef = "env";
    while (!env.isLocal(ch)) {
      envRef += "->parent";
      env = env.getParent();
    }
    return this.sessionPointer(envRef, env.getIndex(ch), ch);
  }

  private String sessionPointer(String env, int ch, String chName) {
    return "ENV_RECORD(" + env + ", " + ch + " /*" + chName + "*/)";
  }

  private String sessionCont(String ptr) {
    return ptr + "->cont";
  }

  private String sessionEnv(String ptr) {
    return ptr + "->env";
  }

  private String sessionRead(String ptr) {
    return ptr + "->read";
  }

  private String sessionWrite(String ptr) {
    return ptr + "->write";
  }

  private String sessionIndex(String ptr) {
    return ptr + "->index";
  }

  private String typeVar(String name) {
    Environment env = environment();
    String envRef = "env";

    while (!env.getTypeVarIndices().containsKey(name)) {
      envRef += "->parent";
      env = env.getParent();
      if (env == null) {
        throw new RuntimeException("Type variable " + name + " not found in environment");
      }
    }

    return "ENV_TYPE_VAR("
        + envRef
        + ", "
        + env.getSize()
        + ", "
        + env.getTypeVarIndex(name)
        + " /*"
        + name
        + "*/)";
  }

  private void branchOnTypePolarity(ASTType type, Runnable onWrite, Runnable onRead) {
    boolean negated = false;
    if (type instanceof ASTNotT) {
      type = ((ASTNotT) type).getin();
      assert type instanceof ASTIdT; // Type checker should guarantee this, right?
      negated = true;
    }

    if (type instanceof ASTIdT) {
      try {
        type = type.unfoldType(environment().getEp());
      } catch (Exception e) {
        e.printStackTrace(System.out);
        System.exit(1);
      }
    }

    if (type instanceof ASTIdT) {
      // We need to do it at runtime, as we don't know the type of the variable at compile time.
      this.putLine(
          "if (" + (negated ? "!" : "") + typeVar(((ASTIdT) type).getid()) + ".polarity) {");
      this.indentLevel += 1;
      onWrite.run();
      this.indentLevel -= 1;
      this.putLine("} else {");
      this.indentLevel += 1;
      onRead.run();
      this.indentLevel -= 1;
      this.putLine("}");
    } else if (negated ^ type.isPosCatch(environment().getEp())) {
      onWrite.run();
    } else {
      onRead.run();
    }
  }

  private void putTrace(String msg) {
    if (this.trace) {
      this.putPrint(msg, true);
    }
  }

  private void putPrint(String msg, boolean newline) {
    this.putLine("fputs(\"" + escapeString(msg) + (newline ? "\\n" : "") + "\", stdout);");
  }

  // Adds an indented line to the generated code
  private void putLine(String line) {
    this.beginLine(line);
    this.endLine("");
  }

  // Adds an indented line to the generated code, without a newline at the end
  private void beginLine(String line) {
    if (!line.isBlank()) {
      generatedCode += "  ".repeat(indentLevel) + line;
    }
  }

  private void continueLine(String line) {
    generatedCode += line;
  }

  private void endLine(String line) {
    generatedCode += line + "\n";
  }

  // Adds a label to the generated code
  private void putLabel(String label) {
    generatedCode += label + ":\n";
  }

  private String escapeString(String string) {
    return string.replace("\\", "\\\\").replace("\"", "\\\"").replace("\n", "\\n");
  }

  private String makeLabel(String prefix) {
    return prefix + "_lbl_" + labelIndex++;
  }

  private Environment environment() {
    return environments.peek();
  }

  private void pushWrappingComment(String comment) {
    wrappingComments.push(comment);
    putLine("/* BEGIN " + comment + " */");
    this.indentLevel++;
  }

  private void popWrappingComment() {
    this.indentLevel--;
    putLine("/* END " + wrappingComments.pop() + " */");
  }
}
