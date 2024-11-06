package pt.inescid.cllsj.compiler;

import java.util.Stack;
import pt.inescid.cllsj.ast.ASTNodeVisitor;
import pt.inescid.cllsj.ast.nodes.ASTBang;
import pt.inescid.cllsj.ast.nodes.ASTCall;
import pt.inescid.cllsj.ast.nodes.ASTCase;
import pt.inescid.cllsj.ast.nodes.ASTClose;
import pt.inescid.cllsj.ast.nodes.ASTCoClose;
import pt.inescid.cllsj.ast.nodes.ASTCut;
import pt.inescid.cllsj.ast.nodes.ASTEmpty;
import pt.inescid.cllsj.ast.nodes.ASTFwd;
import pt.inescid.cllsj.ast.nodes.ASTMix;
import pt.inescid.cllsj.ast.nodes.ASTNode;
import pt.inescid.cllsj.ast.nodes.ASTRecv;
import pt.inescid.cllsj.ast.nodes.ASTSelect;
import pt.inescid.cllsj.ast.nodes.ASTSend;
import pt.inescid.cllsj.ast.nodes.ASTWhy;

public class Generator extends ASTNodeVisitor {
  private String generatedCode = "";
  private int indentLevel = 0;
  private int labelIndex = 0;
  private Stack<Environment> environments = new Stack<>();
  private Stack<String> wrappingComments = new Stack<>();

  public static String generate(ASTNode ast) {
    Generator generator = new Generator();

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
    generator.putLine("struct record* records[];");
    generator.indentLevel--;
    generator.putLine("};");
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

    ast.accept(generator);

    generator.putLine("end:");
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
    this.putPrint("!(" + node.getChr() + "):" + node.lineno);

    String replRhs = this.makeLabel("repl_" + node.getChr() + "_" + node.getChi() + "_rhs");

    // We need a new, fresh environment for the replicated right hand side.
    // This new environment keeps a pointer to the parent environment, so that it can access any
    // exponential variables in the parent environment.
    Environment env = new Environment(this.environment());
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
    env.setPolarity(node.getChi(), false); // This won't ever be the first end point.
    this.generateContinuation(node.getRhs());
    this.environments.pop();
    this.popWrappingComment();

    this.popWrappingComment();
  }

  @Override
  public void visit(ASTCall node) {
    this.pushWrappingComment("call(" + node.getChr() + ", " + node.getChi() + "):" + node.lineno);
    this.putPrint("call(" + node.getChr() + ", " + node.getChi() + "):" + node.lineno);

    // The bang node has previously sent an environment size and a continuation to the session
    // queue. We first allocate a new environment with that size.
    this.putLine("tmp_env = " + allocEnvironment(peekBangEnvironmentSize(node.getChr())) + ";");
    this.putLine("tmp_env->parent = " + peekBangEnvironmentParent(node.getChr()) + ";");

    // Then, we initialize the session with the new environment and continuation.
    String chiPtr = this.sessionPointer("tmp_env", 0, node.getChi());
    this.initSession(chiPtr, SizeCalculator.calculate(node.getType()));
    this.putLine(sessionEnv(chiPtr) + " = tmp_env;");
    this.putLine(sessionCont(chiPtr) + " = " + peekBangContinuation(node.getChr()) + ";");
    this.putLine(sessionIndex(chiPtr) + " = 0;");

    // We also set the record pointer of the current environment to point to the new session.
    this.putLine(sessionPointer(node.getChi()) + " = " + chiPtr + ";");

    // Finally, we generate the continuation, as usual.
    this.generateContinuation(node.getRhs());

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

      this.environments.push(this.environment().copy());
      this.pushWrappingComment("case" + label + "(" + node.getCh() + "):" + node.lineno);
      this.putPrint("case" + label + "(" + node.getCh() + "):" + node.lineno);
      this.generateContinuation(node.getCase(label));
      this.popWrappingComment();
      this.environments.pop();

      this.indentLevel--;
    }

    this.indentLevel--;
    this.putLine("}");

    this.popWrappingComment();
  }

  @Override
  public void visit(ASTClose node) {
    this.pushWrappingComment("close(" + node.getCh() + "):" + node.lineno);
    this.putPrint("close(" + node.getCh() + "):" + node.lineno);

    // Push a close token onto the session queue and flip to the other side of the session.
    this.pushClose(node.getCh());
    this.finalFlip(node.getCh());

    this.popWrappingComment();
  }

  @Override
  public void visit(ASTCoClose node) {
    this.pushWrappingComment("coclose(" + node.getCh() + "):" + node.lineno);
    this.putPrint("coclose(" + node.getCh() + "):" + node.lineno);

    // Pop a close token from the session queue, terminate the session, and generate the
    // continuation.
    this.popClose(node.getCh());
    this.putLine("free(" + this.sessionPointer(node.getCh()) + ");");
    this.generateContinuation(node.getRhs());

    this.popWrappingComment();
  }

  @Override
  public void visit(ASTCut node) {
    this.pushWrappingComment("cut(" + node.getCh() + "):" + node.lineno);
    this.putPrint("cut(" + node.getCh() + "):" + node.lineno);
    this.pushScope(node);

    // Initialize the cut session, with initial continuation pointing to the right hand side.
    String cutRhs = this.makeLabel("cut_" + node.getCh());
    String chPtr = this.sessionPointer(node.getCh());
    this.initSession(chPtr, environment().getSessionCSize(node.getCh()));
    this.putLine(sessionCont(chPtr) + " = &&" + cutRhs + ";");
    this.putLine(sessionEnv(chPtr) + " = env;");
    this.putLine(sessionIndex(chPtr) + " = " + environment().getIndex(node.getCh()) + ";");

    ASTNode positive, negative;
    if (node.getChType().isPos()) {
      positive = node.getRhs();
      negative = node.getLhs();
    } else {
      positive = node.getLhs();
      negative = node.getRhs();
    }

    // The first code to be executed for a given session must be positive.
    this.environment().setPolarity(node.getCh(), true);
    this.generateContinuation(positive);

    // The right hand side code will only execute after the left hand side jumps to it.
    // Thus, we set the initial polarity to negative, so that it initially doesn't flip back when
    // reading.
    // It shouldn't flip back as the positive code has already run and written to the channel.
    this.putLabel(cutRhs);
    this.environment().setPolarity(node.getCh(), false);
    this.generateContinuation(negative);

    this.popScope();
    this.popWrappingComment();
  }

  @Override
  public void visit(ASTEmpty node) {
    this.pushWrappingComment("empty:" + node.lineno);

    this.putPrint("empty():" + node.lineno);
    putLine("tmp_task = next_task->next;");
    putLine("env = next_task->env;");
    putLine("tmp_cont = next_task->cont;");
    putLine("free(next_task);");
    putLine("next_task = tmp_task;");
    putLine("goto *tmp_cont;");

    this.popWrappingComment();
  }

  @Override
  public void visit(ASTFwd node) {
    this.pushWrappingComment("fwd(" + node.getCh1() + ", " + node.getCh2() + "):" + node.lineno);

    // Check which of the channels is positive.
    String negative, positive;
    if (node.getCh2Type().isPos()) {
      positive = node.getCh2();
      negative = node.getCh1();
    } else {
      positive = node.getCh1();
      negative = node.getCh2();
    }

    String negativePtr = this.sessionPointer(negative);
    String positivePtr = this.sessionPointer(positive);

    if (this.environment().getPolarity(negative)) {
      // We've previously written to the negative channel.
      // Thus, the negative channel may have data on its buffer that it still hasn't read.
      // We pass control to the negative channel, so that it can read that data that is already in
      // the buffer.

      this.putLine("tmp_env = " + sessionEnv(negativePtr) + ";");
      this.putLine("tmp_cont = " + sessionCont(negativePtr) + ";");

      if (this.environment().getPolarity(positive)) {
        this.putPrint("fwd(W " + negative + ", W " + positive + "):" + node.lineno);

        // We've previously written to the positive channel.
        // Thus, the positive channel might have data on its buffer that it still hasn't read.
        //
        // In this case, we should append what we've already written in the positive channel to
        // the negative channel's buffer - basically pretending that it was written by the negative
        // channel.

        this.beginLine("memcpy(" + sessionWrite(negativePtr) + ", ");
        this.continueLine(sessionRead(positivePtr) + ", ");
        this.endLine(sessionWrite(positivePtr) + " - " + sessionRead(positivePtr) + ");");

        this.beginLine(sessionWrite(negativePtr) + " += ");
        this.endLine(sessionWrite(positivePtr) + " - " + sessionRead(positivePtr) + ";");
      } else {
        this.putPrint("fwd(W " + negative + ", R " + positive + "):" + node.lineno);

        // We've previously read from the positive channel.
        // Thus, since we're now writing to it, we must have already read all the data sent by it.
        // So, we can ignore the contents of the positive channel's buffer, and do nothing.
      }

      // We need to set the continuation of the negative channel to the continuation of the positive
      // channel.
      this.putLine(sessionEnv(negativePtr) + " = " + sessionEnv(positivePtr) + ";");
      this.putLine(sessionCont(negativePtr) + " = " + sessionCont(positivePtr) + ";");

      // We make the positive channel bindings point to the negative channel, and terminate the old
      // positive channel.
      this.putLine("tmp_session = " + positivePtr + ";");
      this.putLine(
          sessionEnv(positivePtr)
              + "->records["
              + sessionIndex(positivePtr)
              + "] = "
              + negativePtr
              + ";");
      this.putLine("free(tmp_session);");
    } else {
      // We've previously read from the negative channel.
      // Thus, data meant for the positive channel will already have been pushed to the negative
      // channel's buffer.
      // We pass control to the positive channel, so that it can read that data that is already in
      // the buffer.

      this.putLine("tmp_env = " + sessionEnv(positivePtr) + ";");
      this.putLine("tmp_cont = " + sessionCont(positivePtr) + ";");

      if (this.environment().getPolarity(positive)) {
        this.putPrint("fwd(R " + negative + ", W " + positive + "):" + node.lineno);

        // We've previously written to the positive channel.
        // Thus, the positive channel might have data on its buffer that it still hasn't read.
        // In this case, we should append the extra data from the negative channel's buffer to the
        // positive channel's buffer.

        this.beginLine("memcpy(" + sessionWrite(positivePtr) + ", ");
        this.continueLine(sessionRead(negativePtr) + ", ");
        this.endLine(sessionWrite(negativePtr) + " - " + sessionRead(negativePtr) + ");");

        this.beginLine(sessionWrite(positivePtr) + " += ");
        this.endLine(sessionWrite(negativePtr) + " - " + sessionRead(negativePtr) + ";");

        // We need to set the continuation of the positive channel to the continuation of the
        // negative channel.
        this.putLine(sessionEnv(positivePtr) + " = " + sessionEnv(negativePtr) + ";");
        this.putLine(sessionCont(positivePtr) + " = " + sessionCont(negativePtr) + ";");

        // We make the negative channel bindings point to the positive channel, and terminate the
        // old negative channel.
        this.putLine("tmp_session = " + negativePtr + ";");
        this.putLine(
            sessionEnv(negativePtr)
                + "->records["
                + sessionIndex(negativePtr)
                + "] = "
                + positivePtr
                + ";");
        this.putLine("free(tmp_session);");
      } else {
        this.putPrint("fwd(R " + negative + ", R " + positive + "):" + node.lineno);

        // We've previously read from the positive channel.
        // Thus, since we're now writing to it, we must have already read all the data sent by it.
        // So, we can ignore the contents of the positive channel's buffer.

        // We make the positive channel bindings point to the negative channel, and terminate the
        // old positive channel.
        this.putLine("tmp_session = " + positivePtr + ";");
        this.putLine(
            sessionEnv(positivePtr)
                + "->records["
                + sessionIndex(positivePtr)
                + "] = "
                + negativePtr
                + ";");
        this.putLine("free(tmp_session);");
      }
    }

    this.putLine("env = tmp_env;");
    this.putLine("goto *tmp_cont;");
  }

  @Override
  public void visit(ASTMix node) {
    this.pushWrappingComment("mix:" + node.lineno);
    this.putPrint("mix(lhs):" + node.lineno);

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
    this.putPrint("mix(rhs):" + node.lineno);
    node.getRhs().accept(this);

    this.popWrappingComment();
  }

  @Override
  public void visit(ASTRecv node) {
    this.pushWrappingComment("recv(" + node.getChr() + ", " + node.getChi() + "):" + node.lineno);
    this.putPrint("recv(" + node.getChr() + ", " + node.getChi() + "):" + node.lineno);

    // We simply pop the record from the queue and store it in the environment.
    this.putLine(this.sessionPointer(node.getChi()) + " = " + this.popRecord(node.getChr()) + ";");
    this.generateContinuation(node.getRhs());

    this.popWrappingComment();
  }

  @Override
  public void visit(ASTSelect node) {
    this.pushWrappingComment("select" + node.getLabel() + "(" + node.getCh() + "):" + node.lineno);
    this.putPrint("select" + node.getLabel() + "(" + node.getCh() + "):" + node.lineno);

    // Push the label's index onto the session queue.
    this.pushLabel(node.getCh(), node.getLabelIndex(), node.getLabel());
    this.generateContinuation(node.getRhs());

    this.popWrappingComment();
  }

  @Override
  public void visit(ASTSend node) {
    this.pushWrappingComment("send(" + node.getChs() + "):" + node.lineno);
    this.putPrint("send(" + node.getChs() + "):" + node.lineno);

    String sendLhs = this.makeLabel("send_" + node.getChs() + "_" + node.getCho() + "_lhs");
    String sendRhs = this.makeLabel("send_" + node.getChs() + "_" + node.getCho() + "_rhs");

    // We initialize a new session record.
    String choPtr = this.sessionPointer(node.getCho());
    this.initSession(choPtr, environment().getSessionCSize(node.getCho()));
    this.putLine(sessionCont(choPtr) + " = &&" + sendLhs + ";");
    this.putLine(sessionEnv(choPtr) + " = env;");
    this.putLine(sessionIndex(choPtr) + " = " + environment().getIndex(node.getCho()) + ";");

    // We send the session record to the session queue.
    this.pushRecord(node.getChs(), choPtr);

    // We jump to the right hand side, so that we can generate the left hand side code here.
    this.pushWrappingComment("closure(" + node.getCho() + "):" + node.lineno);
    this.putLine("goto " + sendRhs + ";");
    this.putLabel(sendLhs);
    this.environment().setPolarity(node.getCho(), false); // This won't ever be the first end point.
    this.generateContinuation(node.getLhs());
    this.popWrappingComment();

    // In the right hand side, we simply continue as usual.
    this.putLabel(sendRhs);
    this.generateContinuation(node.getRhs());

    this.popWrappingComment();
  }

  @Override
  public void visit(ASTWhy node) {
    this.pushWrappingComment("?(" + node.getCh() + "):" + node.lineno);
    this.putPrint("?(" + node.getCh() + "):" + node.lineno);

    // This node doesn't really do anything - the continuation sent by the bang node will never be
    // popped.
    // Future call nodes will simply peek the continuation from the queue.

    this.generateContinuation(node.getRhs());

    this.popWrappingComment();
  }

  private void generateContinuation(ASTNode node) {
    String ch = node.getSubjectCh();
    if (ch != null) {
      // If we were previously writing to the channel and we're now reading from it, we need to flip
      // to the other side of the session.
      if (this.environment().getPolarity(ch) && !node.isPos()) {
        this.flip(ch);
      }

      // Update polarity for future continuations.
      this.environment().setPolarity(ch, node.isPos());
    }

    node.accept(this);
  }

  private void pushClose(String ch) {
    // The close token is zero-sized, so we don't actually do anything.
  }

  private void popClose(String ch) {
    // The close token is zero-sized, so we don't actually do anything.
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

  private void flip(String ch) {
    this.pushWrappingComment("flip(" + ch + ")");
    putPrint("flip(" + ch + ")");

    String label = makeLabel("flip_" + ch);
    putLine("tmp_env = " + sessionEnv(sessionPointer(ch)) + ";");
    putLine("tmp_cont = " + sessionCont(sessionPointer(ch)) + ";");
    putLine(sessionCont(sessionPointer(ch)) + " = &&" + label + ";");
    putLine(sessionEnv(sessionPointer(ch)) + " = env;");
    putLine(sessionIndex(sessionPointer(ch)) + " = " + environment().getIndex(ch) + ";");
    putLine("env = tmp_env;");
    putLine("goto *tmp_cont;");
    putLabel(label);

    this.popWrappingComment();
  }

  private void finalFlip(String ch) {
    this.pushWrappingComment("finalFlip(" + ch + ")");
    putPrint("finalFlip(" + ch + ")");

    putLine("tmp_session = " + sessionPointer(ch) + ";");
    putLine("env = " + sessionEnv("tmp_session") + ";");
    putLine("goto *" + sessionCont("tmp_session") + ";");

    this.popWrappingComment();
  }

  private void pushScope(ASTNode node) {
    if (this.environments.empty()) {
      Environment env = new Environment();
      env.insertFromNode(node);
      this.environments.push(env);
      this.putLine("env = " + this.allocEnvironment(this.environment().getSize()) + ";");
    } else {
      this.environments.push(this.environment());
    }
  }

  private void popScope() {
    this.environments.pop();
    if (this.environments.empty()) {
      this.putLine("free(env);");
    }
  }

  private String allocEnvironment(int envSize) {
    return this.allocEnvironment(Integer.toString(envSize));
  }

  private String allocEnvironment(String envSize) {
    return "malloc(sizeof(struct environment) + sizeof(struct record*) * (" + envSize + "));";
  }

  private void initSession(String ptr, String size) {
    putLine(ptr + " = malloc(sizeof(struct record) + (" + size + "));");
    putLine(
        sessionWrite(ptr)
            + " = "
            + sessionRead(ptr)
            + " = (unsigned char*)"
            + ptr
            + " + sizeof(struct record);");
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
    return env + "->records[" + ch + " /*" + chName + "*/]";
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

  private void putPrint(String msg) {
    putLine("puts(\"" + escapeString(msg) + "\");");
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
    return string.replace("\\", "\\\\").replace("\"", "\\\"");
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
