package pt.inescid.cllsj.compiler;

import java.util.Stack;
import pt.inescid.cllsj.ast.ASTNodeVisitor;
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
    generator.putLine("struct record** env;");
    generator.putLine("unsigned char* read;");
    generator.putLine("unsigned char* write;");
    generator.putLine("int index;");
    generator.indentLevel--;
    generator.putLine("};");
    generator.putLine("");
    generator.putLine("struct task {");
    generator.indentLevel++;
    generator.putLine("void* cont;");
    generator.putLine("struct record** env;");
    generator.putLine("struct task* next;");
    generator.indentLevel--;
    generator.putLine("};");
    generator.putLine("");
    generator.putLine("int main() {");
    generator.indentLevel++;
    generator.putLine("struct record* tmp_session;");
    generator.putLine("struct record** env = NULL;");
    generator.putLine("struct record** tmp_env;");
    generator.putLine("void* tmp_cont;");
    generator.putLine("struct task* next_task = NULL;");
    generator.putLine("struct task* tmp_task;");
    generator.putLine("");
    ast.accept(generator);

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
    this.flip(node.getCh());

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
    this.initSession(node.getCh());
    this.putLine(sessionCont(node.getCh()) + " = &&" + cutRhs + ";");
    this.putLine(sessionEnv(node.getCh()) + " = env;");
    this.putLine(sessionIndex(node.getCh()) + " = " + environment().getIndex(node.getCh()) + ";");

    ASTNode positive, negative;
    if (node.getChType().isPos()) {
      positive = node.getRhs();
      negative = node.getLhs();
    } else {
      positive = node.getLhs();
      negative = node.getRhs();
    }

    // The first code to be executed for a given session must be positive.
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

    putLine("if (next_task != NULL) {");
    indentLevel++;
    this.putPrint("empty(continue):" + node.lineno);
    putLine("tmp_cont = next_task->cont;");
    putLine("env = next_task->env;");
    putLine("next_task = next_task->next;");
    putLine("free(next_task);");
    putLine("goto *tmp_cont;");
    indentLevel--;
    putLine("}");
    this.putPrint("empty(done):" + node.lineno);
    putLine("return 0;");

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

    if (this.environment().getPolarity(negative)) {
      // We've previously written to the negative channel.
      // Thus, the negative channel may have data on its buffer that it still hasn't read.
      // We pass control to the negative channel, so that it can read that data that is already in
      // the buffer.

      this.putLine("tmp_env = " + sessionEnv(negative) + ";");
      this.putLine("tmp_cont = " + sessionCont(negative) + ";");

      if (this.environment().getPolarity(positive)) {
        this.putPrint("fwd(W " + negative + ", W " + positive + "):" + node.lineno);

        // We've previously written to the positive channel.
        // Thus, the positive channel might have data on its buffer that it still hasn't read.
        //
        // In this case, we should append what we've already written in the positive channel to
        // the negative channel's buffer - basically pretending that it was written by the negative
        // channel.

        this.beginLine("memcpy(" + sessionWrite(negative) + ", ");
        this.continueLine(sessionRead(positive) + ", ");
        this.endLine(sessionWrite(positive) + " - " + sessionRead(positive) + ");");

        this.beginLine(sessionWrite(negative) + " += ");
        this.endLine(sessionWrite(positive) + " - " + sessionRead(positive) + ";");
      } else {
        this.putPrint("fwd(W " + negative + ", R " + positive + "):" + node.lineno);

        // We've previously read from the positive channel.
        // Thus, since we're now writing to it, we must have already read all the data sent by it.
        // So, we can ignore the contents of the positive channel's buffer, and do nothing.
      }

      // We need to set the continuation of the negative channel to the continuation of the positive
      // channel.
      this.putLine(sessionEnv(negative) + " = " + sessionEnv(positive) + ";");
      this.putLine(sessionCont(negative) + " = " + sessionCont(positive) + ";");

      // We make the positive channel bindings point to the negative channel, and terminate the old
      // positive channel.
      this.putLine("tmp_session = " + sessionPointer(positive) + ";");
      this.putLine(
          sessionEnv(positive)
              + "["
              + sessionIndex(positive)
              + "] = "
              + sessionPointer(negative)
              + ";");
      this.putLine("free(tmp_session);");
    } else {
      // We've previously read from the negative channel.
      // Thus, data meant for the positive channel will already have been pushed to the negative
      // channel's buffer.
      // We pass control to the positive channel, so that it can read that data that is already in
      // the buffer.

      this.putLine("tmp_env = " + sessionEnv(positive) + ";");
      this.putLine("tmp_cont = " + sessionCont(positive) + ";");

      if (this.environment().getPolarity(positive)) {
        this.putPrint("fwd(R " + negative + ", W " + positive + "):" + node.lineno);

        // We've previously written to the positive channel.
        // Thus, the positive channel might have data on its buffer that it still hasn't read.
        // In this case, we should append the extra data from the negative channel's buffer to the
        // positive channel's buffer.

        this.beginLine("memcpy(" + sessionWrite(positive) + ", ");
        this.continueLine(sessionRead(negative) + ", ");
        this.endLine(sessionWrite(negative) + " - " + sessionRead(negative) + ");");

        this.beginLine(sessionWrite(positive) + " += ");
        this.endLine(sessionWrite(negative) + " - " + sessionRead(negative) + ";");

        // We need to set the continuation of the positive channel to the continuation of the
        // negative channel.
        this.putLine(sessionEnv(positive) + " = " + sessionEnv(negative) + ";");
        this.putLine(sessionCont(positive) + " = " + sessionCont(negative) + ";");

        // We make the negative channel bindings point to the positive channel, and terminate the
        // old negative channel.
        this.putLine("tmp_session = " + sessionPointer(negative) + ";");
        this.putLine(
            sessionEnv(negative)
                + "["
                + sessionIndex(negative)
                + "] = "
                + sessionPointer(positive)
                + ";");
        this.putLine("free(tmp_session);");
      } else {
        this.putPrint("fwd(R " + negative + ", R " + positive + "):" + node.lineno);

        // We've previously read from the positive channel.
        // Thus, since we're now writing to it, we must have already read all the data sent by it.
        // So, we can ignore the contents of the positive channel's buffer.

        // We make the positive channel bindings point to the negative channel, and terminate the
        // old positive channel.
        this.putLine("tmp_session = " + sessionPointer(positive) + ";");
        this.putLine(
            sessionEnv(positive)
                + "["
                + sessionIndex(positive)
                + "] = "
                + sessionPointer(negative)
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

    // We need a new, fresh environment for the send left hand side.
    Environment env = Environment.fromNode(node.getLhs());
    env.insert(node.getCho(), node.getLhsType());

    // We initialize the new environment and record.
    this.environments.push(env);
    this.putLine("tmp_env = env;");
    this.putLine("env = malloc(sizeof(struct record*) * (" + env.getSize() + "));");
    this.initSession(node.getCho());
    this.putLine(sessionEnv(node.getCho()) + " = env;");
    this.putLine(sessionCont(node.getCho()) + " = &&" + sendLhs + ";");
    this.putLine(sessionIndex(node.getCho()) + " = " + env.getIndex(node.getCho()) + ";");
    this.putLine("tmp_session = " + sessionPointer(node.getCho()) + ";");
    this.putLine("env = tmp_env;");
    this.environments.pop();

    // We send the session record to the session queue.
    this.pushRecord(node.getChs(), "tmp_session");

    // We jump to the right hand side, so that we can generate the left hand side code here.
    this.pushWrappingComment("closure(" + node.getCho() + "):" + node.lineno);
    this.putLine("goto " + sendRhs + ";");
    this.putLabel(sendLhs);
    this.environments.push(env);
    env.setPolarity(node.getCho(), false); // This won't ever be the first end point.
    this.generateContinuation(node.getLhs());
    this.environments.pop();
    this.popWrappingComment();

    // In the right hand side, we simply continue as usual.
    this.putLabel(sendRhs);
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

  // Creates a new statement which pushes a continuation onto the session queue.
  private void pushContinuation(String ch, String cont) {
    pushLiteral(ch, "void*", "&&" + cont);
  }

  // Creates a new expression which pops a continuation from the session queue.
  private String popContinuation(String ch) {
    return popLiteral(ch, "void*");
  }

  // Creates statements which pushes a literal onto the session queue.
  private void pushLiteral(String ch, String cType, String literal) {
    putLine("*(" + cType + "*)" + sessionWrite(ch) + " = " + literal + ";");
    putLine(sessionWrite(ch) + " += sizeof(" + cType + ");");
  }

  // Returns a new expression which pops a literal from the session queue.
  private String popLiteral(String ch, String cType) {
    return "*(("
        + cType
        + "*)(("
        + sessionRead(ch)
        + " += sizeof("
        + cType
        + ")) - sizeof("
        + cType
        + ")))";
  }

  private void flip(String ch) {
    this.pushWrappingComment("flip(" + ch + ")");
    ;
    putPrint("flip(" + ch + ")");

    String label = makeLabel("flip_" + ch);
    putLine("tmp_env = " + sessionEnv(ch) + ";");
    putLine("tmp_cont = " + sessionCont(ch) + ";");
    putLine(sessionCont(ch) + " = &&" + label + ";");
    putLine(sessionEnv(ch) + " = env;");
    putLine(sessionIndex(ch) + " = " + environment().getIndex(ch) + ";");
    putLine("env = tmp_env;");
    putLine("goto *tmp_cont;");
    putLabel(label);
    this.popWrappingComment();
  }

  private void pushScope(ASTNode node) {
    if (this.environments.empty()) {
      this.environments.push(Environment.fromNode(node));
      this.putLine("env = malloc(sizeof(struct record*) * " + this.environment().getSize() + ");");
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

  private void initSession(String ch) {
    putLine(
        sessionPointer(ch)
            + " = malloc(sizeof(struct record) + ("
            + environment().getSessionCSize(ch)
            + "));");
    putLine(
        sessionWrite(ch)
            + " = "
            + sessionRead(ch)
            + " = (unsigned char*)"
            + sessionPointer(ch)
            + " + sizeof(struct record);");
  }

  private String sessionPointer(String ch) {
    return "env[" + environment().getIndex(ch) + " /*" + ch + "*/]";
  }

  private String sessionCont(String ch) {
    return sessionPointer(ch) + "->cont";
  }

  private String sessionEnv(String ch) {
    return sessionPointer(ch) + "->env";
  }

  private String sessionRead(String ch) {
    return sessionPointer(ch) + "->read";
  }

  private String sessionWrite(String ch) {
    return sessionPointer(ch) + "->write";
  }

  private String sessionIndex(String ch) {
    return sessionPointer(ch) + "->index";
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
