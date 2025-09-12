package pt.inescid.cllsj.compiler.c;

import pt.inescid.cllsj.compiler.ir.slot.IRBoolS;
import pt.inescid.cllsj.compiler.ir.slot.IRCellS;
import pt.inescid.cllsj.compiler.ir.slot.IRExponentialS;
import pt.inescid.cllsj.compiler.ir.slot.IRIntS;
import pt.inescid.cllsj.compiler.ir.slot.IRSessionS;
import pt.inescid.cllsj.compiler.ir.slot.IRSlot;
import pt.inescid.cllsj.compiler.ir.slot.IRSlotVisitor;
import pt.inescid.cllsj.compiler.ir.slot.IRStringS;
import pt.inescid.cllsj.compiler.ir.slot.IRTagS;
import pt.inescid.cllsj.compiler.ir.slot.IRTypeS;
import pt.inescid.cllsj.compiler.ir.slot.IRVarS;

public class CPrintGenerator extends IRSlotVisitor {
  public String function;
  public String formatString;
  public String argument;
  private String value;

  public static CPrintGenerator forValue(String value, IRSlot type) {
    CPrintGenerator gen = new CPrintGenerator();
    gen.value = value;
    type.accept(gen);
    return gen;
  }

  @Override
  public void visit(IRIntS slot) {
    function = "printf";
    formatString = "%d";
    argument = value;
  }

  @Override
  public void visit(IRBoolS slot) {
    function = "printf";
    formatString = "%s";
    argument = "(" + value + " ? \"true\" : \"false\")";
  }

  @Override
  public void visit(IRStringS slot) {
    function = "string_print";
    formatString = "%s";
    argument = value;
  }

  @Override
  public void visit(IRTagS slot) {
    throw new IllegalArgumentException("Cannot print tag values");
  }

  @Override
  public void visit(IRVarS slot) {
    throw new IllegalArgumentException("Cannot print polymorphic sessions");
  }

  @Override
  public void visit(IRTypeS slot) {
    throw new IllegalArgumentException("Cannot print types");
  }

  @Override
  public void visit(IRSessionS slot) {
    throw new IllegalArgumentException("Cannot print sessions");
  }

  @Override
  public void visit(IRExponentialS slot) {
    throw new IllegalArgumentException("Cannot print exponential values");
  }

  @Override
  public void visit(IRCellS slot) {
    throw new IllegalArgumentException("Cannot print cells");
  }
}
