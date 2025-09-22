package pt.inescid.cllsj.compiler.ir.id;

public class IRTypeFlag {
  private int bit;
  private String name;

  public static final IRTypeFlag IS_VALUE = new IRTypeFlag(0, "isValue");
  public static final IRTypeFlag IS_CLONEABLE = new IRTypeFlag(1, "isCloneable");
  public static final IRTypeFlag IS_DROPPABLE = new IRTypeFlag(1, "isDroppable");

  private IRTypeFlag(int bit, String name) {
    this.bit = bit;
    this.name = name;
  }

  public int getBit() {
    return bit;
  }

  @Override
  public String toString() {
    return name;
  }
}
