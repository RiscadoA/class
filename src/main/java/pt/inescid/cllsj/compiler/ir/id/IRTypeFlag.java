package pt.inescid.cllsj.compiler.ir.id;

public class IRTypeFlag {
  private int bit;
  private String name;

  public static final IRTypeFlag IS_VALUE = new IRTypeFlag(0, "isValue");

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
