package pt.inescid.cllsj.compiler.c;

// Adds bit by bit resolution to a CSize
public class CSizeBits {
  private CSize size;
  private int bits;

  public CSizeBits(CSize size, int bits) {
    this.size = size;
    this.bits = bits;
  }

  public static CSizeBits of(CSize size) {
    return new CSizeBits(size, 0);
  }

  public static CSizeBits of(CSize size, int bits) {
    return new CSizeBits(size, bits);
  }

  public CSize getSize() {
    return size;
  }

  public int getBits() {
    return bits;
  }
}
