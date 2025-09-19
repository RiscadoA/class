package pt.inescid.cllsj.compiler.c;

import java.util.List;

public class CArchitecture {
  public CSize recordHeaderSize = CSize.constant(24);

  public CSize intSize = CSize.constant(4);
  public CAlignment intAlignment = CAlignment.constant(4);

  public CSize unsignedCharSize = CSize.constant(1);
  public CAlignment unsignedCharAlignment = CAlignment.constant(1);

  public CSize pointerSize = CSize.constant(8);
  public CAlignment pointerAlignment = CAlignment.constant(8);

  public CSize sessionSize() {
    return pointerSize.multiply(4);
  }

  public CAlignment sessionAlignment() {
    return pointerAlignment;
  }

  public CSize exponentialSize(CSize envSize) {
    return pointerSize.multiply(3).add(envSize);
  }

  public CSize cellDataOffset(boolean withMutex) {
    CSize size = withMutex ? CSize.sizeOf("sem_t") : CSize.zero();
    size = size.align(intAlignment);
    size = size.add(intSize); // ref count
    size = size.align(pointerAlignment); // assume data is pointer aligned
    return size;
  }

  public CSize cellSize(boolean withMutex, CSize dataSize) {
    return cellDataOffset(withMutex).add(dataSize);
  }

  public CSize typeSize() {
    return pointerSize.add(intSize.multiply(2));
  }

  public CAlignment typeAlignment() {
    return pointerAlignment;
  }

  public CSize typeNodeSize() {
    return intSize.multiply(2);
  }

  public CAlignment typeNodeAlignment() {
    return intAlignment;
  }

  public static class Test {
    public String cType;
    public CSize expectedSize;
    public CAlignment expectedAlignment;

    public Test(String cType, CSize expectedSize, CAlignment expectedAlignment) {
      this.cType = cType;
      this.expectedSize = expectedSize;
      this.expectedAlignment = expectedAlignment;
    }
  }

  // Generates a list of tests to ensure the target architecture matches the architecture we're
  // running in
  public List<Test> getTests() {
    return List.of(
        new Test("int", intSize, intAlignment),
        new Test("unsigned char", unsignedCharSize, unsignedCharAlignment),
        new Test("void*", pointerSize, pointerAlignment),
        new Test("struct session", sessionSize(), sessionAlignment()),
        new Test("struct type", typeSize(), typeAlignment()),
        new Test("struct type_node", typeNodeSize(), typeNodeAlignment()),
        new Test("struct exponential", exponentialSize(CSize.zero()), pointerAlignment));
  }
}
