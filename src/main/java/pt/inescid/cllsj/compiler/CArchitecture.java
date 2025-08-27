package pt.inescid.cllsj.compiler;

import java.util.List;

public class CArchitecture {
  public CSize recordHeaderSize = CSize.constant(24);

  public CSize intSize = CSize.constant(4);
  public CAlignment intAlignment = CAlignment.constant(4);

  public CSize unsignedCharSize = CSize.constant(1);
  public CAlignment unsignedCharAlignment = CAlignment.constant(1);

  public CSize pointerSize = CSize.constant(8);
  public CAlignment pointerAlignment = CAlignment.constant(8);

  public static class Test {
    public String cType;
    public CSize expected;

    public Test(String cType, CSize expected) {
      this.cType = cType;
      this.expected = expected;
    }
  }

  // Generates a list of tests to ensure the target architecture matches the architecture we're
  // running in
  public List<Test> getTests() {
    return List.of(
        new Test("struct record_header", recordHeaderSize),
        new Test("int", intSize),
        new Test("unsigned char", unsignedCharSize),
        new Test("void*", pointerSize));
  }
}
