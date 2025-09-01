package pt.inescid.cllsj.compiler.c;

public class CStringEscape {
  public static final String escape(String s) {
    return s.replace("\\", "\\\\")
            .replace("\"", "\\\"")
            .replace("\n", "\\n")
            .replace("\t", "\\t");
  }
}
