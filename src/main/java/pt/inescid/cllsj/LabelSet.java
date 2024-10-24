package pt.inescid.cllsj;

public class LabelSet extends SessionField {

  String label;

  static String useS = new String("use");
  static String discardS = new String("discard");
  static LabelSet discardTok = new LabelSet(discardS);
  static LabelSet useTok = new LabelSet(useS);

  public LabelSet(String lab) {
    label = lab;
  }

  public void setLabel(String _label) {
    label = _label;
  }

  public String getLabel() {
    return label;
  }

  public String show() {
    return "Label# " + label;
  }
}
