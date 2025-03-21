package pt.inescid.cllsj.compiler.ir.type;

import java.util.List;

import pt.inescid.cllsj.compiler.ir.IRTypeVisitor;

public class IRTag extends IRType {
    private List<IRType> choices;

    public IRTag(List<IRType> choices) {
        this.choices = choices;
    }

    public List<IRType> getChoices() {
        return choices;
    }

    @Override
    public void accept(IRTypeVisitor visitor) {
        visitor.visit(this);
    }

    @Override
    public String toString() {
        String result = "tag";
        for (int i = 0; i < choices.size(); i++) {
            result += " { " + i + ": " + choices.get(i) + " }";
        }
        return result;
    }
}
