package pt.inescid.cllsj.compiler.ir;

import java.util.Map;

public class IRProcess {
    private Map<String, IRBlock> blocks;

    public IRProcess(Map<String, IRBlock> blocks) {
        this.blocks = blocks;
    }

    public Map<String, IRBlock> getBlocks() {
        return blocks;
    }

    @Override
    public String toString() {
        String result = "";
        for (Map.Entry<String, IRBlock> entry : blocks.entrySet()) {
            result += entry.getKey() + ":\n";
            result += entry.getValue().toString().indent(4);
        }
        return result;
    }
}
