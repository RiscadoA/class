package pt.ulisboa.tecnico.cllsj;

public class TypeEntry implements EnvEntry {
    ASTType t;
    
    public TypeEntry(ASTType _t) {
	t = _t;
    }

    public ASTType getType(){
	return t;
    }
}
