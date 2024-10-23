package pt.ulisboa.tecnico.cllsj;

public class TypeDefEntry implements EnvEntry {
    ASTTypeDef t;
    
    public TypeDefEntry(ASTTypeDef _t) {
	t = _t;
    }

    public ASTTypeDef getTypeDef(){
	return t;
    }
}
