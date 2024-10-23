package pt.ulisboa.tecnico.cllsj;

class VBool extends Value{

    boolean val;

    VBool(boolean val){
        this.val = val;
    }

    boolean get(){
        return val;
    }

    String toStr(){
        return Boolean.toString(val);
    }

    boolean equal(Value v) {
	return val == ((VBool)v).get();
    }


}
