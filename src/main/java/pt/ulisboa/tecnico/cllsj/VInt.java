package pt.ulisboa.tecnico.cllsj;

class VInt extends Value {

    int val;

    VInt(int val){
        this.val = val;
    }

    int get(){
        return val;
    }
    String toStr(){
        return Integer.toString(val);
    }

    boolean equal(Value v) {
	return val == ((VInt)v).get();
    }

}
