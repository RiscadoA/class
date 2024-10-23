package pt.ulisboa.tecnico.cllsj;

abstract class Value extends Server {

    
    LinSession call(String _chi) throws Exception{
        LinSession session = new LinSession(_chi);
        Value linValue = this;
        CLLSj.threadPool.submit(
                new Runnable(){
                    public void run(){ try{
                        session.send(linValue);
                    }catch (Exception e) { e.printStackTrace(System.out);} }
                });
        return session;
    }

    
    abstract String toStr();
    abstract boolean equal(Value v);
}
