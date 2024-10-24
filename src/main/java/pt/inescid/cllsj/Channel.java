package pt.inescid.cllsj;

public interface Channel {

  String getId();

  void send(Object _message) throws Exception;

  Object receive() throws Exception;

  void send(String lab, Object _message) throws Exception;

  Object receive(String _lab) throws Exception;
}
