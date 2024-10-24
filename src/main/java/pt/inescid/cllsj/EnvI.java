package pt.inescid.cllsj;

public interface EnvI<X> {
  public EnvI<X> assoc(String id, X assoc);

  public void insert(String _id, X _assoc);

  public Env<X> shallowDup();

  public Env<X> dup();

  public Env<X> dupe();

  public boolean eq(Env<X> e);

  public String findI(SessionRecord _it) throws Exception;

  public X find(String _id) throws Exception;

  public boolean def(String _id) throws Exception;

  public void upd(String _id, X _assoc) throws Exception;

  public void updmove(String _id) throws Exception;

  public void updmove(Env<X> env2) throws Exception;

  public void crawl();
}
