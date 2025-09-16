package pt.inescid.cllsj.compiler.anl;

import java.util.Optional;
import pt.inescid.cllsj.compiler.ir.id.IRSessionId;
import pt.inescid.cllsj.compiler.ir.slot.IRSlotTree;

public abstract class AnlSlot {
  public static final AnlSlot UNKNOWN = new Unknown();

  public static int count(IRSlotTree tree) {
    if (tree.isLeaf()) {
      return 0;
    } else if (tree.isUnary()) {
      return 1 + count(((IRSlotTree.Unary) tree).child());
    } else if (tree.isIsValue()) {
      IRSlotTree.IsValue valueTree = (IRSlotTree.IsValue) tree;
      return Math.max(count(valueTree.value()), count(valueTree.notValue()));
    } else if (tree.isTag()) {
      int count = 0;
      for (IRSlotTree child : ((IRSlotTree.Tag) tree).cases()) {
        count = Math.max(count, count(child));
      }
      return 1 + count;
    } else {
      throw new UnsupportedOperationException("Unsupported slot tree type");
    }
  }

  public Optional<Session> assumeSession() {
    return assume(Session.class);
  }

  public Optional<Tag> assumeTag() {
    return assume(Tag.class);
  }

  public <A> Optional<A> assume(Class<A> cls) {
    if (this instanceof Unknown) {
      return Optional.empty();
    } else if (cls.isInstance(this)) {
      return Optional.of(cls.cast(this));
    } else {
      throw new IllegalStateException(
          "Expected a " + cls.getSimpleName() + " or unknown slot, got: " + this);
    }
  }

  public abstract AnlSlot clone();

  public abstract AnlSlot merge(AnlSlot other);

  public abstract void markAsUnknown(Analyzer analyzer, AnlFlowState state);

  public static class Unknown extends AnlSlot {
    @Override
    public String toString() {
      return "?";
    }

    @Override
    public AnlSlot clone() {
      return UNKNOWN;
    }

    @Override
    public AnlSlot merge(AnlSlot other) {
      return UNKNOWN;
    }

    @Override
    public void markAsUnknown(Analyzer analyzer, AnlFlowState state) {
      // Nothing to do, already unknown
    }
  }

  public static class Session extends AnlSlot {
    private IRSessionId sessionId;

    public Session(IRSessionId sessionId) {
      this.sessionId = sessionId;
    }

    public IRSessionId id() {
      return sessionId;
    }

    @Override
    public String toString() {
      return sessionId.toString();
    }

    @Override
    public AnlSlot clone() {
      return new Session(this.sessionId);
    }

    @Override
    public AnlSlot merge(AnlSlot other) {
      if (other instanceof Session) {
        Session o = (Session) other;
        if (o.sessionId.equals(this.sessionId)) {
          return this.clone();
        }
      }
      return UNKNOWN;
    }

    @Override
    public void markAsUnknown(Analyzer analyzer, AnlFlowState state) {
      state.session(sessionId).markAsUnknown(analyzer, state);
    }
  }

  public static class Tag extends AnlSlot {
    private int tag;

    public Tag(int tag) {
      this.tag = tag;
    }

    public int tag() {
      return tag;
    }

    @Override
    public AnlSlot clone() {
      return new Tag(this.tag);
    }

    @Override
    public AnlSlot merge(AnlSlot other) {
      if (other instanceof Tag) {
        Tag o = (Tag) other;
        if (o.tag == this.tag) {
          return this.clone();
        }
      }
      return UNKNOWN;
    }

    @Override
    public void markAsUnknown(Analyzer analyzer, AnlFlowState state) {
      // Nothing to do, tags do not have state
    }

    @Override
    public String toString() {
      return "tag(" + tag + ")";
    }
  }
}
