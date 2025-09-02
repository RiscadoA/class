package pt.inescid.cllsj.compiler.ir;

import java.util.Optional;
import pt.inescid.cllsj.compiler.ir.id.IRSessionId;
import pt.inescid.cllsj.compiler.ir.id.IRTypeId;
import pt.inescid.cllsj.compiler.ir.instruction.IRProcess;
import pt.inescid.cllsj.compiler.ir.slot.IRSlotCombinations;
import pt.inescid.cllsj.compiler.ir.slot.IRSlotSequence;

// Maps names to IR ids for types, sessions and local data within a process
public class IREnvironment {
  private IRProcess process;
  private Optional<IREnvironment> parent;

  public IREnvironment(IRProcess process) {
    this.process = process;
    this.parent = Optional.empty();
  }

  public IREnvironment(IREnvironment parent) {
    this.process = parent.process;
    this.parent = Optional.of(parent);
  }

  public IREnvironment addType(String name) {
    return new Type(this, name, process.addType());
  }

  public Type getType(String name) {
    if (this instanceof Type) {
      Type typeEnv = (Type) this;
      if (typeEnv.getName().equals(name)) {
        return typeEnv;
      }
    }

    if (parent.isPresent()) {
      return parent.get().getType(name);
    } else {
      throw new IllegalArgumentException("Type " + name + " not found");
    }
  }

  public IREnvironment addSession(String name, IRSlotCombinations combinations) {
    return new Session(this, name, process.addSession(combinations), IRSlotSequence.EMPTY);
  }

  public Session getSession(String name) {
    if (this instanceof Session) {
      Session sessionEnv = (Session) this;
      if (sessionEnv.getName().equals(name)) {
        return sessionEnv;
      }
    }

    if (parent.isPresent()) {
      return parent.get().getSession(name);
    } else {
      throw new IllegalArgumentException("Session " + name + " not found");
    }
  }

  public IREnvironment advanceSession(String name, IRSlotSequence offset) {
    Session session = getSession(name);
    return new Session(
        this, session.getName(), session.getId(), session.getOffset().suffix(offset));
  }

  public IREnvironment resetSession(String name) {
    Session session = getSession(name);
    return new Session(this, session.getName(), session.getId(), IRSlotSequence.EMPTY);
  }

  public static class Type extends IREnvironment {
    private String name;
    private IRTypeId id;

    public Type(IREnvironment parent, String name, IRTypeId id) {
      super(parent);
      this.name = name;
      this.id = id;
    }

    public String getName() {
      return name;
    }

    public IRTypeId getId() {
      return id;
    }
  }

  public static class Session extends IREnvironment {
    private String name;
    private IRSessionId id;
    private IRSlotSequence offset;

    public Session(IREnvironment parent, String name, IRSessionId id, IRSlotSequence offset) {
      super(parent);
      this.name = name;
      this.id = id;
      this.offset = offset;
    }

    public String getName() {
      return name;
    }

    public IRSessionId getId() {
      return id;
    }

    public IRSlotSequence getOffset() {
      return offset;
    }
  }
}
