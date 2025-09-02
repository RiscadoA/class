package pt.inescid.cllsj.compiler.ir;

import java.util.Optional;
import pt.inescid.cllsj.compiler.ir.id.IRDataLocation;
import pt.inescid.cllsj.compiler.ir.id.IRLocalDataId;
import pt.inescid.cllsj.compiler.ir.id.IRSessionId;
import pt.inescid.cllsj.compiler.ir.id.IRTypeId;
import pt.inescid.cllsj.compiler.ir.instruction.IRProcess;
import pt.inescid.cllsj.compiler.ir.slot.IRExponentialS;
import pt.inescid.cllsj.compiler.ir.slot.IRSlotCombinations;
import pt.inescid.cllsj.compiler.ir.slot.IRSlotSequence;

// Maps names to IR ids for types, sessions and local data within a process
public class IREnvironment {
  protected IRProcess process;
  private Optional<IREnvironment> parent;

  public IREnvironment(IRProcess process) {
    this.process = process;
    this.parent = Optional.empty();
  }

  public IREnvironment(IREnvironment parent) {
    this.process = parent.process;
    this.parent = Optional.of(parent);
  }

  public IREnvironment addType(String name, boolean isPositive) {
    return new Type(this, name, process.addType(), isPositive);
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
    IRLocalDataId localDataId = process.addLocalData(combinations);
    return new Session(this, name, process.addSession(localDataId), IRSlotSequence.EMPTY);
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

  public IREnvironment addExponential(String name) {
    return new Exponential(
        this, name, process.addLocalData(IRSlotCombinations.of(new IRExponentialS())));
  }

  public Exponential getExponential(String name) {
    if (this instanceof Exponential) {
      Exponential expEnv = (Exponential) this;
      if (expEnv.getName().equals(name)) {
        return expEnv;
      }
    }

    if (parent.isPresent()) {
      return parent.get().getExponential(name);
    } else {
      throw new IllegalArgumentException("Exponential " + name + " not found");
    }
  }

  public static class Type extends IREnvironment {
    private String name;
    private IRTypeId id;
    private boolean isPositive;

    public Type(IREnvironment parent, String name, IRTypeId id, boolean isPositive) {
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

    public boolean isPositive() {
      return isPositive;
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

    public IRLocalDataId getLocalDataId() {
      return process.getSessionLocalDataId(id);
    }

    public IRDataLocation getLocalData() {
      return IRDataLocation.local(getLocalDataId(), offset);
    }

    public IRDataLocation getRemoteData() {
      return IRDataLocation.remote(id, offset);
    }
  }

  public static class Exponential extends IREnvironment {
    private String name;
    private IRLocalDataId dataId;

    public Exponential(IREnvironment parent, String name, IRLocalDataId dataId) {
      super(parent);
      this.name = name;
      this.dataId = dataId;
    }

    public String getName() {
      return name;
    }

    public IRLocalDataId getDataId() {
      return dataId;
    }
  }
}
