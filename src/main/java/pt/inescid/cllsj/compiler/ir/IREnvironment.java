package pt.inescid.cllsj.compiler.ir;

import java.util.Optional;
import pt.inescid.cllsj.Env;
import pt.inescid.cllsj.EnvEntry;
import pt.inescid.cllsj.ast.types.ASTIdT;
import pt.inescid.cllsj.ast.types.ASTNotT;
import pt.inescid.cllsj.ast.types.ASTType;
import pt.inescid.cllsj.compiler.ir.id.IRDataLocation;
import pt.inescid.cllsj.compiler.ir.id.IRLocalDataId;
import pt.inescid.cllsj.compiler.ir.id.IRSessionId;
import pt.inescid.cllsj.compiler.ir.id.IRTypeId;
import pt.inescid.cllsj.compiler.ir.instruction.IRProcess;
import pt.inescid.cllsj.compiler.ir.slot.IRExponentialS;
import pt.inescid.cllsj.compiler.ir.slot.IRSlotCombinations;
import pt.inescid.cllsj.compiler.ir.slot.IRSlotOffset;

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

  public IRProcess getProcess() {
    return process;
  }

  public IREnvironment addType(String name, boolean isPositive, boolean isValue) {
    return new Type(this, name, process.addType(), isPositive, isValue);
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

  public IREnvironment addArgSession(String name, IRSlotCombinations combinations) {
    IRLocalDataId localDataId = process.addLocalData(combinations);
    return new Channel(
        this,
        name,
        Optional.of(process.addArgSession(localDataId)),
        IRSlotOffset.ZERO,
        Optional.of(localDataId));
  }

  public IREnvironment addSession(String name, IRSlotCombinations combinations) {
    IRLocalDataId localDataId = process.addLocalData(combinations);
    return new Channel(
        this, name, Optional.of(process.addSession()), IRSlotOffset.ZERO, Optional.of(localDataId));
  }

  public IREnvironment addSession(String name) {
    return new Channel(this, name, Optional.of(process.addSession()), IRSlotOffset.ZERO, Optional.empty());
  }

  public IREnvironment addValue(String name, IRSlotCombinations combinations) {
    return new Channel(
        this,
        name,
        Optional.empty(),
        IRSlotOffset.ZERO,
        Optional.of(process.addLocalData(combinations)));
  }

  public Channel getChannel(String name) {
    if (this instanceof Channel) {
      Channel sessionEnv = (Channel) this;
      if (sessionEnv.getName().equals(name)) {
        return sessionEnv;
      }
    }

    if (parent.isPresent()) {
      return parent.get().getChannel(name);
    } else {
      throw new IllegalArgumentException("Channel " + name + " not found");
    }
  }

  public IREnvironment advanceChannel(String name, IRSlotOffset offset) {
    Channel channel = getChannel(name);
    return new Channel(
        this,
        channel.getName(),
        channel.sessionId,
        channel.getOffset().advance(offset),
        channel.localDataId);
  }

  public IREnvironment resetChannel(String name) {
    Channel channel = getChannel(name);
    return new Channel(this, channel.getName(), channel.sessionId, IRSlotOffset.ZERO, channel.localDataId);
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

  public boolean isPositive(Env<EnvEntry> ep, ASTType type) {
    boolean dual = false;
    if (type instanceof ASTNotT) {
      type = ((ASTNotT) type).getin();
      dual = true;
    }

    if (type instanceof ASTIdT) {
      try {
        type = type.unfoldType(ep);
      } catch (Exception e) {
        e.printStackTrace(System.err);
        System.exit(1);
      }
    }

    if (type instanceof ASTIdT) {
      return dual ^ getType(((ASTIdT) type).getid()).isPositive();
    } else {
      return dual ^ type.getPolarityForCompilerCatch(ep).get();
    }
  }

  public static class Type extends IREnvironment {
    private String name;
    private IRTypeId id;
    private boolean isPositive;
    private boolean isValue;

    public Type(
        IREnvironment parent, String name, IRTypeId id, boolean isPositive, boolean isValue) {
      super(parent);
      this.name = name;
      this.id = id;
      this.isPositive = isPositive;
      this.isValue = isValue;
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

    public boolean isValue() {
      return isValue;
    }
  }

  public static class Channel extends IREnvironment {
    private String name;
    private Optional<IRSessionId> sessionId;
    private IRSlotOffset offset;
    private Optional<IRLocalDataId> localDataId;

    public Channel(
        IREnvironment parent,
        String name,
        Optional<IRSessionId> sessionId,
        IRSlotOffset offset,
        Optional<IRLocalDataId> localDataId) {
      super(parent);
      this.name = name;
      this.sessionId = sessionId;
      this.offset = offset;
      this.localDataId = localDataId;
    }

    public String getName() {
      return name;
    }

    public IRSessionId getSessionId() {
      return sessionId.orElseThrow();
    }

    public IRSlotOffset getOffset() {
      return offset;
    }

    public IRLocalDataId getLocalDataId() {
      return localDataId.orElseThrow();
    }

    public IRDataLocation getLocalData() {
      return IRDataLocation.local(getLocalDataId(), offset);
    }

    public IRDataLocation getRemoteData() {
      return IRDataLocation.remote(getSessionId(), offset);
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
