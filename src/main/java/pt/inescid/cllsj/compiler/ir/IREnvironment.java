package pt.inescid.cllsj.compiler.ir;

import java.util.Optional;
import java.util.function.Consumer;
import pt.inescid.cllsj.Env;
import pt.inescid.cllsj.EnvEntry;
import pt.inescid.cllsj.ast.types.ASTIdT;
import pt.inescid.cllsj.ast.types.ASTNotT;
import pt.inescid.cllsj.ast.types.ASTType;
import pt.inescid.cllsj.compiler.Compiler;
import pt.inescid.cllsj.compiler.ir.id.IRDataLocation;
import pt.inescid.cllsj.compiler.ir.id.IRDropId;
import pt.inescid.cllsj.compiler.ir.id.IRLocalDataId;
import pt.inescid.cllsj.compiler.ir.id.IRSessionId;
import pt.inescid.cllsj.compiler.ir.id.IRTypeId;
import pt.inescid.cllsj.compiler.ir.instruction.IRProcess;
import pt.inescid.cllsj.compiler.ir.slot.IRSlotCombinations;
import pt.inescid.cllsj.compiler.ir.slot.IRSlotOffset;
import pt.inescid.cllsj.compiler.ir.slot.IRSlotTree;

// Maps names to IR ids for types, sessions and local data within a process
public class IREnvironment {
  private Compiler compiler;
  protected IRProcess process;
  private Optional<IREnvironment> parent;
  private Env<EnvEntry> ep;

  public IREnvironment(Compiler compiler, IRProcess process, Env<EnvEntry> ep) {
    this.compiler = compiler;
    this.process = process;
    this.parent = Optional.empty();
    this.ep = ep;
  }

  public IREnvironment(IREnvironment parent, Env<EnvEntry> ep) {
    this.compiler = parent.compiler;
    this.process = parent.process;
    this.parent = Optional.of(parent);
    this.ep = ep;
  }

  public IRProcess getProcess() {
    return process;
  }

  public Env<EnvEntry> getEp() {
    return ep;
  }

  public IREnvironment changeEp(Env<EnvEntry> ep) {
    return new IREnvironment(this, ep);
  }

  public IREnvironment addType(String name, boolean isPositive) {
    return new Type(this, name, Optional.of(process.addType()), isPositive);
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

  public Type getType(IRTypeId id) {
    if (this instanceof Type) {
      Type typeEnv = (Type) this;
      if (typeEnv.hasId() && typeEnv.getId().equals(id)) {
        return typeEnv;
      }
    }

    if (parent.isPresent()) {
      return parent.get().getType(id);
    } else {
      throw new IllegalArgumentException("Type " + id + " not found");
    }
  }

  public IREnvironment addArgSession(String name, IRSlotCombinations combinations) {
    IRSessionId sessionId = process.addSession();
    IRLocalDataId localDataId = process.addLocalData(combinations);
    process.associateLocalData(sessionId, localDataId);
    process.makeSessionArgument(sessionId);
    return new Channel(
        this,
        name,
        Optional.of(sessionId),
        IRSlotOffset.ZERO,
        Optional.of(localDataId),
        Optional.empty());
  }

  public IREnvironment addSession(String name, IRSlotCombinations combinations) {
    IRSessionId sessionId = process.addSession();
    IRLocalDataId localDataId = process.addLocalData(combinations);
    process.associateLocalData(sessionId, localDataId);
    return new Channel(
        this,
        name,
        Optional.of(sessionId),
        IRSlotOffset.ZERO,
        Optional.of(localDataId),
        Optional.empty());
  }

  public IREnvironment addSession(String name) {
    return new Channel(
        this,
        name,
        Optional.of(process.addSession()),
        IRSlotOffset.ZERO,
        Optional.empty(),
        Optional.empty());
  }

  public IREnvironment addValue(
      String name, IRSlotCombinations combinations, Optional<IRSlotTree> exponentialType) {
    return new Channel(
        this,
        name,
        Optional.empty(),
        IRSlotOffset.ZERO,
        Optional.of(process.addLocalData(combinations)),
        exponentialType);
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
        channel.localDataId,
        channel.exponentialType);
  }

  public IREnvironment resetChannel(String name) {
    Channel channel = getChannel(name);
    return new Channel(
        this,
        channel.getName(),
        channel.sessionId,
        IRSlotOffset.ZERO,
        channel.localDataId,
        channel.exponentialType);
  }

  public IREnvironment resetChannelIfNotValue(String name, IRValueRequisites reqs) {
    Channel channel = getChannel(name);
    return new Channel(
        this,
        channel.getName(),
        channel.sessionId,
        IRSlotOffset.of(
            IRSlotTree.isValue(reqs, channel.offset.getPast(), IRSlotTree.LEAF),
            IRSlotTree.isValue(reqs, channel.offset.getFuture(), IRSlotTree.LEAF)),
        channel.localDataId,
        channel.exponentialType);
  }

  public IREnvironment makeChannelExponential(
      String name, IRSlotTree slots, boolean always, Consumer<IRDropId> dropId) {
    Channel channel = getChannel(name);
    IRDropId id =
        process.addDropOnEnd(channel.getLocalDataId(), channel.getOffset(), slots, always);
    dropId.accept(id);
    return new Channel(
        this,
        channel.getName(),
        channel.sessionId,
        channel.getOffset(),
        channel.localDataId,
        Optional.of(slots));
  }

  public IREnvironment alias(String oldName, String newName) {
    Channel channel = getChannel(oldName);
    return new Channel(
        this,
        newName,
        channel.sessionId,
        channel.getOffset(),
        channel.localDataId,
        channel.exponentialType);
  }

  public boolean isPositive(ASTType type) {
    if (type instanceof ASTNotT) {
      type = ((ASTNotT) type).getin();
      return !isPositive(type);
    }

    if (type instanceof ASTIdT) {
      type = type.unfoldTypeCatch(ep);
    }

    if (type instanceof ASTIdT) {
      return getType(((ASTIdT) type).getid()).isPositive();
    } else {
      return type.isPosCatch(ep);
    }
  }

  public static class Type extends IREnvironment {
    private String name;
    private Optional<IRTypeId> id;
    private boolean isPositive;

    public Type(IREnvironment parent, String name, Optional<IRTypeId> id, boolean isPositive) {
      super(parent, parent.ep);
      this.name = name;
      this.id = id;
      this.isPositive = isPositive;
    }

    public String getName() {
      return name;
    }

    public IRTypeId getId() {
      return id.orElseThrow();
    }

    public boolean hasId() {
      return id.isPresent();
    }

    public boolean isPositive() {
      return isPositive;
    }
  }

  public static class Channel extends IREnvironment {
    private String name;
    private Optional<IRSessionId> sessionId;
    private IRSlotOffset offset;
    private Optional<IRLocalDataId> localDataId;
    private Optional<IRSlotTree> exponentialType;

    public Channel(
        IREnvironment parent,
        String name,
        Optional<IRSessionId> sessionId,
        IRSlotOffset offset,
        Optional<IRLocalDataId> localDataId,
        Optional<IRSlotTree> exponentialType) {
      super(parent, parent.ep);
      this.name = name;
      this.sessionId = sessionId;
      this.offset = offset;
      this.localDataId = localDataId;
      this.exponentialType = exponentialType;
    }

    public String getName() {
      return name;
    }

    public IRSessionId getSessionId() {
      return sessionId.orElseThrow();
    }

    public Optional<IRSessionId> getSessionIdMaybe() {
      return sessionId;
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

    public boolean isExponential() {
      return exponentialType.isPresent();
    }

    public IRSlotTree getExponentialType() {
      return exponentialType.orElseThrow();
    }
  }
}
