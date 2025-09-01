package pt.inescid.cllsj.compiler;

import java.io.PrintStream;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

public class Settings {
  private List<BaseSetting> settings;
  private Map<String, BaseSetting> settingsByName = new HashMap<>();

  public Flag addFlag(
      Optional<Character> shortName, String longName, String description, boolean defaultValue) {
    Flag setting = new Flag(shortName, longName, description, defaultValue);
    addSetting(setting);
    return setting;
  }

  public Int addInt(
      Optional<Character> shortName, String longName, String description, int defaultValue) {
    Int setting = new Int(shortName, longName, description, defaultValue);
    addSetting(setting);
    return setting;
  }

  public Path addPath(
      Optional<Character> shortName, String longName, String description, String defaultValue) {
    Path setting = new Path(shortName, longName, description, defaultValue);
    addSetting(setting);
    return setting;
  }

  public Mode addMode(
      Optional<Character> shortName, String longName, String description, Runnable runnable) {
    Mode setting = new Mode(shortName, longName, description, runnable);
    addSetting(setting);
    return setting;
  }

  public void parse(String args[]) {
    for (int i = 0; i < args.length; ++i) {
      String split[] = args[i].split("=");
      if (split.length > 2) {
        throw new IllegalArgumentException("Invalid argument " + args[i]);
      }

      String name = split[0];
      Optional<String> value = split.length > 1 ? Optional.of(args[1]) : Optional.empty();

      if (!settingsByName.containsKey(name)) {
        throw new IllegalArgumentException("Unknown argument " + name);
      }

      settingsByName.get(name).parse(value);
    }
  }

  public void printHelp(PrintStream stream) {
    stream.println("Usage: CLLSj [OPTS...]");

    for (BaseSetting setting : settings) {
      stream.print("    ");
      if (setting.getShortName().isPresent()) {
        stream.print("-" + setting.getShortName().get());
      }
      stream.print("/--" + setting.getLongName());

      if (setting.getValueDescription().isPresent()) {
        stream.print("=<" + setting.getValueDescription().get() + ">");
      }

      stream.print(": ");
      stream.println(setting.getDescription());
    }
  }

  private void addSetting(BaseSetting setting) {
    String longName = "--" + setting.getLongName();
    if (settingsByName.containsKey(longName)) {
      throw new IllegalArgumentException(
          "There's already a setting with the long name " + longName);
    }

    if (setting.getShortName().isPresent()) {
      String shortName = "-" + setting.getShortName().get();
      if (settingsByName.containsKey(shortName)) {
        throw new IllegalArgumentException(
            "There's already a setting with the short name " + shortName);
      }

      settingsByName.put(shortName, setting);
    }

    settingsByName.put(longName, setting);
    settings.add(setting);
  }

  private abstract static class BaseSetting {
    private Optional<Character> shortName;
    private String longName;
    private String description;

    public BaseSetting(Optional<Character> shortName, String longName, String description) {
      this.shortName = shortName;
      this.longName = longName;
      this.description = description;
    }

    public Optional<Character> getShortName() {
      return shortName;
    }

    public String getLongName() {
      return longName;
    }

    public String getDescription() {
      return description;
    }

    public abstract Optional<String> getValueDescription();

    public abstract void parse(Optional<String> value);
  }

  private abstract static class ValueSetting<T> extends BaseSetting {
    private T value;

    public ValueSetting(
        Optional<Character> shortName, String longName, String description, T defaultValue) {
      super(shortName, longName, description);
      this.value = defaultValue;
    }

    public T get() {
      return value;
    }

    public void set(T value) {
      this.value = value;
    }
  }

  public static class Flag extends ValueSetting<Boolean> {
    public Flag(
        Optional<Character> shortName, String longName, String description, boolean defaultValue) {
      super(shortName, longName, description, defaultValue);
    }

    @Override
    public Optional<String> getValueDescription() {
      return Optional.of("on/off");
    }

    @Override
    public void parse(Optional<String> value) {
      if (value.isEmpty() || value.get().equalsIgnoreCase("on")) {
        set(true);
      } else if (value.get().equalsIgnoreCase("off")) {
        set(false);
      } else {
        throw new IllegalArgumentException("Flags only accept 'on' and 'off' values");
      }
    }
  }

  public static class Int extends ValueSetting<Integer> {
    public Int(
        Optional<Character> shortName, String longName, String description, int defaultValue) {
      super(shortName, longName, description, defaultValue);
    }

    @Override
    public Optional<String> getValueDescription() {
      return Optional.of("int");
    }

    @Override
    public void parse(Optional<String> value) {
      String string =
          value.orElseThrow(
              () ->
                  new IllegalArgumentException(
                      "Expected a value for integer option " + getLongName()));
      set(Integer.parseInt(string));
    }
  }

  public static class Path extends ValueSetting<String> {
    public Path(
        Optional<Character> shortName, String longName, String description, String defaultValue) {
      super(shortName, longName, description, defaultValue);
    }

    @Override
    public Optional<String> getValueDescription() {
      return Optional.of("path");
    }

    @Override
    public void parse(Optional<String> value) {
      set(
          value.orElseThrow(
              () ->
                  new IllegalArgumentException(
                      "Expected a value for path option " + getLongName())));
    }
  }

  public static class Mode extends BaseSetting {
    private Runnable runnable;

    public Mode(
        Optional<Character> shortName, String longName, String description, Runnable runnable) {
      super(shortName, longName, description);
      this.runnable = runnable;
    }

    public void activate() {
      runnable.run();
    }

    @Override
    public Optional<String> getValueDescription() {
      return Optional.empty();
    }

    @Override
    public void parse(Optional<String> value) {
      if (value.isPresent()) {
        throw new IllegalArgumentException(
            "Unexpected value for mode " + getLongName() + "(doesn't accept a value)");
      }
      activate();
    }
  }
}
