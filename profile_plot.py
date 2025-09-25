import pandas as pd
import matplotlib.pyplot as plt

df = pd.read_csv("profile.csv")

# Ask for user input to filter data by 'what' column prefix
prefix = input("Enter prefix to filter 'what' column (leave empty for no filter): ").strip()

# Filter dataframe if prefix is provided
# Additionally remove the prefix from the 'what' column for cleaner labels
if prefix:
  df = df[df["what"].str.startswith(prefix)]
  df["what"] = df["what"].str[len(prefix):]
  df["what"] = df["what"].str.lstrip("_- ")

# Ask for user input to filter out data by 'what' column (not prefix)
pattern = input("Enter pattern to filter out 'what' column (leave empty for no filter): ").strip()
if pattern:
  df = df[df["what"].str.contains(pattern) == False]

# Convert wall_time (format mm:ss.xx) into seconds
def parse_time(t):
  if ":" in str(t):
    m, s = t.split(":")
    return float(m) * 60 + float(s)
  return float(t)

df = df.drop(columns=["date"])
df["wall_time"] = df["wall_time"].apply(parse_time)
df = df.groupby(["what", "input"], as_index=False).mean()
df = df.sort_values(by="input")

# Execution Time Plot
plt.figure(figsize=(8,5))
for tag, g in df.groupby("what"):
  plt.plot(g["input"], g["wall_time"], marker="o", label=tag)

plt.title("Execution Time Comparison")
plt.xlabel("Input")
plt.ylabel("Wall Time (seconds)")
plt.legend()
plt.grid(True, linestyle="--", alpha=0.6)
plt.tight_layout()
plt.savefig("bin/execution_time.png")
print(f"Saved bin/execution_time.png")

# Memory Usage Plot
plt.figure(figsize=(8,5))
for tag, g in df.groupby("what"):
  plt.plot(g["input"], g["max_rss"], marker="o", label=tag)

plt.title("Memory Usage Comparison")
plt.xlabel("Input")
plt.ylabel("Max RSS (KB)")
plt.legend()
plt.grid(True, linestyle="--", alpha=0.6)
plt.tight_layout()
plt.savefig("bin/memory_usage.png")
print(f"Saved bin/memory_usage.png")

# Bar plots for each input
for input_val, g in df.sort_values(by="wall_time").groupby("input"):
  plt.figure(figsize=(6,4))
  plt.barh(g["what"], g["wall_time"])
  plt.title(f"Execution time for input {input_val}")
  plt.xlabel("Wall time (s)")
  plt.ylabel("Configuration")
  plt.tight_layout()
  plt.savefig(f"bin/walltime_{input_val}.png")
  plt.close()
  print(f"Saved walltime_{input_val}.png")
