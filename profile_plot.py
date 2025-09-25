import pandas as pd
import matplotlib.pyplot as plt

df = pd.read_csv("profile.csv")

# Convert wall_time (format mm:ss.xx) into seconds
def parse_time(t):
  if ":" in str(t):
    m, s = t.split(":")
    return float(m) * 60 + float(s)
  return float(t)

df["wall_time_sec"] = df["wall_time"].apply(parse_time)

# Execution Time Plot
plt.figure(figsize=(8,5))
for tag, g in df.groupby("what"):
  plt.plot(g["input"], g["wall_time_sec"], marker="o", label=tag)

plt.title("Execution Time Comparison")
plt.xlabel("Input")
plt.ylabel("Wall Time (seconds)")
plt.legend()
plt.grid(True, linestyle="--", alpha=0.6)
plt.tight_layout()
plt.savefig("bin/execution_time.png")

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

# Bar plots for each input
for input_val, g in df.groupby("input"):
  plt.figure(figsize=(6,4))
  plt.bar(g["what"], g["wall_time_sec"])
  plt.title(f"Execution time for input {input_val}")
  plt.ylabel("Wall time (s)")
  plt.xticks(rotation=45)
  plt.tight_layout()
  plt.savefig(f"bin/walltime_{input_val}.png")
  plt.close()
