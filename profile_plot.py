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

# Ask for user input on input range to include (min and max)
min_input = input("Enter minimum input value to include (leave empty for no minimum): ").strip()
max_input = input("Enter maximum input value to include (leave empty for no maximum): ").strip()
if min_input:
  df = df[df["input"] >= float(min_input)]
if max_input:
  df = df[df["input"] <= float(max_input)]

# Ask for user input on whether to use logarithmic scale for the y axis
log_scale = input("Use logarithmic scale for the y axis? (y/n, default n): ").strip().lower() == 'y'

# Sum user_time and sys_time into exec_time
df = df.assign(exec_time=df["user_time"] + df["sys_time"])

# Drop unused columns, group by 'what' and 'input', and average the results
df = df.drop(columns=["date", "wall_time", "user_time", "sys_time"])
df = df.groupby(["what", "input"], as_index=False).mean()
df = df.sort_values(by="input")

# Remove rows with exec_time <= 0
df = df[df["exec_time"] > 0]

# Execution Time Plot
plt.figure(figsize=(8,5))
for tag, g in df.groupby("what"):
  plt.plot(g["input"], g["exec_time"], marker="o", label=tag)

if log_scale:
  plt.yscale("log")
plt.title("Execution Time Comparison")
plt.xlabel("Input")
if log_scale:
  plt.ylabel("Execution Time (seconds, log scale)")
else:
  plt.ylabel("Execution Time (seconds)")
plt.legend()
plt.grid(True, linestyle="--", alpha=0.6)
plt.tight_layout()
plt.savefig("bin/execution_time.png")
print(f"Saved bin/execution_time.png")

# Memory Usage Plot
plt.figure(figsize=(8,5))
for tag, g in df.groupby("what"):
  plt.plot(g["input"], g["max_rss"], marker="o", label=tag)

if log_scale:
  plt.yscale("log")
plt.title("Memory Usage Comparison")
plt.xlabel("Input")
if log_scale:
  plt.ylabel("Max RSS (KB, log scale)")
else:
  plt.ylabel("Max RSS (KB)")
plt.legend()
plt.grid(True, linestyle="--", alpha=0.6)
plt.tight_layout()
plt.savefig("bin/memory_usage.png")
print(f"Saved bin/memory_usage.png")

# Bar plots for each input
for input_val, g in df.sort_values(by="exec_time").groupby("input"):
  plt.figure(figsize=(6,4))
  plt.barh(g["what"], g["exec_time"])
  plt.title(f"Execution time for input {input_val}")
  plt.xlabel("Execution time (s)")
  plt.ylabel("Configuration")
  plt.tight_layout()
  plt.savefig(f"bin/exectime_{input_val}.png")
  plt.close()
  print(f"Saved exectime_{input_val}.png")
