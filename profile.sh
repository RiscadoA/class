#!/usr/bin/env bash

# Receives at least three arguments: a name, a binary and one or more inputs
# The inputs can be either filenames (ending in .in) or direct input text to be piped to the binary
# The binary is run with each input, and its execution time and memory usage are recorded using GNU time
# The results are appended to profile.csv in the current directory

# The script can be run concurrently as it locks the CSV file when writing

if [[ $# -lt 3 ]]; then
    echo "Usage: $0 name binary input1 [input2 ...]"
    exit 1
fi

what="$1"
binary="$2"
shift 2
inputs=("$@")

if [[ ! -x "$binary" ]]; then
    echo "Error: $binary is not executable"
    exit 1
fi

# Find GNU time (gtime on macOS, time elsewhere)
TIME_CMD=$(command -v gtime || command -v time || true)
if [[ -z "$TIME_CMD" ]]; then
    echo "GNU time not found. On Linux: 'apt install time'. On macOS: 'brew install gnu-time'."
    exit 1
fi

CSV="profile.csv"
# Add header if new file
if [[ ! -f "$CSV" ]]; then
    echo "what,input,user_time,sys_time,wall_time,max_rss,date" > "$CSV"
fi

for input in "${inputs[@]}"; do
    if [[ -f "$input" && "$input" == *.in ]]; then
        inputname=$(basename "$input" .in)
        runout="bin/$what.$inputname.runout"
        profile="bin/$what.$inputname.profile"
        echo "Running $binary with input file $input..."
        "$TIME_CMD" -v "$binary" < "$input" > "$runout" 2> "$profile"
    else
        runout="bin/$what.$input.runout"
        profile="bin/$what.$input.profile"
        echo "Running $binary with input argument $input..."
        echo "$input" | "$TIME_CMD" -v "$binary" > "$runout" 2> "$profile"
    fi

    user_time=$(grep "User time" "$profile" | awk '{print $4}')
    sys_time=$(grep "System time" "$profile" | awk '{print $4}')
    wall_time=$(grep "Elapsed (wall clock)" "$profile" | awk '{print $8}')
    max_rss=$(grep "Maximum resident set size" "$profile" | awk '{print $6}')
    date=$(date +"%Y-%m-%d %H:%M:%S")

    (
        flock -x 200
        echo "$what,$input,$user_time,$sys_time,$wall_time,$max_rss,$date" >> "$CSV"
    ) 200>>"$CSV"
done

echo "Profiling complete. Results saved in $CSV"
