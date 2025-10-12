#!/usr/bin/env bash

# Runs all tests in the array defined below, with each combination of flags defined below
# Each test is compiled and then profiled with each input using profile.sh
# The results are stored in profile.csv

# Takes as input a repetition count for each test (default 1)
repetitions=${1:-1}

tests=(
  # "queue|examples/LFCS/xqueue-n.clls|500000"
  # "queue|examples/LFCS/xqueue-n.clls|100000 200000 300000 400000 500000"
  # "merge-sort|tests/complex/merge_sort.clls|profiling/merge_sort_1000000.in"
  # "sieve|examples/sam/sam-sieve-sum-fast-stop-intp.clls|10 100 1000 2000 3000 4000 5000"
  "sieve|examples/sam/sam-sieve-sum-fast-stop-intp.clls|10000 20000 30000 40000 50000"
  # "gol|tests/complex/game_of_life_2.clls|profiling/game_of_life_17x17_1000.in"
  # "gol|tests/complex/game_of_life_2.clls|profiling/game_of_life_17x17_1000.in profiling/game_of_life_17x17_2000.in profiling/game_of_life_17x17_3000.in profiling/game_of_life_17x17_4000.in profiling/game_of_life_17x17_5000.in"
)

flag_combinations=(
  # "compiler-base|-O0"
  "compiler-inlining|-O0 --inlining-threshold=16 --monomorphization"
  "compiler-values|-O0 --optimize-affine-value --optimize-send-value"
  "compiler-endpoints|-O0 --analyze-ir --optimize-tail-calls --optimize-single-endpoint --optimize-known-endpoints"
  "compiler-control-flow|-O0 --analyze-ir --optimize-known-jumps --optimize-always-drop"
  "compiler-locations|-O0 --analyze-ir --optimize-known-locations"
  # "compiler-optimized|"
  "compiler-no-inlining|--inlining-threshold=0 --monomorphization=off"
  "compiler-no-values|--optimize-send-value=off"
  "compiler-no-endpoints|--optimize-tail-calls=off --optimize-single-endpoint=off --optimize-known-endpoints=off"
  "compiler-no-control-flow|--optimize-known-jumps=off --optimize-always-drop=off"
  "compiler-no-locations|--optimize-known-locations=off"
)

mkdir -p bin

for test in "${tests[@]}"; do
  test_name="${test%%|*}"
  source="${test#*|}"
  source="${source%%|*}"
  inputs="${test##*|}"

  for combination in "${flag_combinations[@]}"; do
    combination_name="${combination%%|*}"
    flags="${combination#*|}"

    what="${test_name}-${combination_name}"
    binary="bin/$what"
    CLLS_FLAGS="$flags -e=profile --no-custom-allocator" ./compile.sh -O -o "$binary" "$source" &> "bin/$what.compile.log"
    if [[ $? -ne 0 ]]; then
      echo "Compilation of $source with flags '$flags' failed! See bin/$what.compile.log"
      continue
    fi

    for i in $(seq 1 $repetitions); do
      ./profile.sh "$what" "$binary" $inputs
    done
  done
done
