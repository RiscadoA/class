#!/usr/bin/env bash

# Runs all tests in the array defined below, with each combination of flags defined below
# Each test is compiled and then profiled with each input using profile.sh
# The results are stored in profile.csv

tests=(
  # "merge-sort|tests/complex/merge_sort.clls|profiling/merge_sort_1000000.in"
  "sieve|examples/sam/sam-sieve-sum-fast-stop-intp.clls|100000"
  # "sieve|examples/sam/sam-sieve-sum-fast-stop-intp.clls|10000 20000 30000 40000 50000 60000 70000 80000 90000 100000"
)

flag_combinations=(
  "baseline|-O0"
  "inlining|-O0 --inlining-threshold=8 --monomorphization"
  "values|-O0 --optimize-send-value --optimize-affine-value"
  "endpoints|-O0 --analyze-ir --optimize-tail-calls --optimize-single-endpoint --optimize-known-endpoints"
  "control-flow|-O0 --analyze-ir --optimize-known-jumps --optimize-always-drop"
  "locations|-O0 --analyze-ir --optimize-known-locations"
  "all|"
  "all-sequential|-Os"
  # "no-inlining|--inlining-threshold=0 --monomorphization=off"
  # "no-values|--optimize-send-value=off --optimize-affine-value=off"
  # "no-endpoints|--optimize-tail-calls=off --optimize-single-endpoint=off --optimize-known-endpoints=off"
  # "no-control-flow|--optimize-known-jumps=off --optimize-always-drop=off"
  # "no-locations|--optimize-known-locations=off"
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
    CLLS_FLAGS="$flags -e=profile" ./compile.sh -O -o "$binary" "$source" &> "bin/$what.compile.log"
    if [[ $? -ne 0 ]]; then
      echo "Compilation of $source with flags '$flags' failed! See bin/$what.compile.log"
      continue
    fi

    ./profile.sh "$what" "$binary" $inputs
  done
done

wait
