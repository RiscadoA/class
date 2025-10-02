#!/usr/bin/env bash

MAX_JOBS=6

function success {
    if [ -t 1 ]; then
        echo -e "\e[32m$1\e[0m"
    else
        echo $1
    fi
}

function error {
    if [ -t 1 ]; then
        echo -e "\e[31m$1\e[0m"
    else
        echo $1
    fi
}

# Find all *.clls files in the tests directory, recursively
files=$(find tests -name "*.clls")

# Filter files by regex, if any
if [ ! -z $1 ]; then
    files=$(echo $files | tr ' ' '\n' | grep $1)
fi
count=$(echo $files | wc -w)
if [ $count -eq 0 ]; then
    echo "No test files found"
elif [ $count -eq 1 ]; then
    echo "Found 1 test file"
else
    echo "Found $count test files"
fi

export CLLS_FLAGS="$CLLS_FLAGS --profiling"
echo "Using flags: $CLLS_FLAGS"

success=0
failed=0
processed=0
job_count=0
pids=()
for file in $files
do
    processed=$((processed + 1))
    job_processed=$processed

    # Run the test in background
    (
        # Check if there are accompanying .trace, .in, .out or .err files
        basename=$(basename $file .clls)
        infile=$(dirname $file)/$basename.in
        outfile=$(dirname $file)/$basename.out
        errfile=$(dirname $file)/$basename.err

        baseout=bin/$(dirname $file)/$basename
        mkdir -p $(dirname $baseout)

        echo "($job_processed/$count) $file: compiling... "

        # Compile the file
        ./compile.sh -o $baseout $file &> $baseout.err
        if [ $? -ne 0 ]; then
            error "($job_processed/$count) $file: compilation failed! See $baseout.err"
            failed=$((failed + 1))
            exit 1
        fi
        success "($job_processed/$count) $file: compiled successfully!"

        shopt -s nullglob
        just_failed=0
        infiles=($infile.*)
        if [ ${#infiles[@]} -eq 0 ]; then
            infiles=($infile)
        fi
        echo "($job_processed/$count) $file: running..."
        for file2 in "${infiles[@]}"; do
            suffix=${file2#$infile}

            # Run the compiled file with the input file, if it exists
            if [ -f $file2 ]; then
                $baseout > $baseout.out$suffix 2> $baseout.err$suffix < $file2
            else
                $baseout > $baseout.out$suffix 2> $baseout.err$suffix
            fi
            if [ $? -ne 0 ]; then
                error "($job_processed/$count) $file: execution failed! See $baseout.out$suffix and $baseout.err$suffix"
                just_failed=1
                break
            fi

            # Compare the standard output with the expected
            if [ -f $outfile$suffix ]; then
                diff $baseout.out$suffix $outfile$suffix > /dev/null 2>&1
                if [ $? -ne 0 ]; then
                    error "($job_processed/$count) $file: expected $outfile$suffix, got $baseout.out$suffix"
                    just_failed=1
                    break
                fi
            fi

            # Compare the error output with the expected
            if [ -f $errfile$suffix ]; then
                diff $baseout.err$suffix $errfile$suffix > /dev/null 2>&1
                if [ $? -ne 0 ]; then
                    error "($job_processed/$count) $file: expected $errfile$suffix, got $baseout.err$suffix"
                    just_failed=1
                    break
                fi
            fi

            if [[ $suffix != "" ]]; then
                success "($job_processed/$count) $file: passed $suffix"
            else
                success "($job_processed/$count) $file: passed"
            fi
        done
        if [ $just_failed -ne 0 ]; then
            exit 1
        else
            success "($job_processed/$count) $file: passed"
            exit 0
        fi
    ) &
    pids+=("$!")
    job_count=$((job_count + 1))
    if [ $job_count -ge $MAX_JOBS ]; then
        wait -n
        job_count=$((job_count - 1))
    fi
done
# Wait for any remaining jobs
for pid in "${pids[@]}"; do
    wait $pid
    if [ $? -ne 0 ]; then
        failed=$((failed + 1))
    else
        success=$((success + 1))
    fi
done

if [ $failed -eq 0 ]; then
    success "All tests passed!"
else
    error "Tests failed: $failed/$count"
fi
