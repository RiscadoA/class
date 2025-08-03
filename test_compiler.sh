#!/usr/bin/env bash

function success {
    if [ -t 1 ]; then
        echo -n -e "\e[32m$1\e[0m"
    else
        echo -n $1
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

if [ -z "$CLLS_FLAGS" ]; then
    CLLS_FLAGS=""
else
    CLLS_FLAGS="$CLLS_FLAGS "
fi
CLLS_FLAGS="$CLLS_FLAGS-P"
echo "Using flags: $CLLS_FLAGS"

success=0
failed=0
processed=0
for file in $files
do
    processed=$((processed + 1))

    # Check if there are accompanying .trace, .in, .out or .err files
    basename=$(basename $file .clls)
    infile=$(dirname $file)/$basename.in
    outfile=$(dirname $file)/$basename.out
    errfile=$(dirname $file)/$basename.err

    baseout=bin/$(dirname $file)/$basename
    flags="$CLLS_FLAGS -o $baseout"
    mkdir -p $(dirname $baseout)

    echo -n "($processed/$count) Compiling $file... "

    # Compile the file
    ./compile.sh $flags $file &> $baseout.err
    if [ $? -ne 0 ]; then
        error "compilation failed! See $baseout.err"
        failed=$((failed + 1))
        continue
    fi
    success "success!"

    # Check if there are input files
    shopt -s nullglob
    just_failed=0
    infiles=($infile.*)
    if [ ${#infiles[@]} -eq 0 ]; then
        infiles=($infile)
    fi
    echo -n " Running..."
    for file in "${infiles[@]}"; do
        suffix=${file#$infile}

        # Run the compiled file with the input file, if it exists
        if [ -f $file ]; then
            $baseout > $baseout.out$suffix 2> $baseout.err$suffix < $file
        else
            $baseout > $baseout.out$suffix 2> $baseout.err$suffix
        fi
        if [ $? -ne 0 ]; then
            error " execution failed! See $baseout.out$suffix and $baseout.err$suffix"
            just_failed=1
            break
        fi

        # Compare the standard output with the expected
        if [ -f $outfile$suffix ]; then
            diff $baseout.out$suffix $outfile$suffix > /dev/null 2>&1
            if [ $? -ne 0 ]; then
                error " expected $outfile$suffix, got $baseout.out$suffix"
                just_failed=1
                break
            fi
        fi

        # Compare the error output with the expected
        if [ -f $errfile$suffix ]; then
            diff $baseout.err$suffix $errfile$suffix > /dev/null 2>&1
            if [ $? -ne 0 ]; then
                error " expected $errfile$suffix, got $baseout.err$suffix"
                just_failed=1
                break
            fi
        fi

        if [[ $suffix != "" ]]; then
            success " $suffix"
        fi
    done
    success " passed"

    # If any of the passes above failed
    if [ $just_failed -ne 0 ]; then
        failed=$((failed + 1))
    else
        success=$((success + 1))
        success "\n"
    fi
done

if [ $failed -eq 0 ]; then
    success "All tests passed!\n"
else
    error "Tests failed: $failed/$count"
fi
