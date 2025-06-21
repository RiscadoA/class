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

success=0
failed=0
processed=0
for file in $files
do
    processed=$((processed + 1))

    # Check if there are accompanying .trace or .out files
    basename=$(basename $file .clls)
    outfile=$(dirname $file)/$basename.out
    errfile=$(dirname $file)/$basename.err

    baseout=bin/$(dirname $file)/$basename
    flags="-P -o $baseout"
    if [ ! -f $errfile ]; then
        flags="$flags -t";
    fi
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
    echo -n " Running... "

    # Run the compiled file
    $baseout > $baseout.out 2> $baseout.err
    if [ $? -ne 0 ]; then
        error "execution failed! See $baseout.out and $baseout.err"
        failed=$((failed + 1))
        continue
    fi

    # Compare the standard output with the expected
    if [ -f $outfile ]; then
        diff $baseout.out $outfile > /dev/null 2>&1
        if [ $? -ne 0 ]; then
            error "expected $outfile, got $baseout.out"
            failed=$((failed + 1))
            continue
        fi
    fi

    # Compare the error output with the expected
    if [ -f $errfile ]; then
        diff $baseout.err $errfile > /dev/null 2>&1
        if [ $? -ne 0 ]; then
            error "expected $errfile, got $baseout.err"
            failed=$((failed + 1))
            continue
        fi
    fi

    success "passed\n"
    success=$((success + 1))
done

if [ $failed -eq 0 ]; then
    success "All tests passed!\n"
else
    error "Tests failed: $failed/$count"
fi
