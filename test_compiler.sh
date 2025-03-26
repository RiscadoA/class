#!/usr/bin/env bash

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

processed=0
for file in $files
do
    processed=$((processed + 1))

    # Check if there are accompanying .trace or .out files
    basename=$(basename $file .clls)
    outfile=$(dirname $file)/$basename.out

    baseout=bin/$(dirname $file)/$basename
    flags="-t -P -o $baseout"
    mkdir -p $(dirname $baseout)

    if [ ! -f $outfile ]; then
        error "($processed/$count) Skipping $file: missing expected output file $outfile"
        continue
    fi

    # Compile the file
    echo -n "($processed/$count) Compiling $file... "
    ./compile.sh $flags $file &> $baseout.err
    if [ $? -ne 0 ]; then
        error "failed! See $baseout.err"
        continue
    fi
    success "done"

    # Run the compiled file
    echo -n "($processed/$count) Running $file... "
    $baseout > $baseout.out 2> $baseout.err
    if [ $? -ne 0 ]; then
        error "failed! See $baseout.out and $baseout.err"
        continue
    fi

    # Compare the output with the expected
    diff $baseout.out $outfile > /dev/null 2>&1
    if [ $? -ne 0 ]; then
        error "expected $outfile, got $baseout.out"
        continue
    else
        success "passed"
    fi
done
