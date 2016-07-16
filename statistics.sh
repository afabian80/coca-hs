#!/bin/bash

# Commands are for OS X v10.11.5

echo "Collecting word statistics"

INPUT=greenwords.txt
OUTPUT=stats.txt
#NUM=$(wc -l $INPUT)

rm $OUTPUT

# Find occurences and root for each word
for WORD in `cat $INPUT`; do
    echo -n "."
    OCC=$(grep -ciw $WORD index.html)
    BASEWORD=$(grep -Riw -B 50 $WORD cocadb-indented | grep -iE "txt-\w|txt:\w" | tail -n1 | egrep -iEo "(\w+) ")
    echo -e "$WORD\t$BASEWORD\t$OCC" >> $OUTPUT
done

echo ""
# Consoldate statistics by summing root words on column 2 with values on column 3,
# then reverse numerically sort output on column 2
awk '{arr[$2]+=$3}END{for (a in arr) print a, arr[a]}' $OUTPUT | sort -n -r -k 2,2
