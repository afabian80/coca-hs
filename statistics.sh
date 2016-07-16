#!/bin/bash

# Commands are for OS X v10.11.5

echo "Collecting word statistics"

INPUT=greenwords.txt
OUTPUT=occurences.txt
CONSOLIDATED=consolidated.txt
#NUM=$(wc -l $INPUT)

rm -f $OUTPUT

# Find occurences and root for each word
for WORD in `head -n 20 $INPUT`; do
    echo -n "."
    OCC=$(grep -ciw $WORD index.html)
    BASEWORD=$(grep -Riw -B 50 $WORD cocadb-indented | grep -iE "txt-\w|txt:\w" | tail -n1 | egrep -iEo "(\w+) ")
    echo -e "$WORD\t$BASEWORD\t$OCC" >> $OUTPUT
done

echo ""
# Consoldate statistics by summing root words on column 2 with values on column 3,
# then reverse numerically sort output on column 2
awk '{arr[$2]+=$3}END{for (a in arr) print a, arr[a]}' $OUTPUT | sort -n -r -k 2,2 > $CONSOLIDATED

# Collect formed words again
while read line; do
    WORD=$(echo $line | cut -d ' ' -f 1)
    OCC=$(echo $line | cut -d ' ' -f 2)
    OCC_LINES=$(grep -iw $WORD $OUTPUT | cut -f 1 | tr '\n' ' ')
    echo "$WORD $OCC ($OCC_LINES)"
done < $CONSOLIDATED
