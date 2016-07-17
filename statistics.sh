#!/bin/bash

# Commands are for OS X v10.11.5

if [ $# != 1 ];
then
    echo "Error: Argument mismatch!"
    echo "Usage: $0 <word-list-file>"
    exit 1
fi

echo "Collecting word statistics"

INPUT=$1
OUTPUT=occurences.txt
CONSOLIDATED=consolidated.txt
NUM=$(wc -l < $INPUT | tr -d ' ')

rm -f $OUTPUT

iter=0

# Find occurences and root for each word
for WORD in `cat $INPUT`; do
    let "iter++"
    echo -ne "\r${iter}/${NUM}"
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
