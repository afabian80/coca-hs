#!/bin/bash

DICT=$1
INPUT=$2

if [ $# != 2 ];
then
    echo "Error: Argument mismatch!"
    echo "Usage: $0 <dict> <input>"
    exit 1
fi

echo "Translating words..."

iter=0
NUM=$(wc -l < $DICT | tr -d ' ')
# sample dictionary line:
# SQUIRE 116 (squire squires ) =fegyverhordozó
while read line; do
    let "iter++"
    echo -ne "\r${iter}/${NUM}"
    # get the text in the bracket
    WORDS=$(echo $line | cut -d '(' -f2 | cut -d ')' -f1)
    # get translation
    TRANSLATION=$(echo $line | cut -d '=' -f2)
    # get occurances
    OCC=$(echo $line | cut -d ' ' -f2)
    for word in $WORDS; do
        # using GNU sed command (for case insensitive replace)
        sed -i "s/\b\($word\)\b/\1 ($TRANSLATION $OCC)/gI" $INPUT
    done
done < $DICT
