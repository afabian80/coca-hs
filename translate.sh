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

# sample dictionary line:
# SQUIRE 116 (squire squires ) =fegyverhordozó
while read line; do
    # get the text in the bracket
    WORDS=$(echo $line | cut -d '(' -f2 | cut -d ')' -f1)
    TRANSLATION=$(echo $line | cut -d '=' -f2)
    for word in $WORDS; do
        echo "Translating $word"
        /usr/bin/sed -ie "s/[[:<:]]$word[[:>:]]/$word ($TRANSLATION)/g" $INPUT
        echo ""
    done
done < $DICT
