#!/bin/bash

# Remove our old file
rm new-treatments.txt
# 'Touch' the file to ensure it's blank
touch new-treatments.txt

while read treatment;
do
#    echo "$treatment" ;
    if grep -q "$treatment" 4_1-22_Other-treatments-code.R
    then #if found
        continue
    else #if not found
        printf "$treatment \n"  >> new-treatments.txt
    fi     
done <treatments-other-all_1-22.txt 