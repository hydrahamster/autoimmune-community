#!/bin/bash

# Remove our old file
rm new-drugs.txt
# 'Touch' the file to ensure it's blank
touch new-drugs.txt

while read drug;
do
#    echo "$drug" ;
    if grep -q "$drug" 4_Other-treatments-code.R
    then #if found
        continue
    else #if not found
        printf "$drug \n"  >> new-drugs.txt
    fi     
done <drugs-other-all_final.txt
