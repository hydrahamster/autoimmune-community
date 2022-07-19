#!/bin/bash

# Remove our old file
rm new-ADs.txt
# 'Touch' the file to ensure it's blank
touch new-ADs.txt

while read illness;
do
    if grep -q "$illness" 2_Autoimmune-disorder-clean.R
    then #if found
        continue
    else #if not found
        printf "$illness \n"  >> new-ADs.txt
    fi     
done <ADs-other-all_final.txt
