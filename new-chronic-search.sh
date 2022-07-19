#!/bin/bash

# Remove our old file
rm new-chrons.txt
# 'Touch' the file to ensure it's blank
touch new-chrons.txt

while read illness;
do
    if grep -q "$illness" 2_Autoimmune-disorder-clean.R
    then #if found
        continue
    else #if not found
        printf "$illness \n"  >> new-chrons.txt
    fi     
done <chronic-other-all_final.txt
