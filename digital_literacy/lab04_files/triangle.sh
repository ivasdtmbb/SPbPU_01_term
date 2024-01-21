#!/bin/bash

lines=$1

for (( i=1; $i <= $lines; i++ ))
do
    spaces=$( expr $lines - $i )
    for (( s=1; $s <= spaces; s++ ))
    do
        echo -n "  "
    done
    
    for (( k=1; k <= $i; k++ ))
    do
        echo -n " *"
    done
    printf "\n"
done
