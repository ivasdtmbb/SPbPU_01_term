#!/bin/bash

FC=gfortran

FILE=$1
shift
FOPT=$@

gfortran -Wall -std=f2008ts $FOPT $FILE -lm

EXECUTABLEFILE=./a.out

echo "Ключи оптимизации: $FOPT"
echo "Время, затраченное программой на выполнение:"
time ./a.out
echo "Размер исполняемого файла $(du -b "$EXECUTABLEFILE") byte"
