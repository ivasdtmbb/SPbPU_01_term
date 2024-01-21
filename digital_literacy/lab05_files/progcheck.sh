#!/bin/bash

FC=gfortran

file=$1

FOPT=$2

gfortran -Wall $(FOPT) $(file) -lm

echo "Ключи оптимизации: $2"
echo "Время, затраченное программой на выполнение:"


