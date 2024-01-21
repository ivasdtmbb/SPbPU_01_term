#!/bin/bash

# Write the scripts that checks the file existance.
# Filename is passed as a command line parameter.

file=$1

if [ $# -eq 0 ]
then
    echo "Ошибка: укажите путь к файлу."
elif [ -f $file ]
then
    echo "Файл '$file' существует."
else
    echo "Файл '$file' не существует."
fi
