#!/bin/bash

echo "--------------Калькулятор---------------"

if [ $# -lt 3 ]
then
    echo 'Ошибка: введите не менее трёх аргументов [число] [оператор] [число]'
    exit
fi

res=$1
shift 1

while [ $# -ge 2 ]
do
    
    operator=$1
    arg2=$2
    
    isNumber="^-?[0-9]+[.]?[0-9]*$"
    if ! [[ "$res" =~ $isNumber ]] || ! [[ "$arg2" =~ $isNumber  ]];
    then
        echo "Ошибка, аргументы не являются числами: $res, $arg2";
        exit
    fi
    
    case $operator in
        'x') operator='*' ;;
        'X') operator='*' ;;
        '+') operator='+' ;;
        '-') operator='-' ;;
        '/') operator='/' ;;
        *) echo 'Ошибка...'
           exit;;
    esac

    if [ "$operator" = '/' ] && [ `bc <<< "$arg2 == 0"` = '1' ]
    then
        echo "Ошибка: деление на ноль..."
        exit
    fi
    
    echo "$res $operator $arg2"

    echo "Целочисленные операции:"
    echo $( expr $res "$operator" $arg2 )

    echo "Вещественные операции:"
#    echo $( bc <<< "scale=6; $res $operator $arg2" )
    res=$( bc <<< "scale=6; $res $operator $arg2" )

    echo $res
    echo "------------Next_iteration----------"
    shift 2

done

echo "Результат: $res"
