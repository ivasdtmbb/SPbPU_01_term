#!/bin/sh

echo "Калькулятор от дяди Васи. Поддерживает операции: +, -, x, /"

wantExit="false"

userDialog() {
    echo "*____________________ПУСК_____________________*"
    echo "Введите аргументы: <число1> <оператор> <число2>"
    read number1 operator number2
}    

initialCheck() {
    if [ -n "$number1" -a -n "$operator" -a -n "$number2" ]
    then
        initCheck=true
    else
        initCheck=false
    fi
}

calculating() {
    if [ $operator = "+" ]
    then
        result=$( expr $number1 + $number2 )
#        let "result = $number1 + $number2"
    elif [ $operator = "-" ]
    then
        result=$( expr $number1 - $number2 )
    elif [ $operator = "x" -o $operator = "X" ]
    then
        result=$(( $number1 * $number2 ))
    elif [ $operator = "/" ]
    then
        if [ $number2 = "0" ]
        then
           result="Братишка, делить на ноль - последнее дело..."
        else
           result=$( expr $number1 / $number2 )
        fi
    else
        result="Братишка, введи верный оператор."
    fi
}

cleanData() {
    unset number1 operator number2 initCheck result wantExit
}    

while [ $wantExit != "exit" -a $wantExit != "q" ]
do
    userDialog
    initialCheck

    if [ $initCheck = "false" ]
    then
        echo "Братишка, что-то пошло не так, извини..."
        break
    fi
    
    calculating

    echo $result
    echo "Чтобы продолжить, нажми почти любую клавишу."
    echo "Братишка, для выхода набери exit."
    cleanData
    read wantExit

done
