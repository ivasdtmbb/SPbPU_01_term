#!/bin/bash

FC=gfortran

# Цикл, проверяющий ключи оптимизации
optTest() {
    for FOPT in "${OPTOPTIONS[@]}"
    do
        gfortran -Wall -std=f2008ts $FOPT $FILE -lm
        echo "Ключи оптимизации: $FOPT"
        echo "Время, затраченное программой на выполнение:"
        time ./a.out
        echo "Размер исполняемого файла $(du -b "./a.out") byte"
        echo ""
    done
}

# Массив ключей оптимизации
OPTOPTIONS=('-O0' '-Os' '-O1' '-O2' '-O3' '-O2 -march=native' '-O3 -march=native' '-O2 -march=native -funroll-loops' '-O3 -march=native -funroll-loops')

FILE=$1
#shift

# Запуск тестов оптимизации
optTest

echo "Выберите ключ оптимизации:"
read optkey
echo ""

# Составление наборов опций оптимизации
FOPT1="$optkey -fipa-sra -fipa-cp -fipa-vrp"
FOPT2="$optkey -flto"
FOPT3="$optkey -fprofile-generate"
FOPT4="$optkey -fprofile-use"
FOPT5="$optkey -fipa-sra -fipa-cp -fipa-vrp -flto -fprofile-generate"
FOPT6="$optkey -fipa-sra -fipa-cp -fipa-vrp -flto -fprofile-use"

OPTOPTIONS=("$FOPT1" "$FOPT2" "$FOPT3" "$FOPT4" "$FOPT5" "$FOPT6")

# Запуск тестов оптимизации
optTest




