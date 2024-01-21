#!/bin/bash
set -f 

a=$1

while [ 1 ]
do 
   a=$1
   o=$2
   b=$3
  
   if [ $# -lt 3 ]
   then 
      echo 'Неправильное число аргументов. Операции не произведены'
      exit
   fi

   myreg='^[+-]?[0-9]+.?[0-9]*$'
   myope='^[+-xX/]$'

   if ! [[ $a =~ $myreg ]] ;
   then 
      echo "Аргумент 1 не является числом. Задано: $1" 
      exit
   fi

   if ! [[ $o =~ $myope ]] ;
   then 
      echo "Аргумент 2 не является корректно заданной операцией. Задано: $2" 
      exit
   fi

   if ! [[ $b =~ $myreg ]] ;
   then 
      echo "Аргумент 3 не является числом. Задано: $3" 
      exit
   fi

   ope='+'
   case $o in 
      'x') ope=" * " ;;
      'X') ope=" * " ;;
      '+') ope='+' ;;
      '-') ope='-' ;;
      '/') ope='/' ;;
      *)   echo 'Internal error'
           exit;;
   esac

   st=`echo -e  "$b == 0" | bc`
   if [ $ope =  "/" ] && [ $st -eq 1 ]  
   then
      echo "Невозможно выполнить вычисления. Нельзя делить на 0"
      exit
   fi

   #echo "Целочисленные операции:"
   #echo `expr $1 $ope $3`

   echo "$a  $ope  $b"

   echo "Вещественные операции:"
   echo `bc <<< "scale=6; $a $ope $b"`

   a=`echo -e "scale=6; $a $ope $b" | bc`

   echo $res

   shift 2
   if [ $# -eq 1 ] 
   then 
      exit
   fi
   #echo $#
   #exit
done
