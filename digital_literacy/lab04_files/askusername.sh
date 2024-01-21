#!/bin/sh
echo "Введите имя пользователя:"
read username
if [ "$username" = "ivas" ]
then
    echo 'Успешно!!! Вы зашли.'
else
    echo 'Извините, неправильное имя пользователя.'
fi
