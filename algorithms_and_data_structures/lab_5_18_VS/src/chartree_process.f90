module Chartree_Process

   use Environment
   use Chartree_IO

   implicit none

contains

!--------------------------------------------------------------------------------!
  subroutine Calculate_Distance_CharTree(output_file, CharTree, val)
    character(*), intent(in)                  :: output_file
    type(node_chartree), pointer, intent(in)  :: CharTree
    character(len=1, kind=CH_), intent(in)    :: val
    integer                                   :: distance, Out


    distance = Find_Node(CharTree, val, 0)

    open (file=output_file, encoding=E_, position="append", newunit=Out)
       write (out, *) "Расстояние от корня дерева (", &
            CharTree%value, ") до элемента (", val, "):"
       if (distance == -1) then
          write (out, '(a)') "Ошибка: элемент не найден."
       else
          write (out, '(i0)') distance
       end if
    close (out)
  end subroutine Calculate_Distance_CharTree

!--------------------------------------------------------------------------------!
  pure recursive function subroutine Find_Node(current, val, distance) result(dist)
    type(node_chartree), intent(in)  :: current
    character(kind=CH_), intent(in)  :: val
    integer, intent(in)              :: distance, Find_Node


    if (current%value == val)
       Find_Node = distance
       else if (current%value > val) then
!          (обернуть в if существует ли left, else -1)
          dist =  Find_Node(current%left, val, distance+1)
       else if (current%value < val) then
          dist =  Find_Node(current%right, val, distance+1)
       end if

  end subroutine Find_Node

  end module Chartree_Process

  ! использовать объекты, а не ссылки на них (там, где нужно)
  ! использовать функцию, запускаться на нуле, сделать дистанцию с 0
  ! сделать вывод дерева аккуратным - счётчик уровня, форматирование, завести стек или очередь итд

  ! в 4 задании перегрузить оператор деления (в определении типа, как в последней лекции)
  ! Провести деление по схеме Горнера
  ! Найти первую производную, коэф и степени записать в файл


  ! обходить дерево в ширину (найти алг), туда спускать уровень на котором нахожусь и какой по счёту на этом уровне этот элемент
  ! научиться обходить дерево в ширину, и выводить элементы, снабдить параметрами
  ! когда элемент не выводится, нужно, чтобы
  ! посмотреть алг-ты обхода дерева в ширину
