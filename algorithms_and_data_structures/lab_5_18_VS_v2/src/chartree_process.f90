module Chartree_Process

   use Environment
   use Chartree_IO

   implicit none
   integer, parameter :: NONE_EXIST = -1
   
contains

! !--------------------------------------------------------------------------------!
  subroutine Calculate_Distance_CharTree(output_file, CharTree, val)
    character(*), intent(in)               :: output_file
    type(node_chartree), intent(in)        :: CharTree
    character(len=1, kind=CH_), intent(in) :: val
    integer                                :: distance, Out

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
  pure recursive function Find_Node(current, val, level) result(distance)
    type(node_chartree), intent(in) :: current
    character(kind=CH_), intent(in) :: val
    integer                         :: distance
    integer, intent(in)             :: level

    
    if (current%value == val) then
       distance = level

       ! (обернуть в if существует ли left, else -1)       
    else if (current%value > val) then
       if (Allocated(current%left)) then
          distance = Find_Node(current%left, val, level + 1)
       else
          distance = NONE_EXIST
       end if
    else if (current%value < val) then
       if (Allocated(current%right)) then
          distance = Find_Node(current%right, val, level + 1)
       else
          distance = NONE_EXIST
       end if
    end if
  end function Find_Node

!написать подпрограмму
!pure recursive subroutine rebalance_tree(

!--------------------------------------------------------------------------------!
end module Chartree_Process

  ! использовать функцию, запускаться на нуле, сделать дистанцию с 0
  ! сделать вывод дерева аккуратным - счётчик уровня, форматирование, завести стек или очередь итд

  ! обходить дерево в ширину (найти алг), туда спускать уровень на котором нахожусь и какой по счёту на этом уровне этот элемент
  ! научиться обходить дерево в ширину, и выводить элементы, снабдить параметрами

  ! Сделать балансировку дерева
