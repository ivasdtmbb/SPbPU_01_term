module Group_Process
   use Environment
   use Group_IO

   implicit none
   
contains
  ! Сортировка списка класса по году рождения.
  pure subroutine Sort_class_list(Group)
    type(student), intent(inout)  :: Group(:)
    integer        :: j, k, youngest_stud

    ! Selection sort by Year Of Birth / Сортировка выбором
    ! Поочерёдное сравнение всех элементов неотсортированной части массива,
    ! подстановка на первое место наименьшего элемента.
    do j = 1, Size(Group) - 1
       youngest_stud = j
       do k = j + 1, Size(Group)
          if (Swap(Group, k, youngest_stud)) then
             youngest_stud = k
          end if
       end do
       Group([j, youngest_stud]) = Group([youngest_stud, j]) ! Векторный индекс
    end do
  end subroutine Sort_class_list
  
  ! Проверка того, стоит ли менять местами учащихся
  pure logical function Swap(Group, i, j)
    type(student), intent(in)  :: Group(:)
    integer, intent(in)        :: i, j

    Swap = .false.
    if (Group(i)%YOB < Group(j)%YOB) then
       Swap = .true.
    else if (Group(i)%YOB == Group(j)%YOB) then
       if (Group(i)%Surname < Group(j)%Surname) then
          Swap = .true.
       else if (Group(i)%Surname==Group(j)%Surname .and. &
            Group(i)%Initials < Group(j)%Initials) then
          Swap = .true.
       end if
    end if
  end function Swap
end module group_process
