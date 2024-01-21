module Group_Process
   use Environment
   use Group_IO

   implicit none
   
contains

  !-------------------------------------------------------------------------!
  ! Сортировка списка группы по году рождения, используя метод "выбором" (selection sort) 
  pure recursive subroutine Sort_class_list(Group, N)
    type(student), intent(inout)   :: Group(:)
    integer, intent(in)            :: N
    integer                        :: youngest_stud
    type(student)                  :: tmp_stud

    ! Находим индекс самого молодого студента, используя рекурсивную функцию
    youngest_stud = Find_youngest(Group, N, 1, N)

    ! Ставим самого молодого студента в конец массива
    tmp_stud = Group(N)
    Group(N) = Group(youngest_stud)
    Group(youngest_stud) = tmp_stud
    
    if (N >= 2) &
         call Sort_class_list(Group, N-1)
  end subroutine Sort_class_list

  !-------------------------------------------------------------------------!
  ! Поочерёдно сравниваем студентов, передаём дальше в рекурсивную
  ! функцию индекс самого молодого студента (согласно результату сравнения).
  ! Если достигли конца массива студентов, возвращаем индекс самого молодого. 
  pure recursive function Find_youngest(Group, youngest, j, N) result(res)
    type(student), intent(in)   :: Group(:)
    integer, intent(in)         :: N, j, youngest
    integer                     :: res

    if (j == N) then
       res = youngest
    else if (Swap(Group, j, youngest)) then
       res = Find_youngest(Group, j, j+1, N)
    else
       res = Find_youngest(Group, youngest, j+1, N)
    end if
  end function Find_youngest

  !-------------------------------------------------------------------------!
   ! Проверка того, стоит ли менять местами учащихся
   pure logical function Swap(Group, i, j)
      type(student), intent(in)  :: Group(:)
      integer, intent(in)        :: i, j

      Swap = .false.
      if (Group(i)%YOB > Group(j)%YOB) then
         Swap = .true.
      else if (Group(i)%YOB == Group(j)%YOB) then
         if (Group(i)%Surname > Group(j)%Surname) then
            Swap = .true.
         else if (Group(i)%Surname==Group(j)%Surname .and. &
              Group(i)%Initials > Group(j)%Initials) then
            Swap = .true.
         end if
     end if
   end function Swap
end module group_process
