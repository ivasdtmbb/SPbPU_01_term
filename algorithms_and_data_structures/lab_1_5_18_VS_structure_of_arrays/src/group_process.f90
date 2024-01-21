module Group_Process
   use Environment
   use Group_IO

   implicit none
   
contains

  !-------------------------------------------------------------------------!
  ! Сортировка списка группы по году рождения, используя метод "выбором" (selection sort) 
  pure recursive subroutine Sort_class_list(Group, N)
    type(students), intent(inout)  :: Group
    integer, intent(in)            :: N
    integer                        :: youngest_stud

    ! Находим индекс самого молодого студента, используя рекурсивную функцию
    youngest_stud = Find_youngest(Group, N, 1, N)

    ! Ставим самого молодого студента в конец массива
    Group%Surnames([N, youngest_stud]) = Group%Surnames([youngest_stud, N])
    Group%Initials([N, youngest_stud]) = Group%Initials([youngest_stud, N])
    Group%Genders([N, youngest_stud])  = Group%Genders([youngest_stud, N])
    Group%YOBs([N, youngest_stud])     = Group%YOBs([youngest_stud, N])
    
    if (N >= 2) &
         call Sort_class_list(Group, N-1)
  end subroutine Sort_class_list

  !-------------------------------------------------------------------------!
  ! Поочерёдно сравниваем студентов, передаём дальше в рекурсивную
  ! функцию индекс самого молодого студента (согласно результату сравнения).
  ! Если достигли конца массива студентов, возвращаем индекс самого молодого. 
  pure recursive function Find_youngest(Group, youngest, j, N) result(res)
    type(students), intent(in)  :: Group
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
      type(students), intent(in)  :: Group
      integer, intent(in)         :: i, j

      Swap = .false.
      if ((Group%YOBs(i)) > (Group%YOBs(j))) then
         Swap = .true.
      else if (Group%YOBs(i) == Group%YOBs(j)) then
         if (Group%Surnames(i) > Group%Surnames(j)) then
            Swap = .true.
         else if (Group%Surnames(i) == Group%Surnames(j) .and. &
              Group%Initials(i) < Group%Initials(j)) then
            Swap = .true.
         end if
      end if
   end function Swap
!-------------------------------------------------------------------------!
   ! Создание подгруппы одного пола
   pure subroutine Gender_group(Group, Sub_group, Gender)
     type(students), intent(in)        :: Group
     character(kind=CH_), intent(in)   :: Gender
     type(students), intent(inout)     :: Sub_group
     logical, allocatable              :: Is_a_gender(:)
     integer                           :: genderAmount

     Is_A_Gender = Group%Genders == Gender
     genderAmount = Count(Is_A_Gender)

     Sub_group%Surnames = Pack(Group%Surnames, Is_a_gender)
     Sub_group%Initials = Pack(Group%Initials, Is_a_gender)
     Sub_group%Genders = Pack(Group%Genders, Is_a_gender)
     Sub_group%YOBs = Pack(Group%YOBs, Is_a_gender)
   end subroutine Gender_Group
!---------------------------------------------------------------------------!
end module group_process
