module Group_Process
   use Environment
   use Group_IO

   implicit none
   
contains

!---------------------------------------------------------------------------!
   ! Сортировка списка класса по году рождения.
  pure subroutine Sort_class_list(Group)
      type(students), intent(inout)  :: Group
      integer        :: j, k, youngest_stud

      ! Selection sort by Year Of Birth / Сортировка выбором
      ! Поочерёдное сравнение всех элементов неотсортированной части массива,
      ! подстановка на первое место наименьшего элемента.
      do j = 1, Size(Group%Surnames) - 1
         youngest_stud = j
         do k = j + 1, Size(Group%Surnames)
            if (Swap(Group, k, youngest_stud)) then
               youngest_stud = k
            end if
         end do
         call Swap_two_students(Group, j, youngest_stud)
      end do
   end subroutine Sort_class_list

!---------------------------------------------------------------------------!
   ! Проверка того, стоит ли менять местами учащихся
   pure logical function Swap(Group, i, j)
      type(students), intent(in)  :: Group
      integer, intent(in)        :: i, j

      Swap = .false.
      if ((Group%YOBs(i)) < (Group%YOBs(j))) then
         Swap = .true.
      else if (Group%YOBs(i) == Group%YOBs(j)) then
         if (Group%Surnames(i) < Group%Surnames(j)) then
            Swap = .true.
         else if (Group%Surnames(i) == Group%Surnames(j) .and. &
              Group%Initials(i) < Group%Initials(j)) then
            Swap = .true.
         end if
      end if

   end function Swap

!---------------------------------------------------------------------------!
   ! Перестановка двух студентов местами
   pure subroutine Swap_two_students(Group, from_pos, to_pos)
     type(students), intent(inout)  :: Group
     integer, intent(in)            :: from_pos, to_pos
     
     character(SURNAME_LEN, kind=CH_) :: tmpSurname
     character(INITIALS_LEN, kind=CH_) :: tmpInitials
     character(kind=CH_) :: tmpGender
     integer(I_) :: tmpYOB

     tmpSurname = Group%Surnames(to_pos)
     Group%Surnames(to_pos) = Group%Surnames(from_pos)
     Group%Surnames(from_pos) = tmpSurname

     tmpInitials = Group%Initials(to_pos)
     Group%Initials(to_pos) = Group%Initials(from_pos)
     Group%Initials(from_pos) = tmpInitials

     tmpGender = Group%Genders(to_pos)
     Group%Genders(to_pos) = Group%Genders(from_pos)
     Group%Genders(from_pos) = tmpGender

     tmpYOB = Group%YOBs(to_pos)
     Group%YOBs(to_pos) = Group%YOBs(from_pos)
     Group%YOBs(from_pos) = tmpYOB
   end subroutine Swap_two_students

!---------------------------------------------------------------------------!
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
!----------------------------------------------------------------------------!
 end module group_process

