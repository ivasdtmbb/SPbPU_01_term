module group_process
  use Environment
  use lab_IO

  implicit none

  contains
    
!----------------------------------------------------------------------------------------------------!
    pure subroutine Get_list_by_gender(Surnames, Initials, Genders, YOB, &
         Gender_Surnames, Gender_Initials, Gender_YOB, Gender)
      character(*, kind=CH_)   Surnames(:, :), Initials(:, :), Genders(:)
      integer                  YOB(:)
      character(*, kind=CH_)   Gender_Surnames(:, :), Gender_Initials(:, :)
      integer                  Gender_YOB(:)
      character(kind=CH_)      Gender

      intent (in)              Surnames, Initials, Genders, YOB, Gender
      intent (out)             Gender_Surnames, Gender_Initials, Gender_YOB
      allocatable              Gender_Surnames, Gender_Initials, Gender_YOB

      logical, allocatable  :: Is_A_Gender(:)
      integer, allocatable  :: Gender_Pos(:)
      integer, allocatable  :: INDEXES(:)
      integer               :: Gender_Amount, i

      ! Logical mask corresponding to the gender
      Is_A_Gender  = Genders == Gender
      Gender_Amount = Count(Is_A_Gender)

      ! Gender arrays
      INDEXES = [(i, i = 1, STUD_AMOUNT)]
      Gender_Pos = Pack(INDEXES, Is_A_Gender)
      allocate (Gender_Surnames(SURNAME_LEN, Gender_Amount), &
           Gender_Initials(INITIALS_LEN, Gender_Amount), Gender_YOB(Gender_Amount))

      ! Получение двумерных списков для пола.
      do concurrent (i = 1:Gender_Amount)
         Gender_Surnames(:, i)  = Surnames(:, Gender_Pos(i))
         Gender_Initials(:, i)  = Initials(:, Gender_Pos(i))
         Gender_YOB(i)  = YOB(Gender_Pos(i))
      end do
    end subroutine Get_list_by_gender

!----------------------------------------------------------------------------------------------------!
    pure subroutine Sort_group_list(Surnames, Initials, YOB)
      character(*, kind=CH_)   Surnames(:, :), Initials(:, :)
      integer                  YOB(:), j, k, minInd
      intent (inout)           Surnames, Initials, YOB

      ! Selection sort by Year Of Birth / Сортировка выбором
      ! Поочерёдное сравнение всех элементов неотсортированной части массива,
      ! подстановка на первое место наименьшего элемента.
      
      do j = 1, Size(YOB) - 1
         minInd = j
         do k = j + 1, Size(YOB)
            if (Swap(YOB, Surnames, Initials, k, minInd)) then
               minInd = k
            end if
         end do
         call Swap_elements(Surnames, Initials, YOB, j, minInd)
      end do

    end subroutine Sort_group_list

!----------------------------------------------------------------------------------------------------!
    ! Проверка того, стоит ли менять местами двух человек в списке
    ! с заданными индексами i и j 
    pure logical function Swap(YOB, Surnames, Initials, i, j)
      character(*, kind=CH_)   Surnames(:, :), Initials(:, :)
      integer                  YOB(:), i, j
      intent (in)              Surnames, Initials, YOB, i, j
      
      Swap = .false.
      if (YOB(i) < YOB(j)) then
         Swap = .true.
      else if (YOB(i) == YOB(j)) then
         if (GT(Surnames(:, j), Surnames(:, i))) then
            Swap = .true.
         else if (ALL(Surnames(:, j) == Surnames(:, i)) &
              .and. GT(Initials(:, j), Initials(:, i))) then
            Swap = .true.
         end if
      end if

    end function Swap

!----------------------------------------------------------------------------------------------------!    
   ! Функция операции > для массивов символов.
   pure logical function GT(arr1, arr2)
      character(kind=CH_), intent(in) :: arr1(:), arr2(:)
      integer :: i 

      ! Поиск первого отличного символа или остановка на последнем символе.
      do  i = 1, Min(Size(arr1), Size(arr2)) - 1
         if (arr1(i) /= arr2(i)) &
              exit
      end do
      GT = arr1(i) > arr2(i)
    end function GT

!----------------------------------------------------------------------------------------------------!
    ! Перестановка местами двух элементов списка с заданными индексами i и j
    pure subroutine Swap_elements(Surnames, Initials, YOB, i, j)
      character(*, kind=CH_)   Surnames(:, :), Initials(:, :)
      integer                  YOB(:), i, j
      intent (in)              i, j
      intent (inout)           Surnames, Initials, YOB

      character(kind=CH_)      tmpSurname(SURNAME_LEN), tmpInitials(INITIALS_LEN)

      tmpSurname  = Surnames(:, i)
      Surnames(:, i) = Surnames(:, j)
      Surnames(:, j) = tmpSurname

      tmpInitials = Initials(:, i)
      Initials(:, i) = Initials(:, j)
      Initials(:, j) = tmpInitials

      YOB([i, j]) = YOB([j, i])
    end subroutine Swap_elements

  End module group_process
