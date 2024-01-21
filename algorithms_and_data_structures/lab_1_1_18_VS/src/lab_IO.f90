module lab_IO
  use Environment

  implicit none
   integer                        :: STUD_AMOUNT
   integer, parameter             :: SURNAME_LEN  = 15
   integer, parameter             :: INITIALS_LEN = 5
   character(kind=CH_), parameter :: MALE = Char(1052, CH_), FEMALE = Char(1046, CH_)

  contains

!----------------------------------------------------------------------------------------------------!
    ! Чтение списка группы: фамилии, инициалы, пол, год рождения
    subroutine Read_group_list(Input_File, Surnames, Initials, Genders, YOB)
      character(*)                        :: Input_File
      character(*, kind=CH_), allocatable :: Surnames(:), Initials(:), Genders(:)
      integer, allocatable                :: YOB(:)

      intent(in)                          ::  Input_File
      intent(out)                         ::  Surnames, Initials, Genders, YOB

      Integer In, IO, i
      character(:), allocatable           :: format
      
      open (file=input_file, encoding=E_, newunit=In)
         read (In, '(i20)', iostat=IO) STUD_AMOUNT
         call Handle_IO_status(IO, "reading STUD_AMOUNT")
         allocate (Surnames(STUD_AMOUNT), Initials(STUD_AMOUNT), &
                   Genders(STUD_AMOUNT), YOB(STUD_AMOUNT))
           
         format = '(3(a, 1x), i4)'
         read (In, format, iostat=IO) (Surnames(i), Initials(i), Genders(i), YOB(i), i = 1, Size(Surnames))
         call Handle_IO_status(IO, "reading group list")
      close (In)

    end subroutine Read_group_list

!----------------------------------------------------------------------------------------------------!
    ! Вывод списка группы.
    subroutine Output_group_list(Output_File, Surnames, Initials, Genders, YOB, List_name, Position)
      character(*)                         Output_file, List_name, Position
      character(*, kind=CH_)               Surnames(:), Initials(:), Genders(:)
      integer                              YOB(:)
      intent(in)                           Output_File, Surnames, Initials, Genders, YOB, List_name, Position

      integer Out, IO, i
      character(:), allocatable         :: format

      open (file=Output_file, encoding=E_, position=position, newunit=Out)
         write (out, '(/a)') List_name
         format = '(3(a, 1x), i4)'
         write (Out, format, iostat=IO) (Surnames(i), Initials(i), Genders(i), YOB(i), i = 1, Size(Surnames))
         call Handle_IO_status(IO, "writing " // List_name)
      close (Out)
    end subroutine Output_group_list

!----------------------------------------------------------------------------------------------------!
    pure subroutine Get_list_by_gender(Surnames, Initials, Genders, YOB, &
         Gender_Surnames, Gender_Initials, Gender_YOB, Gender)
      character(*, kind=CH_)   Surnames(:), Initials(:), Genders(:)
      integer                  YOB(:)
      character(*, kind=CH_)   Gender_Surnames(:), Gender_Initials(:)
      integer                  Gender_YOB(:)
      character(kind=CH_)      Gender
      intent (in)              Surnames, Initials, Genders, YOB, Gender
      intent (out)             Gender_Surnames, Gender_Initials, Gender_YOB
      allocatable              Gender_Surnames, Gender_Initials, Gender_YOB

      logical, allocatable  :: Is_A_Gender(:)
      integer               :: Gender_Amount 

      ! Gender Logical mask
      Is_A_Gender  = Genders == Gender
      Gender_Amount = Count(Is_A_Gender)

      ! Gender arrays
      allocate (Gender_Surnames(Gender_Amount), Gender_Initials(Gender_Amount), &
           Gender_YOB(Gender_Amount))

      Gender_Surnames = Pack(Surnames, Is_A_Gender)
      Gender_Initials = Pack(Initials, Is_A_Gender)
      Gender_YOB      = Pack(YOB, Is_A_Gender)
    end subroutine Get_list_by_gender

!----------------------------------------------------------------------------------------------------!
    pure subroutine Sort_group_list(Surnames, Initials, YOB)
      character(*, kind=CH_)   Surnames(:), Initials(:)
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
         if (minInd /= j) then
            ! Перестановка местами двух элементов списка с помощью векторного индекса
            Surnames([j, minInd]) = Surnames([minInd, j])
            Initials([j, minInd]) = Initials([minInd, j])
            YOB([j, minInd]) = YOB([minInd, j])
         end if
      end do

    end subroutine Sort_group_list

!----------------------------------------------------------------------------------------------------!
    ! Проверка того, стоит ли менять местами двух человек в списке
    pure logical function Swap(YOB, Surnames, Initials, i, j)
      character(*, kind=CH_)   Surnames(:), Initials(:)
      integer                  YOB(:), i, j
      intent (in)              Surnames, Initials, YOB, i, j
      
      Swap = .false.
      if (YOB(i) < YOB(j)) then
         Swap = .true.
      else if (YOB(i) == YOB(j)) then
         if (Surnames(i) < Surnames(j)) then
            Swap = .true.
         else if (Surnames(i) == Surnames(j)) then
            if (Initials(i) < Initials(j)) then
               Swap = .true.
            end if
         end if
      end if
    end function Swap

!----------------------------------------------------------------------------------------------------!
  End module lab_IO
