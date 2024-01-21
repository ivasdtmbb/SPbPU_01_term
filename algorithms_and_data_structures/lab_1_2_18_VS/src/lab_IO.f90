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
      character(*, kind=CH_), allocatable :: Surnames(:, :), Initials(:, :), Genders(:)
      integer, allocatable                :: YOB(:)

      intent(in)                          :: Input_File
      intent(out)                         :: Surnames, Initials, Genders, YOB

      Integer In, IO, i
      character(:), allocatable         :: format
      
      open (file=input_file, encoding=E_, newunit=In)
         read (In, '(i20)', iostat=IO) STUD_AMOUNT
         call Handle_IO_status(IO, "reading STUD_AMOUNT")
         allocate (Surnames(SURNAME_LEN, STUD_AMOUNT),&
                   Initials(INITIALS_LEN, STUD_AMOUNT), &
                   Genders(STUD_AMOUNT), YOB(STUD_AMOUNT))

         format = '(' // SURNAME_LEN // 'a1, 1x, ' // INITIALS_LEN // 'a1, 1x, a, 1x, i4)'
         read (In, format, iostat=IO) (Surnames(:, i), Initials(:, i), Genders(i), YOB(i), i = 1, STUD_AMOUNT)
         call Handle_IO_status(IO, "reading group list")
      close (In)
    end subroutine Read_group_list

!----------------------------------------------------------------------------------------------------!
    ! Вывод списка группы.
    subroutine Output_group_list(Output_File, Surnames, Initials, Genders, YOB, List_name, Position)
      character(*)                         Output_file, List_name, Position
      character(*, kind=CH_)               Surnames(:, :), Initials(:, :), Genders(:)
      integer                              YOB(:)
      intent(in)                           Output_File, Surnames, Initials, Genders, YOB, List_name, Position

      integer Out, IO, i
      character(:), allocatable         :: format

      open (file=Output_file, encoding=E_, position=position, newunit=Out)
         write (out, '(/a)') List_name
         format = '(' // SURNAME_LEN // 'a1, 1x, ' // INITIALS_LEN // 'a1, 1x, a, 1x, i4)'
         write (Out, format, iostat=IO) (Surnames(:, i), Initials(:, i), Genders(i), YOB(i), i = 1, Size(YOB))
         call Handle_IO_status(IO, "writing " // List_name)
      close (Out)
    end subroutine Output_group_list

  end module lab_IO
