module lab_IO
  use Environment
    
  implicit none
  integer, parameter     :: STUD_AMOUNT = 12, SURNAME_LEN = 15, INITIALS_LEN = 5, YOB_LEN = 4

  contains

!----------------------------------------------------------------------------------------------------!
    ! Чтение списка группы: фамилии, инициалы, пол, год рождения
    subroutine Read_group_list(Input_File, Surnames, Initials, Genders, YOB)
      character(*)            :: Input_File
      character(*, kind=CH_)  :: Surnames(:, :), Initials(:, :), Genders(:)
      integer                 :: YOB(:)
      intent(in)              :: Input_File
      intent(out)             :: Surnames, Initials, Genders, YOB

      Integer In, IO, i
      character(:), allocatable         :: format
      
      open (file=input_file, encoding=E_, newunit=In)
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
