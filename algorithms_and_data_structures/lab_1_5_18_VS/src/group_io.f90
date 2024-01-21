module Group_IO
   use Environment

   implicit none
   integer            :: STUD_AMOUNT
   integer, parameter :: SURNAME_LEN  = 15
   integer, parameter :: INITIALS_LEN = 5
   character(kind=CH_), parameter   :: MALE = Char(1052, CH_), FEMALE = Char(1046, CH_)

   ! Структура данных для хранения данных о студенте.
   type student
      character(SURNAME_LEN, kind=CH_)  :: Surname   = ""
      character(INITIALS_LEN, kind=CH_) :: Initials  = ""
      character(kind=CH_)               :: Gender    = ""
      integer(I_)                       :: YOB       = 0
   end type student
   
contains
   ! Создание неформатированного файла данных.
   subroutine Create_data_file(Input_File, Data_File)
      character(*), intent(in)   :: Input_File, data_file
      
      type(student)              :: stud
      integer                    :: In, Out, IO, i, recl
      character(:), allocatable  :: format
      
      open (file=Input_File, encoding=E_, newunit=In)
         read (In, '(i20)', iostat=IO) STUD_AMOUNT
         call Handle_IO_status(IO, "reading STUD_AMOUNT")

         recl = (SURNAME_LEN + INITIALS_LEN + 1)*CH_ + I_
         open (file=Data_File, form='unformatted', newunit=Out, access='direct', recl=recl)
            format = '(3(a, 1x), i4)'
            do i = 1, STUD_AMOUNT
               read (In, format, iostat=IO) stud

               call Handle_IO_status(IO, "reading formatted class list, line " // i)
            
               write (Out, iostat=IO, rec=i) stud
               call Handle_IO_status(IO, "creating unformatted file with class list, record " // i)
            end do
      close (In)
      close (Out)
   end subroutine Create_data_file

   ! Чтение списка класса: фамилии, инициалы, пол и год рождения.
   function Read_class_list(Data_File) result(Group)
      type(student), allocatable :: Group(:)
      character(*), intent(in)   :: Data_File

      integer In, IO, recl
      
      allocate (Group(STUD_AMOUNT))
      recl = ((SURNAME_LEN + INITIALS_LEN + 1)*CH_ + I_) * STUD_AMOUNT
      open (file=Data_File, form='unformatted', newunit=In, access='direct', recl=recl)
         read (In, iostat=IO, rec=1) Group
         call Handle_IO_status(IO, "reading unformatted class list")
      close (In)
   end function Read_class_list
 
   ! Вывод списка класса.
   subroutine Output_class_list(Output_File, Group, List_name, Position)
      character(*), intent(in)   :: Output_File, Position, List_name
      type(student), intent(in)  :: Group(:)

      integer                    :: Out, IO
      character(:), allocatable  :: format
      
      open (file=Output_File, encoding=E_, position=Position, newunit=Out)
         write (out, '(/a)') List_name
         format = '(3(a, 1x), i4)'
         write (Out, format, iostat=IO) Group
         call Handle_IO_status(IO, "writing " // List_name)
      close (Out)
   end subroutine Output_class_list
end module Group_IO 
