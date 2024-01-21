module Group_IO
   use Environment

   implicit none
   integer            :: STUD_AMOUNT
   integer, parameter :: SURNAME_LEN  = 15
   integer, parameter :: INITIALS_LEN = 5
   character(kind=CH_), parameter   :: MALE = Char(1052, CH_), FEMALE = Char(1046, CH_)

   ! Структура данных для хранения данных о студенте.
   type students
      character(SURNAME_LEN, kind=CH_), allocatable  :: Surnames(:)
      character(INITIALS_LEN, kind=CH_), allocatable :: Initials(:)
      character(kind=CH_), allocatable               :: Genders(:)
      integer(I_), allocatable                       :: YOBs(:)
   end type students
   
contains
   ! Создание неформатированного файла данных.
   subroutine Create_data_file(Input_File, Data_File)
      character(*), intent(in)   :: Input_File, data_file
      character(SURNAME_LEN, kind=CH_), allocatable   :: Surnames(:)
      character(INITIALS_LEN, kind=CH_), allocatable  :: Initials(:)
      character(kind=CH_), allocatable                :: Genders(:)
      integer(I_), allocatable                        :: YOBs(:)

      integer                    :: In, Out, IO, i
      character(:), allocatable  :: format
      
      ! Создание массивов с данными (Фамилии всех студентов, инициалы всех студентов...)
      open (file=Input_File, encoding=E_, newunit=In)
         read (In, '(i20)', iostat=IO) STUD_AMOUNT
         call Handle_IO_status(IO, "reading STUD_AMOUNT")
         allocate(Surnames(STUD_AMOUNT), Initials(STUD_AMOUNT), &
                  Genders(STUD_AMOUNT), YOBs(STUD_AMOUNT))

         format = '(3(a, 1x), i4)'
         read (In, format, iostat=IO) (Surnames(i), Initials(i), Genders(i), YOBs(i), i = 1, STUD_AMOUNT)
         call Handle_IO_status(IO, "Reading class list")
      close (In)

      ! Запись массивов один за другим
      open (file=Data_File, form='unformatted', newunit=Out, access='stream')
         write (Out, iostat=IO) Surnames, Initials, Genders, YOBs
         call Handle_IO_status(IO, "Reading unformatted class list")
      close(Out)
   end subroutine Create_data_file

   ! Чтение списка класса: фамилии, инициалы, пол и год рождения.
   function Read_class_list(Data_File) result(Group)
      type(students)             ::   Group
      character(*), intent(in)   :: Data_File

      integer In, IO

      allocate(Group%Surnames(STUD_AMOUNT), Group%Initials(STUD_AMOUNT), &
           Group%Genders(STUD_AMOUNT), Group%YOBs(STUD_AMOUNT))
      
      open (file=Data_File, form='unformatted', newunit=In, access='stream')
         read (In, iostat=IO) Group%Surnames, Group%Initials, Group%Genders, Group%YOBs
         call Handle_IO_status(IO, "reading unformatted class list")
      close (In)
   end function Read_class_list
 
   ! Вывод списка класса.
   subroutine Output_class_list(Output_File, Group, List_name, Position)
      character(*), intent(in)    :: Output_File, Position, List_name
      type(students), intent(in)  :: Group
      integer                     :: Out, IO, i
      character(:), allocatable   :: format
      
      open (file=Output_File, encoding=E_, position=Position, newunit=Out)
         write (out, '(/a)') List_name
         format = '(3(a, 1x), i4)'
         do i = 1, Size(Group%Surnames)
            write (Out, format, iostat=IO) Group%Surnames(i), Group%Initials(i), &
                 Group%Genders(i), Group%YOBs(i)
            call Handle_IO_status(IO, "writing " // List_name)
         end do
      close (Out)
   end subroutine Output_class_list

end module Group_IO 
