program lab_1_2_18
   use Environment
   use lab_IO
   use group_process

   implicit none

   character(kind=CH_), parameter   :: MALE = Char(1052, CH_), FEMALE = Char(1046, CH_)
   ! character(kind=CH_), parameter   :: MALE = Char(0077, CH_), FEMALE = Char(0070, CH_) ! CH__"\u1052" CH__"М"

   character(:), allocatable  :: input_file, output_file

   ! Массивы фамилий, инициалов, полов, годов рождения и временные переменные для обменов при сортировке.

   ! The matrixes (Surnames(:, :), Initials(:, :)) data stored in lines, i.e.
   ! original          : Matrix(m, n)
   ! stored and indexed: Matrix(n, m)
   character(kind=CH_)              :: Surnames(SURNAME_LEN, STUD_AMOUNT)  = "", &
                                        Initials(INITIALS_LEN, STUD_AMOUNT) = "", &
                                        Genders(STUD_AMOUNT)                = ""
   character(kind=CH_), allocatable :: Boys_Surnames(:, :), Girls_Surnames(:, :)
   character(kind=CH_), allocatable :: Boys_Initials(:, :), Girls_Initials(:, :)
   integer              :: YOB(STUD_AMOUNT) = 0, i = 0
   integer, allocatable :: Boys_YOB(:), Girls_YOB(:)
   
   input_file = "../data/class.txt"
   output_file = "output.txt"
   
   call Read_group_list(input_file, Surnames, Initials, Genders, YOB)
   call Output_group_list(output_file, Surnames, Initials, Genders, YOB, "Исходный список: ", "rewind")
!   print '(12a6, 1x)', Initials

   call Get_list_by_gender(Surnames, Initials, Genders, YOB, Boys_Surnames, &
        Boys_Initials, Boys_YOB, MALE)
   call Get_list_by_gender(Surnames, Initials, Genders, YOB, Girls_Surnames, &
        Girls_Initials, Girls_YOB, FEMALE)

   call Sort_group_list(Boys_Surnames, Boys_Initials, Boys_YOB)
   call Sort_group_list(Girls_Surnames, Girls_Initials, Girls_YOB)

   call Output_group_list(output_file, Boys_Surnames, Boys_Initials, [(MALE, i = 1, Size(Boys_YOB))], &
        Boys_YOB, "Список юношей:", "append")
   call Output_group_list(output_file, Girls_Surnames, Girls_Initials, [(FEMALE, i = 1, Size(Girls_YOB))], &
        Girls_YOB, "Список девушек:", "append")

end program lab_1_2_18
