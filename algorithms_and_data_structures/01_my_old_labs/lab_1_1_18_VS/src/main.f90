program lab_1_1_18
   use Environment
   use lab_IO

   implicit none
   integer, parameter         :: STUD_AMOUNT = 12, SURNAME_LEN = 15, INITIALS_LEN = 5, YOB_LEN = 4
   character(kind=CH_), parameter   :: MALE = Char(1052, CH_), FEMALE = Char(1046, CH_)

!   character(kind=CH_), parameter   :: MALE = Char(0077, CH_), FEMALE = Char(0070, CH_) ! CH__"\u1052" CH__"М"

   character(:), allocatable  :: input_file, output_file

   ! Массивы фамилий, инициалов, полов, годов рождения и временные переменные для обменов при сортировке.
   character(SURNAME_LEN, kind=CH_)                :: Surnames(STUD_AMOUNT) = ""
   character(SURNAME_LEN, kind=CH_), allocatable   :: Boys_Surnames(:), Girls_Surnames(:)

   character(INITIALS_LEN, kind=CH_)               :: Initials(STUD_AMOUNT) = ""
   character(INITIALS_LEN, kind=CH_), allocatable  :: Boys_Initials(:), Girls_Initials(:)

   character(kind=CH_)                             :: Genders(STUD_AMOUNT) = ""
   
   integer                                         :: YOB(STUD_AMOUNT) = 0
   integer, allocatable                            :: Boys_YOB(:), Girls_YOB(:) ! Boys_Pos(:), Girls_Pos(:)

!   integer                                         :: Boys_Amount = 0, Girls_Amount = 0
   
   integer :: i
   integer, parameter    :: INDEXES(*) = [(i, i = 1, STUD_AMOUNT)]

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

end program lab_1_1_18
