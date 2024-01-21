program lab_1_1_18
   use Environment
   use lab_IO

   implicit none

   character(:), allocatable  :: input_file, output_file

   ! Значение констант STUD_AMOUNT, SURNAME_LEN, INITIALS_LEN вводятся в модуле lab_IO
   ! Массивы фамилий, инициалов, полов, годов рождения и временные переменные для обменов при сортировке.
   character(SURNAME_LEN, kind=CH_), allocatable   :: Surnames(:), &
                                                      Boys_Surnames(:), &
                                                      Girls_Surnames(:)
   character(INITIALS_LEN, kind=CH_), allocatable  :: Initials(:), &
                                                      Boys_Initials(:), &
                                                      Girls_Initials(:)
   character(kind=CH_), allocatable                :: Genders(:)
   integer, allocatable                            :: YOB(:), Boys_YOB(:), Girls_YOB(:)
   integer                                         :: i
   real                                            :: start, finish

   input_file = "../data/class.txt"
   output_file = "output.txt"
   
   call Read_group_list(input_file, Surnames, Initials, Genders, YOB)
   call Output_group_list(output_file, Surnames, Initials, Genders, YOB, "Исходный список: ", "rewind")

   call cpu_time(start)
   call Get_list_by_gender(Surnames, Initials, Genders, YOB, Boys_Surnames, &
        Boys_Initials, Boys_YOB, MALE)
   call Get_list_by_gender(Surnames, Initials, Genders, YOB, Girls_Surnames, &
        Girls_Initials, Girls_YOB, FEMALE)

   call Sort_group_list(Boys_Surnames, Boys_Initials, Boys_YOB)
   call Sort_group_list(Girls_Surnames, Girls_Initials, Girls_YOB)
   call cpu_time(finish)
   print '("Time = ",f20.3," seconds.")', finish-start
   
   call Output_group_list(output_file, Boys_Surnames, Boys_Initials, [(MALE, i = 1, Size(Boys_YOB))], &
        Boys_YOB, "Список юношей:", "append")
   call Output_group_list(output_file, Girls_Surnames, Girls_Initials, [(FEMALE, i = 1, Size(Girls_YOB))], &
        Girls_YOB, "Список девушек:", "append")

end program lab_1_1_18
