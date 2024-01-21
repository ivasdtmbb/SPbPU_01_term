program lab_3_18_VS
   use Environment
   use Group_Process
   use Group_IO

   implicit none
   character(:), allocatable :: input_file, output_file, data_file
   character(kind=CH_), parameter   :: MALE = Char(1052, CH_), FEMALE = Char(1046, CH_)
      
   type(student)              :: Group(STUD_AMOUNT)
   type(student), allocatable :: Boys(:), Girls(:)

   input_file  = "../data/class.txt"
   output_file = "output.txt"
   data_file   = "class.dat"
   
   call Create_data_file(input_file, data_file)
   
   Group = Read_class_list(data_file)

   call Output_class_list(output_file, Group, "Исходный список:", "rewind")

   Boys  = Pack(Group, Group%Gender == MALE)
   Girls = Pack(Group, Group%Gender == FEMALE)

   call Sort_class_list(Boys)
   call Sort_class_list(Girls)

   call Output_class_list(output_file, Boys, "Успеваемость юношей:", "append")
   call Output_class_list(output_file, Girls, "Успеваемость девушек:", "append")

 end program lab_3_18_VS
