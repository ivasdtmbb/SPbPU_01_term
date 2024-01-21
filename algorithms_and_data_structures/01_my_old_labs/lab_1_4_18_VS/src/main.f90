program lab_1_4_18_VS
   use Environment
   use Group_Process
   use Group_IO

   implicit none
   character(:), allocatable :: input_file, output_file, data_file
   character(kind=CH_), parameter   :: MALE = Char(1052, CH_), FEMALE = Char(1046, CH_)
   type(students)              :: Group, Male_Students, Female_Students

   input_file  = "../data/class.txt"
   output_file = "output.txt"
   data_file   = "class.dat"
   
   call Create_data_file(input_file, data_file)
   
   Group = Read_class_list(data_file)

   call Output_class_list(output_file, Group, "Исходный список:", "rewind")

   call Gender_group(Group, Male_Students, MALE)
   call Gender_group(Group, Female_Students, FEMALE)

   call Sort_class_list(Male_Students)
   call Sort_class_list(Female_Students)

   call Output_class_list(output_file, Male_Students, "Список юношей:", "append")
   call Output_class_list(output_file, Female_Students, "Список девушек:", "append")

 end program lab_1_4_18_VS
