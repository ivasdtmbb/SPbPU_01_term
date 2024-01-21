program lab_1_4_18_VS
   use Environment
   use Group_Process
   use Group_IO

   implicit none
   character(:), allocatable :: input_file, output_file, data_file

   type(students)            :: Group, Male_Students, Female_Students
   real                      :: start, finish

   input_file  = "../data/class.txt"
   output_file = "output.txt"
   data_file   = "class.dat"
   
   call Create_data_file(input_file, data_file)
   Group = Read_class_list(data_file)
   call Output_class_list(output_file, Group, "Исходный список:", "rewind")

   call cpu_time(start)
   call Gender_group(Group, Male_Students, MALE)
   call Gender_group(Group, Female_Students, FEMALE)

   call Sort_class_list(Male_Students)
   call Sort_class_list(Female_Students)
   call cpu_time(finish)
   print '("Time = ",f20.3," seconds.")', finish-start

   call Output_class_list(output_file, Male_Students, "Список юношей:", "append")
   call Output_class_list(output_file, Female_Students, "Список девушек:", "append")

 end program lab_1_4_18_VS
