program lab_1_5_18_VS
   use Environment
   use Group_Process
   use Group_IO

   implicit none
   character(:), allocatable :: input_file, output_file, data_file
      
   type(student), allocatable :: Group(:), Boys(:), Girls(:)
   real                       :: start, finish

   input_file  = "../data/class.txt"
   output_file = "output.txt"
   data_file   = "class.dat"
   
   call Create_data_file(input_file, data_file)
   Group = Read_class_list(data_file)
   call Output_class_list(output_file, Group, "Исходный список:", "rewind")

   call cpu_time(start)
   Boys  = Pack(Group, Group%Gender == MALE)
   Girls = Pack(Group, Group%Gender == FEMALE)

   call Sort_class_list(Boys, Size(Boys))
   call Sort_class_list(Girls, Size(Girls))
   call cpu_time(finish)
   print '("Time = ",f20.3," seconds.")', finish-start   

   call Output_class_list(output_file, Boys, "Успеваемость юношей:", "append")
   call Output_class_list(output_file, Girls, "Успеваемость девушек:", "append")

 end program lab_1_5_18_VS
