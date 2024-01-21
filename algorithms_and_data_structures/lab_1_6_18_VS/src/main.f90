program lab_1_6_18_VS
   use Environment
   use Group_Process
   use Group_IO

   implicit none
   character(:), allocatable      :: input_file, output_file

   type(student), pointer         :: Group_List => Null(), &
                                     Boys_List => Null(), &
                                     Girls_List => Null()
   real                           :: start, finish

   input_file  = "../data/class.txt"
   output_file = "output.txt"
   
   Group_List => Read_Group_list(input_file)

   if (Associated(Group_List)) then
      call Output_group_list(output_file, Group_List, "Исходный список:", "rewind")

      call cpu_time(start)      
      call Get_list_by_gender(Group_List, Boys_List, MALE)
      call Get_list_by_gender(Group_List, Girls_List, FEMALE)
   
      call Sort_Group_List(Boys_List)
      call Sort_Group_List(Girls_List)
      call cpu_time(finish)
      print '("Time = ",f20.3," seconds.")', finish-start   

      if (Associated(Boys_List)) &
         call Output_group_list(output_file, Boys_List, "Успеваемость юношей:", "append")
      if (Associated(Girls_List)) &
         call Output_group_list(output_file, Girls_List, "Успеваемость девушек:", "append")
   end if

end program lab_1_6_18_VS
