program lab_1_6_18_VS
   use Environment
   use Group_Process
   use Group_IO

   implicit none
   character(:), allocatable      :: input_file, output_file
   character(kind=CH_), parameter :: MALE = Char(1052, CH_), FEMALE = Char(1046, CH_)

   type(student), pointer         :: Group_List => Null(), &
                                     Boys_List => Null(), &
                                     Girls_List => Null()

   input_file  = "../data/class.txt"
   output_file = "output.txt"
   
   Group_List => Read_Group_list(input_file)

   if (Associated(Group_List)) then
      call Output_group_list(output_file, Group_List, "Исходный список:", "rewind")

      call Get_list_by_gender(Group_List, Boys_List, MALE)
      call Get_list_by_gender(Group_List, Girls_List, FEMALE)
   
      call Sort_Group_List(Boys_List)
      call Sort_Group_List(Girls_List)
   
      if (Associated(Boys_List)) &
         call Output_group_list(output_file, Boys_List, "Успеваемость юношей:", "append")
      if (Associated(Girls_List)) &
         call Output_group_list(output_file, Girls_List, "Успеваемость девушек:", "append")
   end if

end program lab_1_6_18_VS
