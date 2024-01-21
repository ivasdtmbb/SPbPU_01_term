program lab_3_18_VS
   use Environment
   use Persons_Process
   use Persons_IO

   implicit none
   character(:), allocatable :: input_file, output_file

   type(person), allocatable  :: PersonsList
    
   input_file  = "../data/people.txt"
   output_file = "output.txt"
 
   PersonsList = Read_Persons_List(input_file)
 
   if (Allocated(PersonsList)) then
       call Output_Persons_List(output_file, PersonsList, "Исходный список фамилий:","rewind")
       call Insertion_Sort_Persons_List(PersonsList, PersonsList)
       call Output_Persons_List(output_file, PersonsList, "Отсортированный список фамилий:","append")
      call Purge(PersonsList)

      if (Allocated(PersonsList)) then
         call Output_Persons_List(output_file, PersonsList, "Список после процедуры Purge:","append")
      end if
   end if
 end program lab_3_18_VS
