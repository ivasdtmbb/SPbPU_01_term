program lab_5_18_VS
   use Environment
   use Chartree_Process
   use Chartree_IO

   implicit none
   character(:), allocatable :: input_file, output_file

   type(node_chartree), allocatable  :: CharTree

   input_file  = "../data/chars.txt"
   output_file = "output.txt"
   
   CharTree = Read_CharTree(input_file)

   if (Allocated(CharTree)) then

      call Output_CharTree(output_file, CharTree, "Прямой обход дерева", "rewind")

      call Output_CharTree_Width(output_file, CharTree, "Обход дерева в ширину", "append")

      call Calculate_Distance_CharTree(output_file, CharTree, char(iachar('1'), CH_))
      call Calculate_Distance_CharTree(output_file, CharTree, char(iachar('@'), CH_))
      call Calculate_Distance_CharTree(output_file, CharTree, char(iachar('a'), CH_))
      call Calculate_Distance_CharTree(output_file, CharTree, char(iachar('q'), CH_))
      call Calculate_Distance_CharTree(output_file, CharTree, char(iachar('z'), CH_))
      call Calculate_Distance_CharTree(output_file, CharTree, char(1052, CH_))

   end if

 end program lab_5_18_VS
