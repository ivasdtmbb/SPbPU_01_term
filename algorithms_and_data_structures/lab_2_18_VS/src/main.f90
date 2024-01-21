program lab_2_18_VS
   use Environment
   use Source_Process
   use Source_IO

   implicit none
   character(:), allocatable :: F1, F2, F3

   type(FileName), pointer :: FileNamesMask      => Null()   ! Маска имени файла.
   type(FileName), pointer :: FileNamesForCheck  => Null()   ! Имя файла для проверки.
   type(FileName), pointer :: FileNamesTheSame   => Null()   ! Подходящее маске имя файла.

   F1 = "../data/mask.txt"
   F2 = "../data/file_names.txt"
   F3 = "found_names.txt"
   
   FileNamesMask => Read_FN_Records(F1)
   call Output_File_Names(F3, FileNamesMask, "rewind")
   
   
   FileNamesForCheck => Read_FN_Records(F2)
   ! call Output_File_Names(F3, FileNamesForCheck, "rewind")
  
   if (Associated(FileNamesMask) .and. Associated(FileNamesForCheck)) then
      FileNamesTheSame => Get_Same_File_Names(FileNamesMask, FileNamesForCheck)

      if (Associated(FileNamesTheSame)) &
           call Output_File_Names(F3, FileNamesTheSame, "rewind")
   end if

 end program lab_2_18_VS
