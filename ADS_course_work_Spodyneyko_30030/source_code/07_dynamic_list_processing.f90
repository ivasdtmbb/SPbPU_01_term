!____________________Getting_students_list_by_gender_____________!
if (Stud%Gender == ProcessedGenger) then
   allocate (List, source=Stud)
   List%next => Null()

   if (Associated(Stud%next)) &
        call Get_list_by_gender(Stud%next, List%next, ProcessedGenger)

else if (Associated(Stud%next)) then
   call Get_list_by_gender(Stud%next, List, ProcessedGenger)
else
   List => Null()
end if

!______Recursive_Selection_sort_by_Year_Of_Birth_and_Surname_____!
if (Associated(unsorted)) then
   call Choose_And_Paste(unsorted, unsorted, unsorted%next)
   call Sort_Group_List(unsorted%next)
end if

!______Choose_and_paste_at_the_head_the_maximum_element__________!
if (Associated(current)) then
   if (Swap(current, maximum)) then
      call Choose_And_Paste(unsorted, current, current%next)
   else
      call Choose_And_Paste(unsorted, maximum, current%next)
   end if
else
   if (.not. Associated(unsorted, maximum)) then
      tmp_student => maximum
      maximum => maximum%next
      tmp_student%next => unsorted
      unsorted => tmp_student
   end if
end if

!________________Two_elemets_comparison_by_all_fields____________!
Swap = .false.
if (current%YOB < maximum%YOB) then
   Swap = .true.
else if (current%YOB == maximum%YOB) then
   if (current%Surname < maximum%Surname) then
      Swap = .true.
   else if (current%Surname==maximum%Surname .and. &
        current%Initials < maximum%Initials) then
      Swap = .true.
   end if
end if
end function Swap
