!____________________Getting_students_list_by_gender____________!
Is_A_Gender = Group%Genders == Gender
genderAmount = Count(Is_A_Gender)

Sub_group%Surnames = Pack(Group%Surnames, Is_a_gender)
Sub_group%Initials = Pack(Group%Initials, Is_a_gender)
Sub_group%Genders  = Pack(Group%Genders, Is_a_gender)
Sub_group%YOBs     = Pack(Group%YOBs, Is_a_gender)

!_______________Selection_sort_by_Year_Of_Birth_and_Surname______!
do j = 1, Size(Group%Surnames) - 1
   youngest_stud = j
   do k = j + 1, Size(Group%Surnames)
      if (Swap(Group, k, youngest_stud)) then
         youngest_stud = k
      end if
   end do
   ! Swap two elements
   Group%Surnames([j,youngest_stud])=Group%Surnames([youngest_stud,j])
   Group%Initials([j,youngest_stud])=Group%Initials([youngest_stud,j])
   Group%Genders([j,youngest_stud]) =Group%Genders([youngest_stud,j])
   Group%YOBs([j,youngest_stud])    =Group%YOBs([youngest_stud,j])
end do

!________________Two_elemets_comparison_by_all_fields____________!
Swap = .false.
if ((Group%YOBs(i)) < (Group%YOBs(j))) then
   Swap = .true.
else if (Group%YOBs(i) == Group%YOBs(j)) then
   if (Group%Surnames(i) < Group%Surnames(j)) then
      Swap = .true.
   else if (Group%Surnames(i) == Group%Surnames(j) .and. &
        Group%Initials(i) < Group%Initials(j)) then
      Swap = .true.
   end if
end if
