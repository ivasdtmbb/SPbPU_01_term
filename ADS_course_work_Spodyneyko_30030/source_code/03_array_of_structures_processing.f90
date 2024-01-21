!____________________Getting_students_list_by_gender_____________!
Boys  = Pack(Group, Group%Gender == MALE)
Girls = Pack(Group, Group%Gender == FEMALE)

!_______________Selection_sort_by_Year_Of_Birth_and_Surname______!
do j = 1, Size(Group) - 1
   youngest_stud = j
   do k = j + 1, Size(Group)
      if (Swap(Group, k, youngest_stud)) then
         youngest_stud = k
      end if
   end do
   Group([j, youngest_stud]) = Group([youngest_stud, j])
end do

!________________Two_elemets_comparison_by_all_fields____________!
Swap = .false.
if (Group(i)%YOB < Group(j)%YOB) then
   Swap = .true.
else if (Group(i)%YOB == Group(j)%YOB) then
   if (Group(i)%Surname < Group(j)%Surname) then
      Swap = .true.
   else if (Group(i)%Surname==Group(j)%Surname .and. &
        Group(i)%Initials < Group(j)%Initials) then
      Swap = .true.
   end if
end if
