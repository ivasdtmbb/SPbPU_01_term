!____________________Getting_students_list_by_gender____________!
Boys  = Pack(Group, Group%Gender == MALE)
Girls = Pack(Group, Group%Gender == FEMALE)

!______Recursive_Selection_sort_by_Year_Of_Birth_and_Surname_____!
youngest_stud = Find_youngest(Group, N, 1, N)
! Put the younges at the array's tail position
Group([N, youngest_stud]) = Group([youngest_stud, N])

if (N >= 2) &
     call Sort_class_list(Group, N-1)

!_____________________Find_the_youngest_person___________________!
if (j == N) then
   res = youngest
else if (Swap(Group, j, youngest)) then
   res = Find_youngest(Group, j, j+1, N)
else
   res = Find_youngest(Group, youngest, j+1, N)
end if

!________________Two_elemets_comparison_by_all_fields____________!
Swap = .false.
if (Group(i)%YOB > Group(j)%YOB) then
   Swap = .true.
else if (Group(i)%YOB == Group(j)%YOB) then
   if (Group(i)%Surname > Group(j)%Surname) then
      Swap = .true.
   else if (Group(i)%Surname==Group(j)%Surname .and. &
        Group(i)%Initials > Group(j)%Initials) then
      Swap = .true.
   end if
end if
