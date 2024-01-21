!____________________Getting_students_list_by_gender____________!
Is_A_Gender = Group%Genders == Gender
genderAmount = Count(Is_A_Gender)

Sub_group%Surnames = Pack(Group%Surnames, Is_a_gender)
Sub_group%Initials = Pack(Group%Initials, Is_a_gender)
Sub_group%Genders = Pack(Group%Genders, Is_a_gender)
Sub_group%YOBs = Pack(Group%YOBs, Is_a_gender)

!______Recursive_Selection_sort_by_Year_Of_Birth_and_Surname_____!
youngest_stud = Find_youngest(Group, N, 1, N)
! Put the younges at the array's tail position
Group%Surnames([N, youngest_stud]) = Group%Surnames([youngest_stud, N])
Group%Initials([N, youngest_stud]) = Group%Initials([youngest_stud, N])
Group%Genders([N, youngest_stud])  = Group%Genders([youngest_stud, N])
Group%YOBs([N, youngest_stud])     = Group%YOBs([youngest_stud, N])

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
if ((Group%YOBs(i)) > (Group%YOBs(j))) then
   Swap = .true.
else if (Group%YOBs(i) == Group%YOBs(j)) then
   if (Group%Surnames(i) > Group%Surnames(j)) then
      Swap = .true.
   else if (Group%Surnames(i) == Group%Surnames(j) .and. &
        Group%Initials(i) < Group%Initials(j)) then
      Swap = .true.
   end if
end if
