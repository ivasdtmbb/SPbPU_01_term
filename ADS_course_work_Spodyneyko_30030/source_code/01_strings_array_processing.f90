!____________________Getting_students_list_by_gender____________!
! Gender Logical mask
Is_A_Gender  = Genders == Gender
Gender_Amount = Count(Is_A_Gender)

! Gender arrays
allocate (Gender_Surnames(Gender_Amount), &
     Gender_Initials(Gender_Amount), Gender_YOB(Gender_Amount))

Gender_Surnames = Pack(Surnames, Is_A_Gender)
Gender_Initials = Pack(Initials, Is_A_Gender)
Gender_YOB      = Pack(YOB, Is_A_Gender)

!_______________Selection_sort_by_Year_Of_Birth_and_Surname______!
do j = 1, Size(YOB) - 1
   minInd = j
   do k = j + 1, Size(YOB)
      if (Swap(YOB, Surnames, Initials, k, minInd)) then
         minInd = k
      end if
   end do
   if (minInd /= j) then
      ! Swap two elements
      Surnames([j, minInd]) = Surnames([minInd, j])
      Initials([j, minInd]) = Initials([minInd, j])
      YOB([j, minInd]) = YOB([minInd, j])
   end if
end do

!________________Two_elemets_comparison_by_all_fields____________!
Swap = .false.

if (YOB(i) < YOB(j)) then
   Swap = .true.
else if (YOB(i) == YOB(j)) then
   if (Surnames(i) < Surnames(j)) then
      Swap = .true.
   else if (Surnames(i) == Surnames(j)) then
      if (Initials(i) < Initials(j)) then
         Swap = .true.
      end if
   end if
end if
