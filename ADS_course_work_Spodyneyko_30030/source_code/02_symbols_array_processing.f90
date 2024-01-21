!____________________Getting_students_list_by_gender____________!
! Logical mask corresponding to the gender
Is_A_Gender  = Genders == Gender
Gender_Amount = Count(Is_A_Gender)

! Gender arrays
INDEXES = [(i, i = 1, STUD_AMOUNT)]
Gender_Pos = Pack(INDEXES, Is_A_Gender)
allocate (Gender_Surnames(SURNAME_LEN, Gender_Amount), &
     Gender_Initials(INITIALS_LEN, Gender_Amount), &
     Gender_YOB(Gender_Amount))
! Data matrix for a gender.
do concurrent (i = 1:Gender_Amount)
   Gender_Surnames(:, i)  = Surnames(:, Gender_Pos(i))
   Gender_Initials(:, i)  = Initials(:, Gender_Pos(i))
   Gender_YOB(i)  = YOB(Gender_Pos(i))
end do

!_______________Selection_sort_by_Year_Of_Birth_and_Surname______!
do j = 1, Size(YOB) - 1
   minInd = j
   do k = j + 1, Size(YOB)
      if (Swap(YOB, Surnames, Initials, k, minInd)) then
         minInd = k
      end if
   end do
   call Swap_elements(Surnames, Initials, YOB, j, minInd)
end do

!________________Two_elemets_comparison_by_all_fields____________!
Swap = .false.
if (YOB(i) < YOB(j)) then
   Swap = .true.
else if (YOB(i) == YOB(j)) then
   if (GT(Surnames(:, j), Surnames(:, i))) then
      Swap = .true.
   else if (ALL(Surnames(:, j) == Surnames(:, i)) &
        .and. GT(Initials(:, j), Initials(:, i))) then
      Swap = .true.
   end if
end if

!________________Compare_two_character_arrays____________________!
! Searching the first differing symbol or stop at the last one
do  i = 1, Min(Size(arr1), Size(arr2)) - 1
   if (arr1(i) /= arr2(i)) &
        exit
end do
GT = arr1(i) > arr2(i)

!________________Swap_two_elements_______________________________!
tmpSurname  = Surnames(:, i)
Surnames(:, i) = Surnames(:, j)
Surnames(:, j) = tmpSurname

tmpInitials = Initials(:, i)
Initials(:, i) = Initials(:, j)
Initials(:, j) = tmpInitials

YOB([i, j]) = YOB([j, i])
