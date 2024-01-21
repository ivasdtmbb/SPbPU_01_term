module Group_Process

   use Environment
   use Group_IO

   implicit none

contains
   ! Получение списков по полу.
   pure recursive subroutine Get_List_By_Gender(Stud, List, ProcessedGenger)
      type(student), intent(in)        :: Stud
      type(student), pointer           :: List
      character(kind=CH_), intent(in)  :: ProcessedGenger
     
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
   end subroutine Get_list_by_gender
 
  !-------------------------------------------------------------------------!
   pure recursive subroutine Sort_Group_List(unsorted)
      type(student), pointer, intent(inout)  :: unsorted

      if (Associated(unsorted)) then
         call Choose_And_Paste(unsorted, unsorted, unsorted%next)
         call Sort_Group_List(unsorted%next)
      end if
    end subroutine Sort_Group_List

  !-------------------------------------------------------------------------!
   pure recursive subroutine Choose_And_Paste(unsorted, maximum, current)
      type(student), pointer  :: unsorted, maximum, current
      type(student), pointer  :: tmp_student

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
   end subroutine Choose_And_Paste

  !-------------------------------------------------------------------------!
   ! Проверка того, стоит ли менять местами учащихся
   pure logical function Swap(current, maximum)
      type(student), pointer  :: current, maximum

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

end module Group_process
