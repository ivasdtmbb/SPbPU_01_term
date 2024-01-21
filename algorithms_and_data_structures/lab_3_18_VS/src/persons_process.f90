module Persons_Process

   use Environment
   use Persons_IO

   implicit none

contains
  !----------------Сортировка_вставками--------------------------------------!
  pure recursive subroutine Insertion_Sort_Persons_List(Persons, lastSorted)
      type(person), allocatable, intent(inout)  :: Persons
      type(person), allocatable, intent(inout)  :: lastSorted
      type(person), allocatable                 :: tmp_person

      if (Allocated(lastSorted%next)) then
         if (lastSorted%next%Surname < lastSorted%Surname) then
            call Move_Alloc(lastSorted%next, tmp_person)
            call Move_Alloc(tmp_person%next, lastSorted%next)
            call Paste(Persons, tmp_person)
            call Insertion_Sort_Persons_List(Persons, lastSorted)
         else
            call Insertion_Sort_Persons_List(Persons, lastSorted%next)
         end if
      end if
    end subroutine Insertion_Sort_Persons_List

  !-------------------------------------------------------------------------!
    pure recursive subroutine Paste(current, itemToInsert)
      type(person), allocatable, intent(inout) :: current, itemToInsert
      type(person) , allocatable               :: tmp
      
      if (itemToInsert%Surname < current%Surname) then
         call Move_Alloc(current, itemToInsert%next)
         call Move_Alloc(itemToInsert, current)

         ! call Move_Alloc(current, tmp)
         ! allocate (current, source=itemToInsert)
         ! call Move_Alloc(tmp, current%next)
      else
         call Paste(current%next, itemToInsert)
      end if

    end subroutine Paste

!-----------------------Удаление_Списка-----------------------------------!
    pure recursive subroutine Purge(current)
      type(person), allocatable, intent(inout) :: current
      type(person), allocatable  :: tmp

      if (Allocated(current)) then
         call Move_Alloc(current%next, tmp)
         call Move_Alloc(tmp, current)
         call Purge(current)
      end if
    end subroutine Purge

 end module Persons_Process

