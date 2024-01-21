module Source_Process

   use Environment
   use Source_IO
   
   implicit none

contains

!----------------------------------------------------------------------------!
  pure recursive function Get_Same_File_Names(Mask, FN_ForCheck) result(FN_Correct)
     type(FileName), pointer      :: FN_Correct
     type(FileName), intent(in)   :: Mask, FN_ForCheck

     ! Если маска подходит, создаём новый экземпляр (записываем подходящее имя файла)
        if (Compare_FN_Fields(Mask, FN_ForCheck)) then
           allocate (FN_Correct)
           FN_Correct%Name      = FN_ForCheck%Name
           FN_Correct%Extension = FN_ForCheck%Extension

     ! Если ещё остались имена файлов, то сравниваем их с маской
           if (Associated(FN_ForCheck%Next)) then
              FN_Correct%Next => Get_Same_File_Names(Mask, FN_ForCheck%Next)
           end if

     ! Если маска не подходит, проверяем следующее имя файла
        else if (Associated(FN_ForCheck%Next)) then
           FN_Correct => Get_Same_File_Names(Mask, FN_ForCheck%Next)
        else
           FN_Correct => Null()
        end if

   end function Get_Same_File_Names

!----------------------------------------------------------------------------!
   ! По очереди проверяем поля имени и расширения
   pure logical  function Compare_FN_Fields(Mask, FN_ForCheck)
     type(FileName), intent(in)   :: Mask, FN_ForCheck

     Compare_FN_Fields = .false.
     if (Compare_Fields(Mask%Name, FN_ForCheck%Name)) then
        if (Compare_Fields(Mask%Extension, FN_ForCheck%Extension)) then
           Compare_FN_Fields = .true.
        end if
     end if
   end function Compare_FN_Fields

!----------------------------------------------------------------------------!
   pure logical function Compare_Fields(Mask_Field, FN_Field)
     character(:, kind=CH_), allocatable, intent(in)       :: Mask_Field
     character(:, kind=CH_), allocatable, intent(in)       :: FN_Field
     integer  :: AsteriskIndex, MaskLength

     Compare_Fields = .false.
     AsteriskIndex = Index(Mask_Field, Asterisk)
     MaskLength = len(Mask_Field)
     
     ! Если нет '*', то сравниваем поля
     if (AsteriskIndex == 0) then
        Compare_Fields = (Mask_Field == FN_Field)

     ! Если '*' стоит в начале строки, то проверяем строку после неё
     else if (AsteriskIndex == 1) then
        if (MaskLength > 1) then
           Compare_Fields = (Index(FN_Field, Mask_Field(2 : MaskLength)) /= 0)
        else
           Compare_Fields = .true.
        end if

     ! Если '*' стоит не в начале, то сравниваем левую часть строк
     else
        Compare_Fields = (FN_Field(1 : (AsteriskIndex-1)) == Mask_Field(1 : (AsteriskIndex-1)))
     end if
   end function Compare_Fields

 end module Source_process
