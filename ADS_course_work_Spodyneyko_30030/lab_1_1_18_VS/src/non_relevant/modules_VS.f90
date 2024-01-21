module modules_IO
  use Environment

  implicit none

  contains
    subroutine Read_group_list(Input_File, Surnames, Initials, YOB)
      character(*)                         Input_File
      character(STUD_AMOUNT, kind=CH_)     Surnames(:), Initials(:)
      integer                              YOB(:)
      intent(in)                           Input_File
      intent(out)                          Surnames, Initials, YOB

      Integer In, IO, i
      character(:), allocatable         :: format
      
      ! Чтение списка группы: фамилии, инициалы, год рождения
      open (file=input_file, encoding=E_, newunit=In)
         format = '(2(a, 1x), i4)'
         read (In, format, iostat=IO) (Surnames(i), Initials(i), YOB(i), i = 1, STUD_AMOUNT)
         call Handle_IO_status(IO, "reading group list")
      close (In)

    end subroutine Read_group_list

    ! Вывод списка группы.
    subroutine Output_group_list(Output_File, Surnames, Initials, YOB, List_name)
      character(*)                         Output_file
      character(STUD_AMOUNT, kind=CH_)     Surnames(:), Initials(:)
      integer                              YOB(:)
      intent(in)                           Output_File, Surnames, Initials, YOB

      integer Out, IO, i
      character(:), allocatable         :: format

      open (file=Output_file, encoding=E_, newunit=Out)
         write (out, '(/a)') List_name
         format = '(2(a, 1x), i4)'
         write (Out, format, iostat=IO) (Surnames(i), Initials(i), YOB(i), i = 1, STUD_AMOUNT)
         call Handle_IO_status(IO, "writing " // List_name)
      close (Out)
    end subroutine Output_group_list

end module modules_IO

   ! ! Составление логической маски, соответствующей юношам.
   ! Is_A_Boy       = Gender == MALE ! Gender == CH__"М" в некоторых компиляторах может пока не поддерживаться.
   ! Boys_Amount    = Count(Is_A_Boy)
   
   ! ! Получение массивов, связынных с юношами.
   ! ! 1-ый способ. Использование массива номеров юношей в списке.
   ! Boys_Pos   = Pack(INDEXES, Is_A_Boy) ! == [1, 2, 3]
   ! allocate (Boys_Surnames(Boys_Amount), Boys_Initials(Boys_Amount), &
   !    Boys_Marks(Boys_Amount, MARKS_AMOUNT))
   ! do concurrent (i = 1:Boys_Amount)
   !    ! Получение списков юношей.
   !    Boys_Surnames(i)  = Surnames(Boys_Pos(i))
   !    Boys_Initials(i)  = Initials(Boys_Pos(i))
   !    Boys_Marks(i, :)  = Marks(Boys_Pos(i), :)
   ! end do
   
   ! ! 2-ой способ. Использование двумерной маски, накладываемой на массив оценок.
   ! ! ! Получение списков юношей.
   ! ! Boys_Surnames  = Pack(Surnames, Is_A_Boy)
   ! ! Boys_Initials  = Pack(Initials, Is_A_Boy)
   ! ! ! Получение двумерного списка оценок юношей:
   ! ! ! 1. Для получение двумерного списка оценок необходима двумерная маска, захватывающая все оценки юношей:
   ! ! ! Spread(Is_A_Boy, 2, MARKS_AMOUNT)
   ! ! ! 2. По такой маске Pack вернёт одномерный массив со всеми оценками юношей, который
   ! ! ! необходимо будет переформировать в двумерный массив размером: [Boys_amount, MARKS_AMOUNT].
   ! ! Boys_Marks     = Reshape( Pack(Marks, Spread(Is_A_Boy, 2, MARKS_AMOUNT)), [Boys_amount, MARKS_AMOUNT])
   
   ! ! Вычисление средней оценки для юношей. Вне цикла для векторизации.
   ! Boys_Aver_Marks   = Sum(Boys_Marks, dim=2) / Real(MARKS_AMOUNT, R_)

   ! Is_A_Girl      = .not. Is_A_Boy
   ! Girls_Amount   = STUD_AMOUNT - Boys_Amount
   
   ! ! Получение массивов, связынных с девушками.
   ! Girls_Pos   = Pack(INDEXES, Is_A_Girl) ! == [4, 5]
   ! allocate (Girls_Surnames(Girls_Amount), Girls_Initials(Girls_Amount), &
   !    Girls_Marks(Girls_Amount, MARKS_AMOUNT))
   ! do concurrent (i = 1:Girls_Amount)
   !    ! Получение списков девушек.
   !    Girls_Surnames(i)  = Surnames(Girls_Pos(i))
   !    Girls_Initials(i)  = Initials(Girls_Pos(i))
   !    Girls_Marks(i, :)  = Marks(Girls_Pos(i), :)
   ! end do
      
   ! ! Вычисление средней оценки для девушек. Вне цикла для векторизации.
   ! Girls_Aver_Marks = Sum(Girls_Marks, dim=2) / Real(MARKS_AMOUNT, R_)

   ! ! Сортировка списка юношей по среднему баллу методом пузырька.
   ! do i = Boys_amount, 2, -1
   !    ! Просматриваем список с начала, ставя в конец менее успешного.
   !    do j = 1, i-1
   !       Swap = .false.
   !       ! Проверка на то, стоит ли менять учащихся местами.
   !       if (Boys_Aver_Marks(j) < Boys_Aver_Marks(j+1)) then
   !          Swap = .true.
   !       else if (Boys_Aver_Marks(j) == Boys_Aver_Marks(j+1)) then
   !          if (Boys_Surnames(j) > Boys_Surnames(j+1)) then
   !             Swap = .true.
   !          else if (Boys_Surnames(j)==Boys_Surnames(j+1) .and. Boys_Initials(j)>Boys_Initials(j+1)) then
   !             Swap = .true.
   !          end if
   !       end if
         
   !       if (Swap) then
   !          tmpSurname           = Boys_Surnames(j+1)
   !          Boys_Surnames(j+1)   = Boys_Surnames(j)
   !          Boys_Surnames(j)     = tmpSurname
	! 		! Boys_Surnames(j+1:j:-1) = Boys_Surnames(j:j+1)
	! 		! Boys_Surnames([j, j+1]) = Boys_Surnames([j+1, j])

   !          tmpInitials          = Boys_Initials(j+1)
   !          Boys_Initials(j+1)   = Boys_Initials(j)
   !          Boys_Initials(j)     = tmpInitials

   !          tmpMarks             = Boys_Marks(j+1, :)
   !          Boys_Marks(j+1, :)   = Boys_Marks(j, :)
   !          Boys_Marks(j, :)     = tmpMarks

   !          tmpAverMark          = Boys_Aver_Marks(j+1)
   !          Boys_Aver_Marks(j+1) = Boys_Aver_Marks(j)
   !          Boys_Aver_Marks(j)   = tmpAverMark
   !       end if
   !    end do
   ! end do

   ! ! Сортировка списка девушек по среднему баллу методом пузырька.
   ! do i = Girls_Amount, 2, -1
   !    ! Просматриваем список с начала, ставя в конец менее успешного.
   !    do j = 1, i-1
   !       Swap = .false.
   !       ! Проверка на то, стоит ли менять учащихся местами.
   !       if (Girls_Aver_Marks(j) < Girls_Aver_Marks(j+1)) then
   !          Swap = .true.
   !       else if (Girls_Aver_Marks(j) == Girls_Aver_Marks(j+1)) then
   !          if (Girls_Surnames(j) > Girls_Surnames(j+1)) then
   !             Swap = .true.
   !          else if (Girls_Surnames(j)==Girls_Surnames(j+1) .and. Girls_Initials(j)>Girls_Initials(j+1)) then
   !             Swap = .true.
   !          end if
   !       end if

   !       if (Swap) then
   !          tmpSurname           = Girls_Surnames(j+1)
   !          Girls_Surnames(j+1)   = Girls_Surnames(j)
   !          Girls_Surnames(j)     = tmpSurname

   !          tmpInitials          = Girls_Initials(j+1)
   !          Girls_Initials(j+1)   = Girls_Initials(j)
   !          Girls_Initials(j)     = tmpInitials

   !          tmpMarks             = Girls_Marks(j+1, :)
   !          Girls_Marks(j+1, :)   = Girls_Marks(j, :)
   !          Girls_Marks(j, :)     = tmpMarks

   !          tmpAverMark          = Girls_Aver_Marks(j+1)
   !          Girls_Aver_Marks(j+1) = Girls_Aver_Marks(j)
   !          Girls_Aver_Marks(j)   = tmpAverMark
   !       end if
   !    end do
   ! end do

   ! ! Вывод отсортированного списка юношей со средним баллом.
   ! open (file=output_file, encoding=E_, position='append', newunit=Out)
   !    write (out, '(/a)') "Успеваемость юношей:"
   !    write (Out, format, iostat=IO) &
   !       (Boys_Surnames(i), Boys_Initials(i), "М", Boys_Marks(i, :), Boys_Aver_Marks(i), i = 1, Boys_Amount)
   ! close (Out)
   ! ! Обработка статуса записи.
   ! Out = OUTPUT_UNIT
   ! open (Out, encoding=E_)
   ! select case(io)
   !    case(0)
   !    case(IOSTAT_END)
   !       write (Out, '(a)') "End of file has been reached while writing sorted boys list."
   !    case(1:)
   !       write (Out, '(a)') "Error while writing sorted boys list: ", io
   !    case default
   !       write (Out, '(a)') "Undetermined error has been reached while writing sorted boys list: ", io
   ! end select
   
   !    ! Вывод отсортированного списка девушек со средним баллом.
   ! open (file=output_file, encoding=E_, position='append', newunit=Out)
   !    write (out, '(/a)') "Успеваемость девушек:"
   !    write (Out, format, iostat=IO) (Girls_Surnames(i), Girls_Initials(i), "Ж", Girls_Marks(i, :), &
   !       Girls_Aver_Marks(i), i = 1, Girls_Amount)
   ! close (Out)
   ! ! Обработка статуса записи.
   ! Out = OUTPUT_UNIT
   ! open (Out, encoding=E_)
   ! select case(io)
   !    case(0)
   !    case(IOSTAT_END)
   !       write (Out, '(a)') "End of file has been reached while writing sorted girls list."
   !    case(1:)
   !       write (Out, '(a)') "Error while writing sorted girls list: ", io
   !    case default
   !       write (Out, '(a)') "Undetermined error has been reached while writing sorted girls list: ", io
   ! end select
