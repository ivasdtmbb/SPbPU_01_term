!____________________Symbols_array_declaration___________________!
implicit none
character(:), allocatable         :: input_file, output_file

integer                           :: STUD_AMOUNT
integer, parameter                :: SURNAME_LEN  = 15
integer, parameter                :: INITIALS_LEN = 5
character(kind=CH_), parameter    :: MALE = Char(1052, CH_), &
                                     FEMALE = Char(1046, CH_)
! The matrixes (Surnames(:,:), Initials(:,:)) data stored in lines:
! original          : Matrix(m, n)
! stored and indexed: Matrix(n, m)
character(kind=CH_), allocatable :: Surnames(:, :), &
                                    Boys_Surnames(:, :), &
                                    Girls_Surnames(:, :)
character(kind=CH_), allocatable :: Initials(:, :), &
                                    Boys_Initials(:, :), &
                                    Girls_Initials(:, :)
character(kind=CH_), allocatable :: Genders(:)
integer, allocatable             :: YOB(:), Boys_YOB(:), Girls_YOB(:)

integer, allocatable             :: INDEXES(:), Gender_Pos(:)
integer                          :: Gender_Amount, i
