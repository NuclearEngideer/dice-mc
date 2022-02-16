program roll

  implicit none
  
  character(len=8) :: M_char
  integer(8) :: M
  real(8)    :: rand

  call get_command_argument(1, M_char)
  ! Fortran internal read to convert read character to integer
  read(M_char, '(i8)') M

  call random_number(rand)
  write(*,*) ceiling(M*rand)
    
end program

