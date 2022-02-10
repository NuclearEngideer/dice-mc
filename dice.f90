program roll

  implicit none
  
  integer(4) :: M
  real(8)    :: rand
  
  call random_number(rand)
  write(*,*) ceiling(6*rand)
    
end program

