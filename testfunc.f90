program test

  implicit none

  character(128) :: blah
  read(*,'(A)') blah
  write(*,*) trim(blah)

  write(*,*) dice_mc(2)

  contains

  integer function dice_mc(M) result(roll)
    integer(4), intent(in) :: M
    real(8)                :: rand
    call random_number(rand)
    roll = ceiling(M*1.0)
  end function dice_mc

end program
