program roll

  implicit none
  
  integer(4), allocatable   :: N, M
  integer(4)                :: i, j, end_index, ii
  integer(4)                :: num_elements, global_total, inner_total
  character(256)            :: inString
  character(:), allocatable :: splitstring(:)
  logical                   :: default_plus
  
  write(*,*) '    _/_/_/    _/_/_/   _/_/_/  _/_/_/            _/      _/     _/_/_/'
  write(*,*) '   _/    _/    _/   _/        _/                _/_/  _/_/   _/'
  write(*,*) '  _/    _/    _/   _/        _/_/_/   _/_/_/   _/  _/  _/   _/'
  write(*,*) ' _/    _/    _/   _/        _/                _/      _/   _/'
  write(*,*) '_/_/_/_/  _/_/_/   _/_/_/  _/_/_/            _/      _/     _/_/_/'
  write(*,*)
  write(*,*) 'Enter in the form of "1d20 + 1d6 - 1d8 x4"'
  write(*,*) 'to roll those dice 4 times'
  write(*,*)
  write(*,*) 'Help with "h"'
  write(*,*) 'Quit with "q"'
  
  100 continue
  default_plus = .false.
  global_total=0
  write(*,*)
  write(*,*) 'Input >'

  ! Read the input from the terminal as a string
  read(*,'(A)') inString

  ! Check if user quit or requested help
  if ( inString .eq. 'q' .or. inString .eq. 'Q' ) then 
    write(*,*) 'Goodbye!'
    call exit(0)
  elseif ( inString .eq. 'h' .or. instring .eq. 'H' .or. instring .eq. 'help' ) then
    write(*,*) 'This program rolls dice as requested by the user.'
    write(*,*) 'WHITESPACE IN YOUR REQUEST IS CRITICAL'
    write(*,*) ''
    write(*,*) 'Requests are made at the "Input >" prompt with the following syntax'
    write(*,*) ''
    write(*,*) '    [+/-] AdB +/- CdE +/- FdG ... +/- XdY [xN]'
    write(*,*) ''
    write(*,*) 'The initial +/- sign is optional.'
    write(*,*) 'If omitted, there should be no whitespace before "AdB"'
    write(*,*) 'Requesting multiple rolls returns independent values from each trial'
    write(*,*) 'and the sum from all trials'
    write(*,*) ''
    write(*,*) 'Allowable inputs:'
    write(*,*) '"1d20 + 1d4 x4" rolls 1d20 and adds 1d4, computes sum of 4 trials'
    write(*,*) '"- 1d8 + 2d2"   rolls 1d8 and subtracts that value from 2d2'
    write(*,*) '"q"             quits program loop'
    write(*,*) '"h" or "help"   prints this message'
    write(*,*) ''
    write(*,*) 'Illegal inputs:'
    write(*,*) '"-1d8"          illegal because operator touches the dice'
    write(*,*) '"1d4+2d20"      illegal because operator touches the dice'
    write(*,*) '"1d6 + 2d4 x 3" illegal because the "x3" is "x 3"'
    write(*,*) ''
  endif

  ! Begin input parsing...

  num_elements = 0
  num_elements = count([(instring(i:i), i=1,len_trim(instring))] .eq. 'd' .or. &
                      &[(instring(i:i), i=1,len_trim(instring))] .eq. 'x' .or. & 
                      &[(instring(i:i), i=1,len_trim(instring))] .eq. '-' .or. & 
                      &[(instring(i:i), i=1,len_trim(instring))] .eq. '+')

  if ( inString(1:1) .ne. '-' .and. &
     & inString(1:1) .ne. '+') then
     default_plus = .true.
  end if
 
  ! Handle bad inputs
  if ( num_elements == 0 ) then
     write(*,*) '########################'
     write(*,*) '# ERROR: Missing Input #'
     write(*,*) '########################'
     write(*,*) 'Enter in the form of "1d20 + 1d6 + 1d8 x4"'
     write(*,*) 'to roll those dice 4 times'
     write(*,*)
     write(*,*) 'Quit with "q"'
     goto 100
  end if

  ! allocate the array to store the values of the input string
  allocate( character(5) :: splitstring( num_elements ) )

  read(instring,*) splitstring
  ! end initial input parsing

  ! Set loop if needed
  j=1
  end_index = size(splitstring)
  if ( splitstring(end_index)(1:1) .eq. 'x' ) then
    j = return_int( splitstring(size(splitstring))(2:)) 
    end_index=size(splitstring)-1
  end if

  do i=1,j
    inner_total=0
     do ii=1,end_index
      ! if the index is an operator, take note of the operator
      ! then read the next index and cycle 
      if ( ii==1 .and. default_plus ) then
        inner_total=inner_total+parse_next(splitstring(ii))
      else if ( splitstring(ii) .eq. '+' ) then
        inner_total=inner_total+parse_next(splitstring(ii+1))
      else if ( splitstring(ii) .eq. '-' ) then
        inner_total=inner_total-parse_next(splitstring(ii+1))
      endif
    enddo
    if ( j > 1 ) then
      write(*,*) '----------------------------------------'
      write(*,*) 'Total for roll', i, 'is', inner_total
      write(*,*) '----------------------------------------'
    endif
    global_total=global_total+inner_total
  enddo

  ! Write final rolls
  write(*,*)
  write(*,*) 'Total of all',j,'rolls is', global_total

  ! Cleanup before next loop
  deallocate(splitstring)
  goto 100

CONTAINS

  integer function return_int(char_int) result(val)
    character(*), intent(in) :: char_int
    read (char_int, '(I10)') val
  end function return_int

  integer function dice_mc(M) result(roll)
    integer(4), intent(in) :: M
    real(8)                :: rand
    call random_number(rand)
    roll = ceiling(M*rand)
  end function dice_mc
    
  integer function parse_next(item) result(total)
    character(5), intent(in) :: item
    integer(4)               :: M, N, i
    integer(4), allocatable  :: trial(:)

    ! Number of dice
    N=return_int(item(1:index(item, 'd')-1))
    ! Value of dice
    M=return_int(item(index(item, 'd')+1:))
    allocate(trial(N))
    do i=1,N
      trial(i) = dice_MC(M)
    end do
    total = sum(trial)
    write(*,*) item, trial, 'total=',total
    deallocate(trial)
  end function parse_next 

end program

