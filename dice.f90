program roll

  implicit none
  
  integer(4), allocatable   :: N, M
  integer(4)                :: i, ilast, j
  integer(4)                :: num_elements, total
  character(256)            :: inString
  character(:), allocatable :: splitstring(:)
  logical                   :: default_plus = .false.
  
  write(*,*) '    _/_/_/    _/_/_/   _/_/_/  _/_/_/            _/      _/   _/_/_/'
  write(*,*) '   _/    _/    _/   _/        _/                _/_/  _/_/   _/'
  write(*,*) '  _/    _/    _/   _/        _/_/_/   _/_/_/   _/  _/  _/   _/'
  write(*,*) ' _/    _/    _/   _/        _/                _/      _/   _/'
  write(*,*) '_/_/_/_/  _/_/_/   _/_/_/  _/_/_/            _/      _/     _/_/_/'
  write(*,*)
  write(*,*) 'Enter in the form of "1d20 + 1d6 - 1d8 x4"'
  write(*,*) 'to roll those dice 4 times'
  write(*,*)
  write(*,*) 'Quit with "q"'
  
  100 continue
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

  do i=1,len_trim(instring)
    ! If no sign preceeding first NdM, must be appended later and array needs to be +1
    if ( i == 1 .and. inString(i:i) .ne. '-' .or. &
       & i == 1 .and. inString(i:i) .ne. '1' .or. &
       & i == 1 .and. inString(i:i) .ne. '2' .or. &
       & i == 1 .and. inString(i:i) .ne. '3' .or. &
       & i == 1 .and. inString(i:i) .ne. '4' .or. &
       & i == 1 .and. inString(i:i) .ne. '5' .or. &
       & i == 1 .and. inString(i:i) .ne. '6' .or. &
       & i == 1 .and. inString(i:i) .ne. '7' .or. &
       & i == 1 .and. inString(i:i) .ne. '8' .or. &
       & i == 1 .and. inString(i:i) .ne. '9' .or. &
       & i == 1 .and. inString(i:i) .ne. '0' ) then
       num_elements = num_elements + 1
       default_plus = .true.
    end if
    ! First, read the number of elements in the input string
    if ( inString(i:i) .eq. ' ') then
      if ( inString(i+1:i+1) .eq. ' ' ) then
        cycle
      end if
    num_elements = num_elements + 1
    end if
  end do
 
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
  allocate( character(5) :: splitstring( num_elements + 1 ) )

  ilast = 1
  j = 1
  do i=1,len_trim(instring)+1
    ! Parse the user's input string:
    ! step through the string, appending non-whitespace values to appropriate arrays
    if ( inString(i:i) .eq. ' ' ) then
      splitstring(j) = inString(ilast:i-1)
      ilast=i+1
      j=j+1
    end if 
  enddo

  ! end initial input parsing


  ! Begin initial loop by reading the last value but only if last element starts with x
  if ( splitstring( size( splitstring ) )(1:1) .eq. 'x' ) then
    total=0
    do i=1, return_int(trim(splitstring(size(splitstring))(2:)))
      write(*,*) 'Roll set ', i
      ! loop over everything else in the input array thing
      do i=1, splitstring( size( splitstring ) - 1 )
        ! Check if the first item in the input array is an arithmetic operator
        ! If so, pass the appropriate sign to the next item function

        ! TODO the logic should check that there's a sign or not in front of the first d term
        ! if so, we can step through with slices of 2. If not, we need to default a + sign
        ! then read the first term, then read 2nd/3rd, then 4th/5th, etc
        ! if we further break the splitstring down from "1d20 + 1d4 - 1d8f x4"
        ! to "+ 1 d 2 0 + 1 d 4 - 1 d 8 x 4", it would be pretty easy to parse thru triplets
        ! rework input parsing maybe? we can strip whitespace somehow
        ! maybe keep the input parse as it is, but add logic to pre-allocation to check if the first non-white space char
        ! is an operator or a number, then if it's not a negative sign, default the assignment after allocating array to +,
        ! then can feed "triplets" to the roller (still have to split around the d)
        if (i==1 .and. len_trim(splitstring(1)) == 1 ) then
          total = total + parse_next(
          
      total = total + parse_next(  
    enddo
  endif

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
    
  integer function parse_next(item,sign) result(item_val)
    character(5), intent(in) :: item


end program

