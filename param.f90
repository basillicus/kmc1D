module param
  use strings
  implicit none

    integer :: kmc !> #Step of the simulation
!... No_kmc - number of KMC steps
    integer :: No_kmc
    integer :: m  !> Index of each move
!... Max. number of states
    integer, parameter :: total_states=2

    integer :: direction !> Direction of the moves:: 1: right; -1: left
    integer :: previous_direction !> If direction=previous_direction -> molecule moves
    integer :: current_state
    integer :: distance !> Distance diffused by the molecule

!  Time variables
    real*8  :: time, dt
!... useful global-types
    real*8, parameter :: tiny=0.000001
    real*8  :: real_seed
    integer :: seed, iPrnt
    integer :: luo=10 !> Logic Unit Output

!... strings for working with input
    character :: Line*200
    integer   :: LinEnd(100), LinPos(100), NumLin, iErr

!... Statistics variables
    logical   :: do_statistics   ! If perform statistics
    integer   :: freq_statistics ! Perform statistics every this steps

!... elementary barriers (in eV) and inverse temperature (in 1/eV), temperature (K)
    real*8  :: elem_barrier(total_states*2)
    real*8  :: prefactors(total_states*2), pref
    real*8 rat(10)
    real*8  :: temperature
    real*8  :: beta

    real*8,parameter :: Boltzm = 8.617343e-5  ! in eV/K

 
  CONTAINS

  subroutine read_input()
!..............................................................................
!....... read input info ......................................................
!..............................................................................
    integer i,l0
    character cha*10
!
    open(1,file='input.dat', form='formatted')

!
!_________________ No of KMC steps
!
    call find_string('number_of_kmc_steps',19,Line,1,.true.,iErr)
    if(iErr.ne.0) go to 10
    call CutStr(Line,NumLin,LinPos,LinEnd,0,0,iErr)
    if(NumLin.lt.2) go to 10
    read(Line(LinPos(2):LinEnd(2)),*,err=10) No_kmc
    write(9,'(a,i10)') '... Number of KMC steps found = ',No_kmc
    write(cha,'(i10)') No_kmc
    ! call posit(cha,10,l0)
    ! len_No_kmc=10-l0+1
! !
! !_________________ if to do testing
! !
!     testing=.false.
!     call find_string('do_testing',10,Line,1,.true.,iErr)
!     if(iErr.eq.0) testing=.true.
!     write(9,'(a,l1)') '... TESTING = ',testing
!     ! TO CHECK: If read correctly the barriers
!
!_________________ elementary barrier (in eV)
!
    elem_barrier=0.0
    call find_string('barriers',8,Line,1,.true.,iErr)
    if(iErr.ne.0) go to 10
    call CutStr(Line,NumLin,LinPos,LinEnd,0,0,iErr)
        if(NumLin.lt.4) go to 10
    read(Line(LinPos(2):LinEnd(5)),*,err=10) (elem_barrier(i),i=1,4)
!
!_________________ prefactors (in eV)
!
    prefactors=0.0
    call find_string('prefactors',10,Line,1,.true.,iErr)
    if(iErr.ne.0) go to 10
    call CutStr(Line,NumLin,LinPos,LinEnd,0,0,iErr)
    if(NumLin.lt.4) go to 10
    read(Line(LinPos(2):LinEnd(5)),*,err=10) (prefactors(i),i=1,4)
!
!_________________ temperature 
!
    call find_string('temperature',11,Line,1,.true.,iErr)
    if(iErr.ne.0) go to 10
    call CutStr(Line,NumLin,LinPos,LinEnd,0,0,iErr)
    if(NumLin.lt.2) go to 10
    read(Line(LinPos(2):LinEnd(2)),*,err=10) temperature
    beta=1.0/(Boltzm*temperature)
! !
! !________ prefix to the rate, i.e. the exp prefactor to the rate:
! !         - in inverse ps (1 ps=10^{-12} s)
!     pref= +1.0 * log(10.0)
!
!...................... RATES TABLE ...............................................................
!
    do i=1,4
       pref= +1.0 * log(prefactors(i))
       rat(i)=exp(-beta*elem_barrier(i)+pref)
    end do
    ! rat(8)=depos_rate   ;    elem_barrier(8)=0.0d0
    write(9,'(/a)')'============|  Barriers (in Ev)  and Rates (in ps^-1) |============'
    write(9,'(a,f10.3,a)') '   Temperature = ',temperature,' K'
    write(9,'(a,f10.5,x,e12.6)') ' 1. State 1 to the right:',elem_barrier(1),rat(1)
    write(9,'(a,f10.5,x,e12.6)') ' 2. State 1 to the left :',elem_barrier(2),rat(2)
    write(9,'(a,f10.5,x,e12.6)') ' 3. State 2 to the right:',elem_barrier(3),rat(3)
    write(9,'(a,f10.5,x,e12.6)') ' 4. State 2 to the left :',elem_barrier(4),rat(4)
    write(*,'(/a)')'============|  Barriers (in Ev)  and Rates (in ps^-1) |============'
    write(*,'(a,f10.3,a)') '   Temperature = ',temperature,' K'
    write(*,'(a,f10.5,x,e12.6,x,e12.6)') ' 1. State 1 to the right:',elem_barrier(1),rat(1), prefactors(1)
    write(*,'(a,f10.5,x,e12.6,x,e12.6)') ' 2. State 1 to the left :',elem_barrier(2),rat(2), prefactors(2)
    write(*,'(a,f10.5,x,e12.6,x,e12.6)') ' 3. State 2 to the right:',elem_barrier(3),rat(3), prefactors(3)
    write(*,'(a,f10.5,x,e12.6,x,e12.6)') ' 4. State 2 to the left :',elem_barrier(4),rat(4), prefactors(4)
!     write(9,'(a,f10.5,x,e12.6)') ' 5. ',elem_barrier(5),rat(5)
!     write(9,'(a,f10.5,x,e12.6)') ' 6. ',elem_barrier(6),rat(6)
!     write(9,'(a,f10.5,x,e12.6)') ' 8. ',elem_barrier(8),rat(8)
    write(9,'(a/)')'==================================================================='
!
!________________ level of print: 0 - very little; 5 - a lot
! 
   iPrnt=0
    call find_string('printing',8,Line,1,.true.,iErr)
    if(iErr.eq.0) then
       call CutStr(Line,NumLin,LinPos,LinEnd,0,0,iErr)
       if(NumLin.lt.2) go to 10
       read(Line(LinPos(2):LinEnd(2)),*,err=10) iPrnt
    end if
    write(9,'(a,i1)')'... Printing level = ',iPrnt
!
!_________________ if perform statistics
    do_statistics = .false.
    call find_string('do_statistics',13,Line,1,.true.,iErr)
    if(iErr.eq.0) do_statistics=.true.
    write(9,'(a,l1)') '... write statistics = ',do_statistics 

    freq_statistics = 1000
    call find_string('freq_statistics',15,Line,1,.true.,iErr)
    if(iErr.eq.0) then
       call CutStr(Line,NumLin,LinPos,LinEnd,0,0,iErr)
       if(NumLin.lt.2) go to 10
       read(Line(LinPos(2):LinEnd(2)),*,err=10) freq_statistics
    end if
    write(9,'(a,i7,a)')'... Heavy statistics perform every  = ',freq_statistics, ' kmc steps'
!
!___________ Choose fixed or random seed for random number generation
    seed=0
    call find_string('seed',4,Line,1,.true.,iErr)
    if(iErr.eq.0) then
       call CutStr(Line,NumLin,LinPos,LinEnd,0,0,iErr)
       if(NumLin.lt.2) go to 10
       read(Line(LinPos(2):LinEnd(2)),*,err=10) seed
    end if
    if (seed > 0 ) then 
        seed = -seed ! Seed has to be a negative integer
        write(9,'(a,i6)')'... Seed used for Random Number Generation = ', -seed
    else 
!       Change the seed for random numbers generation
        call init_random_seed()        ! initialise seed using the clock
        call random_number(real_seed)  ! Get a real random number (0.0-1.0)
        seed = -int(real_seed*10000)-1 ! Convert real_seed to integer between [1? o 0? y 10000]
        write(9,'(a,i6)')'... Random seed used for Random Number Generation = ', -seed
    end if 
!
    close (1)
    write(*,'(/a/)')'... Input file has been read in successfully ...'
    write(9,'(/a/)')'... Input file has been read in successfully ...'

    return
!................ errors in reading input
10  write(*,*)'FATAL errors in reading the input file!'
    stop 'FULL STOP!'
  end subroutine read_input

end module param
