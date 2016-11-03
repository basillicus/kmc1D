module param
  use strings
  implicit none

    integer :: kmc !> #Step of the simulation
!... No_kmc - number of KMC steps
    integer :: No_kmc
    integer :: m  !> Index of each move
!... Max. number of states the code can handle so far
    integer, parameter :: total_states=2 
    integer :: kind_of_PES=1  !> Defines how many intermediates the PES has

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
    integer :: lui=1   !> Logic Input Unit
    integer :: luo0=9  !> Logic Units Output (log file)
    integer :: luo99=99  !> Logic Units Output (debug file)
    integer :: luo=10  !> Logic Units Output
    integer :: luo2=11 !> Logic Unit Output (for time intervals)

!... strings for working with input
    character :: Line*200
    integer   :: LinEnd(100), LinPos(100), NumLin, iErr

!... Statistics variables
    logical   :: do_statistics   !> If to perform statistics
    integer   :: freq_statistics !> Perform statistics every this steps
    integer   :: freq_writing, writing_state=0    !> Write KMC state every freq_writing steps
    real*8    :: time_interval=-1  !> Position will be written after this value
    real*8    :: next_time=0  !> updated to know next time to write position

!... elementary barriers (in eV) and inverse temperature (in 1/eV), temperature (K)
    real*8  :: elem_barrier(total_states*2)
    real*8  :: prefactors(total_states*2), pref
    real*8 rat(10)
    real*8  :: temperature
    real*8  :: beta

!... E field parameters
    real*8  :: alpha=0, alpha_0  !> field factors alpha=alpha_0*sin(omega*t)
    logical :: oscilatory_field=.false. !> If true -> <Direction> changes every <freq_field>
    integer :: field_direction=1 !> Direction of the E field: [1 or -1]
    real*8  :: freq_field=1000000.0 !> Field's frequency omega (Hz?)
    real*8,parameter :: pi = 3.1415926535897931

    real*8,parameter :: Boltzm = 8.617343e-5  ! in eV/K

 
  CONTAINS

  subroutine read_input()
!..............................................................................
!....... read input info ......................................................
!..............................................................................
    integer i,l0
    character cha*10
!
    open(lui,file='input.dat', form='formatted')
    open(luo0,file='kmc.log', form='formatted')

!
!_________________ No of KMC steps
!
    call find_string('number_of_kmc_steps',19,Line,1,.true.,iErr)
    if(iErr.ne.0) call error_message ('Error: number_of_kmc_steps needed')
    call CutStr(Line,NumLin,LinPos,LinEnd,0,0,iErr)
    if(NumLin.lt.2) call error_message ('Error: No number_of_kmc_steps given')
    read(Line(LinPos(2):LinEnd(2)),*,err=10) No_kmc
    write(luo0,'(a,i10)') '... Number of KMC steps found = ',No_kmc
    write(cha,'(i10)') No_kmc
    ! call posit(cha,10,l0)
    ! len_No_kmc=10-l0+1
! !
! !_________________ if to do testing
! !
!     testing=.false.
!     call find_string('do_testing',10,Line,1,.true.,iErr)
!     if(iErr.eq.0) testing=.true.
!     write(luo0,'(a,l1)') '... TESTING = ',testing
!     ! TO CHECK: If read correctly the barriers
!
!__________________ Defining the PES
!
    call find_string('kind_of_PES',11,Line,1,.true.,iErr)
    if(iErr.ne.0) call error_message ('ERROR: kind_of_PES needed')
    call CutStr(Line,NumLin,LinPos,LinEnd,0,0,iErr)
    if(NumLin.lt.2) call error_message ('ERROR: no kind_of_PES specified')
    read(Line(LinPos(2):LinEnd(2)),*,err=10) kind_of_PES
    write(luo0,'(a,i2)') '... kind_of_PES:  = ', kind_of_PES
!
!_________________ elementary barrier (in eV)
!
    elem_barrier=0.0
    call find_string('barriers',8,Line,1,.true.,iErr)
    if(iErr.ne.0) call error_message ('ERROR: barriers needed')
    if (kind_of_PES == 0) then  !> Two identical barriers
        call CutStr(Line,NumLin,LinPos,LinEnd,0,0,iErr)
        if(NumLin.lt.2) call error_message ('ERROR: 2 barriers required for kind_of_PES = 0)')
        read(Line(LinPos(2):LinEnd(3)),*,err=10) (elem_barrier(i),i=1,2)
    else 
        call CutStr(Line,NumLin,LinPos,LinEnd,0,0,iErr)
        if(NumLin.lt.4) call error_message ('ERROR: 4 barriers required for kind_of_PES = 1)')
        read(Line(LinPos(2):LinEnd(5)),*,err=10) (elem_barrier(i),i=1,4)
    end if
!
!_________________ prefactors (in eV)
!
    prefactors=0.0
    call find_string('prefactors',10,Line,1,.true.,iErr)
    if(iErr.ne.0) call error_message ('ERROR: prefactors requiered')
    if (kind_of_PES == 0) then
        call CutStr(Line,NumLin,LinPos,LinEnd,0,0,iErr)
        if(NumLin.lt.2) call error_message ('ERROR: 2 prefactors required for kind_of_PES = 0)')
        read(Line(LinPos(2):LinEnd(3)),*,err=10) (prefactors(i),i=1,2)
    else
        call CutStr(Line,NumLin,LinPos,LinEnd,0,0,iErr)
        if(NumLin.lt.4) call error_message ('ERROR: 4 prefactors required for kind_of_PES = 1)')
        read(Line(LinPos(2):LinEnd(5)),*,err=10) (prefactors(i),i=1,4)
    end if
!
!_________________ temperature 
!
    call find_string('temperature',11,Line,1,.true.,iErr)
    if(iErr.ne.0) call error_message ('ERROR: Temperature needed')
    call CutStr(Line,NumLin,LinPos,LinEnd,0,0,iErr)
    if(NumLin.lt.2) call error_message ('ERROR: temperature not specified')
    read(Line(LinPos(2):LinEnd(2)),*,err=10) temperature
    beta=1.0/(Boltzm*temperature)
!
!_________________ alpha 
!
    call find_string('alpha',5,Line,1,.true.,iErr)
    if(iErr.eq.0) then
       call CutStr(Line,NumLin,LinPos,LinEnd,0,0,iErr)
       if(NumLin.lt.2) call error_message ('ERROR: alpha given, but not specified')
       read(Line(LinPos(2):LinEnd(2)),*,err=10) alpha_0
       alpha = alpha_0
    end if
!
!_________________ oscilatory field 
!
    call find_string('oscilatory_field',16,Line,1,.true.,iErr)
    if(iErr.eq.0) then
       call CutStr(Line,NumLin,LinPos,LinEnd,0,0,iErr)
       if(NumLin.lt.2) call error_message ('ERROR: please write oscilatory_field .true. or .false.')
       read(Line(LinPos(2):LinEnd(2)),*,err=10) oscilatory_field
    end if
    write(luo0,'(a,L3)')'... Oscilatory field  = ', oscilatory_field
    write(luo0,'(a)')'... alpha(w,t) = alpha_0 *sin(omega*t)'
!_________________ Frequency of the oscilatory field 
    call find_string('freq_field',10,Line,1,.true.,iErr)
    if(iErr.eq.0) then
       call CutStr(Line,NumLin,LinPos,LinEnd,0,0,iErr)
       if(NumLin.lt.2) call error_message ('ERROR: freq_field given, but not specified')
       read(Line(LinPos(2):LinEnd(2)),*,err=10) freq_field
    end if
    if (oscilatory_field) write(luo0,'(a,f12.4,a)')'... Frequency of the Oscilatory field (omega)  = ', freq_field, " (Hz)"
! !
! !________ prefix to the rate, i.e. the exp prefactor to the rate:
! !         - in inverse ps (1 ps=10^{-12} s)
!     pref= +1.0 * log(10.0)
!
!...................... RATES TABLE ...............................................................
!
    call compute_rates ()
    call write_data_in_log ()
!
!________________ level of print: 0 - very little; 5 - a lot
! 
    iPrnt=0
    call find_string('printing',8,Line,1,.true.,iErr)
    if(iErr.eq.0) then
       call CutStr(Line,NumLin,LinPos,LinEnd,0,0,iErr)
       if(NumLin.lt.2) call error_message ('ERROR: please specified a printing level (does nothing currently)')
       read(Line(LinPos(2):LinEnd(2)),*,err=10) iPrnt
    end if
    write(luo0,'(a,i1)')'... Printing level = ',iPrnt
!
!_________________ if perform statistics
    do_statistics = .false.
    call find_string('do_statistics',13,Line,1,.true.,iErr)
    if(iErr.eq.0) do_statistics=.true.
    write(luo0,'(a,l1)') '... write statistics = ',do_statistics 

    freq_statistics = 1000
    call find_string('freq_statistics',15,Line,1,.true.,iErr)
    if(iErr.eq.0) then
       call CutStr(Line,NumLin,LinPos,LinEnd,0,0,iErr)
       if(NumLin.lt.2) call error_message ('ERROR: specify freq_statistics')
       read(Line(LinPos(2):LinEnd(2)),*,err=10) freq_statistics
    end if
    write(luo0,'(a,i7,a)')'... Heavy statistics perform every  = ',freq_statistics, ' kmc steps'

    freq_writing = 10000
    call find_string('freq_writing',12,Line,1,.true.,iErr)
    if(iErr.eq.0) then
       call CutStr(Line,NumLin,LinPos,LinEnd,0,0,iErr)
       if(NumLin.lt.2) call error_message ('ERROR: specify freq_writing')
       read(Line(LinPos(2):LinEnd(2)),*,err=10) freq_writing
    end if
    write(luo0,'(a,i7,a)')'... KMC state will be written  every  = ',freq_writing, ' kmc steps'
    
!_________________ Choose a time interval to write results
    call find_string('time_interval',13,Line,1,.true.,iErr)
    if(iErr.eq.0) then
       call CutStr(Line,NumLin,LinPos,LinEnd,0,0,iErr)
       if(NumLin.lt.2) call error_message ('ERROR: specify time_interval')
       read(Line(LinPos(2):LinEnd(2)),*,err=10) time_interval
       time_interval = time_interval / 1d-9
    end if
    write(luo0,'(a,f10.5,a)')'... KMC state will be written every = ',time_interval*1d-9, 'ms (in file kmc_time.out)'
!
!___________ Choose fixed or random seed for random number generation
    seed=0
    call find_string('seed',4,Line,1,.true.,iErr)
    if(iErr.eq.0) then
       call CutStr(Line,NumLin,LinPos,LinEnd,0,0,iErr)
       if(NumLin.lt.2) then
           call warning_message ('Warning: seed given but nor specified, random seed used instead')
       else
           read(Line(LinPos(2):LinEnd(2)),*,err=10) seed
       end if
    end if
    if (seed > 0 ) then 
        seed = -seed ! Seed has to be a negative integer
        write(luo0,'(a,i6)')'... Seed used for Random Number Generation = ', -seed
    else 
!       Change the seed for random numbers generation
        call init_random_seed()        ! initialise seed using the clock
        call random_number(real_seed)  ! Get a real random number (0.0-1.0)
        seed = -int(real_seed*10000)-1 ! Convert real_seed to integer between [1? o 0? y 10000]
        write(luo0,'(a,i6)')'... Random seed used for Random Number Generation = ', -seed
    end if 
!
    close (lui)
    write(*,'(/a/)')'... Input file has been read in successfully ...'
    write(luo0,'(/a/)')'... Input file has been read in successfully ...'
    close (luo0)

    return

!................ errors in reading input
10  write(*,*)'FATAL errors in reading the input file!'
    stop 'FULL STOP!'
  end subroutine read_input

  subroutine error_message (message)
!................ errors in reading input
      implicit none
      character (len=*):: message

      write(*,*)'FATAL errors in reading the input file!'
      write(*,*) trim(message)
      stop 'FULL STOP!'

  end subroutine

  subroutine warning_message (message)
!................ errors in reading input
      implicit none
      character (len=*):: message

      write(*,*) trim(message)

  end subroutine
      
   
  subroutine compute_rates ()
  
      implicit none
      integer :: i
      
      if (kind_of_PES == 0) then
          do i=1,2
             pref= +1.0 * log(prefactors(i))
             if ( i == 2 ) then ! i == 2 Pushing diffusion applying a E field
                 rat(i)=exp(-beta*(elem_barrier(i)+alpha/2)+pref)
             else 
                 rat(i)=exp(-beta*(elem_barrier(i)-alpha/2)+pref)
             end if
          end do
      else 
          do i=1,4
             pref= +1.0 * log(prefactors(i))
             if ( i == 2 ) then ! i == 2 Pushing diffusion to the right
                 rat(i)=exp(-beta*(elem_barrier(i)+alpha)+pref)
             else 
                 rat(i)=exp(-beta*elem_barrier(i)+pref)
             end if
          end do
      end if

  end subroutine compute_rates
    
  subroutine write_data_in_log ()

      ! rat(8)=depos_rate   ;    elem_barrier(8)=0.0d0
      write(luo0,'(/a)')'============|  Barriers (in Ev), Rates (in ps^-1) and Prefactors |============'
      write(luo0,'(a,f10.3,a)') '   Temperature = ',temperature,' K'
      write(luo0,'(a,f8.5,a)') '   alpha = ',alpha, ' eV'
      write(luo0,'(a,f10.5,x,e12.6,x,e12.6)') ' 1. State 1 to the right:',elem_barrier(1),rat(1), prefactors(1)
      write(luo0,'(a,f10.5,x,e12.6,x,e12.6)') ' 2. State 1 to the left :',elem_barrier(2),rat(2), prefactors(2)
      if (kind_of_PES == 1) then
          write(luo0,'(a,f10.5,x,e12.6,x,e12.6)') ' 3. State 2 to the right:',elem_barrier(3),rat(3), prefactors(3)
          write(luo0,'(a,f10.5,x,e12.6,x,e12.6)') ' 4. State 2 to the left :',elem_barrier(4),rat(4), prefactors(4)
      end if
      write(*,'(/a)')'============|  Barriers (in Ev), Rates (in ps^-1) and Prefactors |============'
      write(*,'(a,f10.3,a)') '   Temperature = ',temperature,' K'
      write(*,'(a,f8.5,a)') '   alpha = ',alpha, ' eV'
      write(*,'(a,f10.5,x,e15.9,x,e12.6)') ' 1. State 1 to the right:',elem_barrier(1),rat(1), prefactors(1)
      write(*,'(a,f10.5,x,e15.9,x,e12.6)') ' 2. State 1 to the left :',elem_barrier(2),rat(2), prefactors(2)
      if (kind_of_PES == 1) then
          write(*,'(a,f10.5,x,e15.9,x,e12.6)') ' 3. State 2 to the right:',elem_barrier(3),rat(3), prefactors(3)
          write(*,'(a,f10.5,x,e15.9,x,e12.6)') ' 4. State 2 to the left :',elem_barrier(4),rat(4), prefactors(4)
      end if
  !     write(luo0,'(a,f10.5,x,e12.6)') ' 5. ',elem_barrier(5),rat(5)
  !     write(luo0,'(a,f10.5,x,e12.6)') ' 6. ',elem_barrier(6),rat(6)
  !     write(luo0,'(a,f10.5,x,e12.6)') ' 8. ',elem_barrier(8),rat(8)
      write(luo0,'(a/)')'==================================================================='
      
  end subroutine 

  subroutine write_debug ()
      !write(luo99,*) time, rat(1), rat(2), distance,alpha
      write(luo99,*) time, rat(1), rat(2), distance,alpha
  end subroutine 

end module param
