program KMC_1D
    use param
    use move
    use moves
    
    implicit none

    call flush ()
    call read_input ()

    ! Initialize the system
    call init_kmc ()
    allocate(movement(max_moves))
    ! if (do_statistics) call init_statistics ()

    write (*,*) "KMC is running..."
    do kmc=1,No_kmc

        ! Create/Update moves
        call all_moves()
        ! Choose a move
        call which_move(m)
        ! Perform move
        call perform_move(m,kmc)
        ! Update time
        call update_time()
        ! Update field if oscilatory
        if (oscilatory_field) call update_field()
        ! Write the evolution in a file
        call write_evolution()

    end do

    call finish()
    write (*,*) "KMC finish!"
end program

subroutine init_kmc()
    use param
    use move
    ! Set the initial state of the system
    implicit none
    character :: cha*20='', str*200=''
    integer :: i

    current_state= 1  
    previous_direction = 0 
    time = 0

    ! If non standard KMC add the posibility of do nothing
    if ( constant_time_step > 0 ) max_moves=max_moves+1
    open(luo,file='kmc.out',form='formatted')

    if ( time_interval .gt. 0 ) then 
        open(luo2,file='kmc_time.out',form='formatted')
    end if

    ! Write info of the system in the header of the kmc.out file
    do i=1,size(elem_barrier)
        write (cha, '(f9.5)') elem_barrier(i)
        str = trim(str) // trim(cha)
    end do
    write (luo,'(a)'), "# E_barriers: " // trim(str) // " (eV)"

    if ( time_interval .gt. 0 )  write (luo2,'(a)'), "# E_barriers: " // trim(str) // " (eV)"

    write (cha, '(f9.5)') alpha
    write (luo,'(a)'), "# alpha: " // trim(cha) // " eV"

    if ( time_interval .gt. 0 ) write (luo2,'(a)'), "# alpha: " // trim(cha) // " eV"

    if (oscilatory_field)  then 
        write (luo,'(a)'), "# Oscilatory external field: True"
        write (cha, '(f12.4)') freq_field
        write (luo,'(a)'), "# frequency: " // trim(cha) // " Hz"
        if (field_shape == 1 ) then 
            write (luo,'(a)'), "# field shape: sinusoidal" 
        else if (field_shape == 2 ) then 
            write (cha, '(f9.5)') assym_factor
            write (luo,'(a)'), "# field shape: square wave" 
            write (luo,'(a)'), "# Assymetric factor:"  // trim(cha)
        end if
    else
        write (luo,'(a)'), "# frequency: 0 Hz"
        if ( time_interval .gt. 0 ) write (luo2,'(a)'), "# frequency: 0 Hz"

    end if

    write (cha, '(f9.3)') temperature
    write (luo,'(a)'), "# temperature: " // trim(cha) // " K"
    write (cha, '(i20)') No_kmc
    write (luo,'(a)'), "# No KMC steps: " // trim(cha) // " K"
    if ( time_interval .gt. 0 ) write (luo2,'(a)'), "# temperature: " // trim(cha) // " K"
    write (luo,'(a)'), "# time(ms)  distance  kmc-step"
    if ( time_interval .gt. 0 ) write (luo2,'(a)'), "# time(ms)  distance  kmc-step"

end subroutine init_kmc

subroutine update_time()
    use param
    implicit none
    ! real*8, intent(inout) :: time
    time = time + dt 
end subroutine

subroutine update_field()
    use param
    implicit none

    ! Sinusoidal field
    if ( field_shape  == 1 ) then
        alpha = alpha_0*cos( 2*pi*freq_field*1.0d-12*time ) 
        !alpha = alpha_0*sin( 2*pi*freq_field*1.0d-12*time ) 
    ! Square field
    else if ( field_shape  == 2 ) then 
        alpha = sign(alpha_0, cos( 2*pi*freq_field*1.0d-12*time )+assym_factor )
    end if
    call compute_rates ()
    ! ! KKK
    !  if ( kmc/freq_writing * freq_writing == kmc ) call write_debug ()

end subroutine

subroutine write_evolution ()
    use param
    implicit none
    integer :: counter=1

!     writing_state = writing_state + 1
!     if ( writing_state >= freq_writing ) then
!         write (luo,'(f24.5,i8,i14)'),time*1d-9,distance,kmc
!         writing_state = 0
!     end if
     if ( kmc/freq_writing * freq_writing == kmc ) then
         write (luo,'(f24.5,i10,i14)'),time*1d-9,distance,kmc
         call write_debug ()
     end if
     if (time_interval .gt. 0 ) then
         if ( time .gt. next_time ) then
             write (luo2,'(f14.4,i8,i14)'),time*1d-9,distance,kmc
             next_time = counter*time_interval
             counter = counter+1
         end if
     end if

end subroutine write_evolution 

subroutine finish()
    use param
    implicit none
    close (luo)
    close (luo2)
end subroutine finish
