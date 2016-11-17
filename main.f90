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
    ! Set the initial state of the system
    implicit none

    current_state= 1  
    previous_direction = 0 
    time = 0

    open(luo,file='kmc.out',form='formatted')
    write (luo,'(a)'), "# time(ms)  distance  kmc-step"

    if ( time_interval .gt. 0 ) then 
        open(luo2,file='kmc_time.out',form='formatted')
        write (luo2,'(a)'), "# time(ms)  distance  kmc-step"
    end if
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

    alpha = alpha_0*cos( 2*pi*freq_field*1.0d-12*time ) 
    !alpha = alpha_0*sin( 2*pi*freq_field*1.0d-12*time ) 
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
