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

subroutine write_evolution ()
    use param
    implicit none
!     writing_state = writing_state + 1
!     if ( writing_state >= freq_writing ) then
!         write (luo,'(f24.5,i8,i14)'),time*1d-9,distance,kmc
!         writing_state = 0
!     end if
     if ( kmc/freq_writing * freq_writing == kmc ) then
         write (luo,'(f24.5,i8,i14)'),time*1d-9,distance,kmc
     end if
     if (time_interval .gt. 0 ) then
         if ( time .gt. next_time ) then
             write (luo2,'(f24.5,i8,i14)'),time*1d-9,distance,kmc
             next_time = time + time_interval
         end if
     end if

end subroutine write_evolution 

subroutine finish()
    use param
    implicit none
    close (luo)
end subroutine finish
