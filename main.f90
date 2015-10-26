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
    write (luo,'(a)'), "# time  distance  kmc-step"
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
    write (luo,'(f12.3,i10,i10)'),time,distance,kmc
end subroutine write_evolution 

subroutine finish()
    use param
    implicit none
    close (luo)
end subroutine finish
