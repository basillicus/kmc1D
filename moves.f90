module moves
    use param
    use move

CONTAINS

subroutine all_moves()
  ! use param
  implicit none
      integer :: k
      real*8  :: rate
  m=0

  if (kind_of_PES == 0 ) then 
      ! Move to the right
      call fill_in (m,1,1,elem_barrier(1),rat(1),'diffus')
      ! Move to the left
      call fill_in (m,1,-1,elem_barrier(2),rat(2),'diffus')
  else !> if kind_of_PES == 1
      if (current_state == 1) then 
          ! Move to the right
          call fill_in (m,1,1,elem_barrier(1),rat(1),'diffus')
          ! Move to the left
          call fill_in (m,1,-1,elem_barrier(2),rat(2),'diffus')
      else if (current_state == 2) then 
          ! Move to the right
          call fill_in (m,2,1,elem_barrier(3),rat(3),'diffus')
          ! Move to the left
          call fill_in (m,2,-1,elem_barrier(4),rat(4),'diffus')
      end if
  end if

  sum_rates=0.0
  do k=1,m
      sum_rates=sum_rates+movement(k)%rate
  end do
!
!....... add DO NOTHING as a move if the fixed-step KMC is ON
!
  if ( constant_time_step > 0 ) then 
     rate=exp(-sum_rates*dt)
     call fill_in(m,current_state,0,0.0d0,rate,'do NOTHING')
  end if

  No_moves=m

end subroutine all_moves

subroutine which_move(m)

!.........................................................................
! From all the posible moves it takes one using KMC rules:
! m    - move number to be taken
! time - time for that move to take place (in units of the rates)
!.........................................................................
    implicit none 
    integer m,m1, Nom
    real*8 R,x,rx,x1,ex
    real ran2

    R=sum_rates 
!__________________ calculate time
    if (constant_time_step < 0 ) then ! standard KMC
         ex=0.0d0;  Nom = No_moves
         x1=ran2(seed) ; dt=-1.0/R*log(x1)  
    else ! if constant_time_step > 0 --> dt is constant
         ex=exp(-R*dt) ; Nom = No_moves - 1
    end if 
!
!_____________ trap the transition number
!    
    x=ran2(seed) ; rx=0.0

    do m1=1,Nom
       rx=rx+movement(m1)%rate/R*(1.0d0-ex) 
       if(x.lt.rx) then
          m=m1 ; return
       end if
    end do

    if (constant_time_step < 0 ) then 
        write (luo0,'(a,i10)') 'No move found! at kmc step: ', kmc
    else ! Constant time step
        m=No_moves
    end if

end subroutine which_move

subroutine perform_move(m,kmc)

    implicit none
    integer m,i,j,kmc
    character*6 type

    
!........... get the attributes of the successful move m
    i=movement(m)%ini !> State of the molecule
    ! j = direction of the movement
    j=movement(m)%dir ; type=movement(m)%type 
    
    select case (type)

    ! La molecula solo avanza cuando se mueve desde el estado 2 y el sentido el movimiento es 
    ! el mismo que el sentido de movimiento del paso anterior
    case ('diffus')
        if (kind_of_PES == 0 ) then 
            distance = distance + j !> The molecule always moves
        else 
            if (i==1) then
                ! Move to the next state
                current_state = 2
                ! distance = distance + j
                previous_direction = j
            else if (i == 2) then
                current_state = 1
                if (j == previous_direction ) then 
                    distance = distance + j
                !    previous_direction = 0 !> No es necesario reiniciar este valor
                end if
            end if 
        end if
    end select
end subroutine perform_move


subroutine fill_in(m,i,j,barrier,rate,typ)
! subroutine fill_in(m,i,j,barrier,prefactor,typ)
!..............................................................................................
! fills in the array move for each possible step m with all necessary attributes
!..............................................................................................
   integer :: m
   integer :: i  !> Initial state
   integer :: j  !> Direction of the move (1: right; -1: left)

   real*8 barrier,rate,prefactor
   character*6 typ

   m=m+1
   if(m.gt.max_moves) stop 'max # of moves reached'
   movement(m)=move_attr(i,j,barrier,rate,typ)
   ! if(testing) write(*,'(i5,x,a10,x,3(i5,x),2(i3,x),f10.5,x,e12.6)') m,typ,config,i,i1,j,0,barrier,rate

end subroutine fill_in

end module moves
