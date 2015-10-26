module moves
    use param
    use move

CONTAINS

subroutine all_moves()
  ! use param
  implicit none
      integer :: k
  m=0

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

  sum_rates=0.0
  do k=1,m
      sum_rates=sum_rates+movement(k)%rate
  end do
  No_moves=m

end subroutine all_moves

subroutine which_move(m)

  !.........................................................................
! From all the moves possible it take one using KMC rules:
! m    - move number to be taken
! time - time for that move to take place (in units of the rates)
!.........................................................................
    implicit none 
    integer m,m1
    real*8 R,x,rx,x1
    real ran2
!
!__________________ calculate time
    R=sum_rates ; x1=ran2(seed) ; dt=-1.0/R*log(x1)  
!
!_____________ trap the transition number
!    
    x=ran2(seed) ; rx=0.0

    do m1=1,No_moves
       rx=rx+movement(m1)%rate/R 
       if(x.lt.rx) then
          m=m1 ; return
       end if
    end do
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
!
    case ('diffus')
        if (i==1) then
            ! Move to the next state
            current_state = 2
            distance = distance + j
            ! previous_direction = j
        else if (i == 2) then
            current_state = 1
            ! if (j == previous_direction ) then 
                distance = distance + j
                ! previous_direction = 0 
            ! end if
        end if 
    end select
end subroutine perform_move


subroutine fill_in(m,i,j,barrier,rate,typ)
!..............................................................................................
! fills in the array move for each possible step m with all necessary attributes
!..............................................................................................
   integer :: m
   integer :: i  !> Initial state
   integer :: j  !> Direction of the move (1: right; -1: left)

   real*8 barrier,rate
   character*6 typ

   m=m+1
   if(m.gt.max_moves) stop 'max # of moves reached'
   movement(m)=move_attr(i,j,barrier,rate,typ)
   ! if(testing) write(*,'(i5,x,a10,x,3(i5,x),2(i3,x),f10.5,x,e12.6)') m,typ,config,i,i1,j,0,barrier,rate
end subroutine fill_in

end module moves
