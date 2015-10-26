module move
  implicit none
! attributes of the move
  type :: move_attr
     integer :: ini       ! initial state of the molecule
     integer :: dir       ! direction of the move (1: right; -1: left)
     real*8  :: barrier   ! energy barrier for the move
     real*8  :: rate      ! rate of the move
     character*6 :: type ! type of the move: diffus
  end type move_attr
  type(move_attr), dimension(:), allocatable :: movement
  integer No_moves !> Total number of possible moves in one state
  integer, parameter :: max_moves=2 !> One molecule: right or left
  real*8 sum_rates
end module move
