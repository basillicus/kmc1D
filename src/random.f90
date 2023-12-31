  real function ran2(idum)
    integer idum,im1,im2,imm1,ia1,ia2,iq1,iq2,ir1,ir2,ntab,ndiv
    real am,eps,rnmx
    parameter (im1=2147483563,im2=2147483399,am=1./im1,imm1=im1-1, &
          ia1=40014,ia2=40692,iq1=53668,iq2=52774,ir1=12211,       &
          ir2=3791,ntab=32,ndiv=1+imm1/ntab,eps=1.2e-7,rnmx=1.-eps)
!.............................................................................
!     Long period (>2x10^18) random number generator of L'Ecyer with Bays-   .
!     Durham shuffle and add safeguards. Returns a uniform deviate between   .
!     0.0 and 1.0 (exculsive of the endpoint values). Call with idum a       .
!     negative integer to initialise; thereafter, do no alter idum between   .
!     successive deviates in a sequence. RNMX should approximate the largest .
!     floating value that is less than 1                                     .
!.............................................................................
    integer idum2,j,k,iv(ntab),iy
    save iv,iy,idum2
    data idum2/123456789/, iv/NTAB*0/, iy/0/
    if(idum.le.0) then
       idum=max(-idum,1)
       idum2=idum
       do 11 j=ntab+8,1,-1
          k=idum/iq1
          idum=ia1*(idum-k*iq1)-k*ir1
          if(idum.lt.0) idum=idum+im1
          if(j.le.ntab) iv(j)=idum
11     end do
       iy=iv(1)
    end if
    k=idum/iq1
    idum=ia1*(idum-k*iq1)-k*ir1
    if(idum.lt.0) idum=idum+im1
    k=idum2/iq2
    idum2=ia2*(idum2-k*iq2)-k*ir2
    if(idum2.lt.0) idum2=idum2+im2
    j=1+iy/ndiv
    iy=iv(j)-idum2
    iv(j)=idum
    if(iy.lt.1) iy=iy+imm1
    ran2=min(am*iy,rnmx)
  end function ran2

SUBROUTINE init_random_seed()
! --------------------------------------------------  
! Cambia la semilla para generar numeros aleatorios
! usando el reloj del ordenador.
! --------------------------------------------------
  INTEGER :: i, n, clock
  INTEGER, DIMENSION(:), ALLOCATABLE :: seed
  n=1       
  CALL RANDOM_SEED(size = n)
  ALLOCATE(seed(n))
          
  CALL SYSTEM_CLOCK(COUNT=clock)
          
  seed = clock + 37 * (/ (i - 1, i = 1, n) /)
  CALL RANDOM_SEED(PUT = seed)
          
  DEALLOCATE(seed)
END SUBROUTINE

