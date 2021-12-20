
SUBROUTINE FILLING_SPVAL

! ======================================================================
!    PURPOSE:
!       No VAD as the reference, just put the spval to the point which
!       abs(vr)>0.9 Nyq.
! ======================================================================
!
!    Author  : Nai, Kang
!    Date    : Sep. 26, 2007
!    Action  : Created.
!
!    History :
!   ----------
!
! ----------------------------------------------------------------------

  USE variable_define

  IMPLICIT NONE

  integer :: i,j,k
  real    :: thrpri
  character(len=50) :: name1,name2,name3

! ----------------------------------------------------------------------
! filling in the missing value to the special points.
! ----------------------------------------------------------------------
  k=k_tilt

  thrpri=20.0

  if ( ivadflag<=0 ) then
       unfvel=obsvel
       do j=1,nbeam; do i=iminrng,nrang
               unfvel(i,j)=spval
       enddo; enddo
  endif

! name1='There is no VAD, no action be made!!!'
! i=int(wrt_thet(k)*100.0)
! write(name2,'(i4.4)') i
! name3='fill'//name2(1:4)//'.dat'
! open(31,file=name3)
!   write(31,'(a37)') name1
! close(31)

END SUBROUTINE FILLING_SPVAL
