
SUBROUTINE CONTINUE_CHECK

! ======================================================================
!    PURPOSE:
!       Body check to unfold the point which is the fold point.
! ======================================================================
!
!    Author  : Gong, Jiandong et al.
!    Date    :
!    Action  : Created.
!
!    History :
!   ----------
!   Sep. 20, 2007   Nai, Kang and Qin Xu include the simple unfold mode.
!
! ----------------------------------------------------------------------

  USE variable_define

  IMPLICIT NONE

  integer :: i,j,k,ii
  character(len=80) :: name1,name2

  real,dimension(:),allocatable   :: tmp3

  allocate ( tmp3(nr) )
 
  k=k_tilt
  do i=1,nrang
     tmp3(i)=wrkhgt(i,k)
  enddo

  CALL unfold_simple( nr,np,nrang,nbeam,wrkvel,obsvel,vnyq           &
                     ,unfvel,ivadflag,vadvel,index,thet(k)           &
                     ,tmp3,spval,k )

  deallocate ( tmp3 )

! write out the results.
! ii=int( thet(k)*100.0 )
! write(name1,'(i4.4)') ii
! name2='vadn'//name1(1:4)//'.dat'
! call write_radar(name2,unfvel)

END SUBROUTINE CONTINUE_CHECK
