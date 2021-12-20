
 SUBROUTINE ORIG_RADAR

! ======================================================================
!   PURPOSE:
!     set map projection parameters, and design the radar position
!     as the original points.
! ======================================================================

  use variable_define

  IMPLICIT NONE

! REAL, DIMENSION(2) :: latnot(2)

  trulon=radlon
  latnot(1)=trulat1
  latnot(2)=trulat2
  print*,trulon,trulat1,trulat2,radlat,radlon
  call setmapr(mapproj,sclfct,latnot,trulon)
  call lltoxy(1,1,radlat,radlon,xr0,yr0)

  call setorig(1,xr0,yr0)
  call lltoxy(1,1,radlat,radlon,xr0,yr0)

  print*
  print*,'lat,lon = ',radlat,radlon
  print*,'xr0,yr0 = ',xr0,yr0
  print*

 END SUBROUTINE ORIG_RADAR
