!* ##########################################################################
! *
! *  grid_vars.cc: Implement class function of grid_vars 
! *
! *  Author: Wenwu Xia (CIMMS/NSSL),Jian Zhang (CIMMS/NSSL)
! *  May 10, 2000
! *
! *  Modification History:
! *  3/8/2002  Jian Zhang (CIMMS/NSSL)
! *  Removed the vertical grid streching options.  The exact heights of
! *  each grid levels will be input from the config file.
! *
! * ########################################################################## */

!#include <iostream>
!#include <cstdio>
!#include <string>
!#include <cstdlib>

!#include "setgrd.h"
!#include "maplib3d.h"
!#include "grid_vars.h"
!#include "CONFIG_PARS.h"

!using namespace std;

!void readtrn_func (const int nx, const int ny,
!                  const float dx, const float dy,
!                  const char *terrain_file,
!                  float ** hterain);


!void xytoll_func(float **rlat,float **rlon,float *x,float *y,
!     int idim, int jdim,setmapr &m);
 
      
      SUBROUTINE ini_grd (pp,sm,x,y,zp,hterain,gridlat,gridlon)
       
       use CONFIG_PARS
       use MAPLIB3D
       implicit none

       type (CCOUT_CONFIG_PARS), intent(in)    :: pp
       type (setmapr)          , intent(inout) :: sm
       real                    , intent(out)   :: x(pp.nx),y(pp.ny)
       real                    , intent(out)   :: zp(pp.nz)
       real                    , intent(out)   :: hterain(pp.nx,pp.ny)
       real                    , intent(out)   :: gridlat(pp.nx,pp.ny)
       real                    , intent(out)   :: gridlon(pp.nx,pp.ny)


       integer   i,j,k
       real      topomin,topomax
       real, allocatable  :: s_x(:),s_y(:)
       character*200   temp_str       

!//#######################################################################
!//
!//     Define a 2D uniform Cartesian grid.  
!//
!//#######################################################################

       allocate(s_x(pp.nx),s_y(pp.ny))
       call  set_grd (sm,pp,s_x,s_y,pp.nx,pp.ny)

       print*,pp.nx,pp.ny  

       do i=1,pp.nx,1
        x(i)=s_x(i)
       enddo
       do j=1,pp.ny,1
        y(j)=s_y(j)
       enddo
       do k=1,pp.nz,1
        zp(k)=0.
       enddo

!!!    Specify the terrain

       IF( pp.ternopt.eq.0 )  THEN 

!!!     No terrain, the ground is flat

        do i=1,pp.nx,1
        do j=1,pp.ny,1
          hterain(i,j) = 0.
        enddo
        enddo

       ELSE IF( pp.ternopt.eq.1 ) THEN
 
!!!  Read in the terrain data.  Print min/max topo info.

        write(temp_str,10) pp.g_refdata_dir,pp.terndta
10      format(a,'/',a)

        call  readtrn_func(pp.nx,pp.ny,pp.dx,pp.dy,temp_str,hterain)

        topomin = hterain(1,1)
        topomax = hterain(1,1)

        do i=1,pp.nx,1
        do j=1,pp.ny,1
          if(hterain(i,j) .lt. -9990) hterain(i,j) = -3.0
          if(topomin.gt.hterain(i,j)) topomin=hterain(i,j)
          if(topomax.lt.hterain(i,j)) topomax=hterain(i,j)
        enddo
        enddo

        print*,"  topomin/max:: ",topomin,"  ",topomax

       ENDIF
 
!!!  Find the lat/lon coordinates of each Cartesian grid points.

       call  xytoll_func(gridlat,gridlon,s_x,s_y,pp.nx,pp.ny,sm)

       deallocate(s_x,s_y)

       print*, 'ini_grd', gridlat(301,301), gridlon(301,301)
     
       RETURN
  
      END SUBROUTINE ini_grd 
