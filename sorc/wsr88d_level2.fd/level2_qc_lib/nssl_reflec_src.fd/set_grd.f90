!#include "maplib3d.h"
!#include "setgrd.h"

!using namespace std;


!void lltoxy_func(float rrlat,float rrlon,float &xloc,float &yloc,setmapr& a);


      SUBROUTINE set_grd(sm,pp,xx,yy,nx,ny)
   
       use CONFIG_PARS
       use MAPLIB3D
       implicit none

       type (setmapr)           ,intent(inout) :: sm
       type (CCOUT_CONFIG_PARS) ,intent(in)    :: pp
       integer                  ,intent(in)    :: nx,ny
       real                     ,intent(out)   :: xx(nx),yy(ny)

       real     dxscl        !! grid spacing (m) in x-direction 
                             !! normalized by the map scale 
       real     dyscl        !! grid spacing (m) in y-direction 
                             !! normalized by the map scale 
       real     swx, swy
       real     xloc,yloc
       integer  i,j

       if( map_scale_fct .ne. 1.0) then 
         dxscl = pp.dx/map_scale_fct
         dyscl = pp.dy/map_scale_fct
       else  
         dxscl = pp.dx
         dyscl = pp.dy
       endif

!//#######################################################################
!//
!//     Find the x- and y-coordinates (i.e., ctrx and ctry, in meters) 
!//     of the domain ceter point (ctrlat, ctrlon) with respect to 
!//     the north pole, which is the origin.
!//
!//#######################################################################

       xloc = 0.0
       yloc = 0.0

       print*,dxscl,dyscl,pp.ctrlat,pp.ctrlon         
       call  lltoxy_func(pp.ctrlat,pp.ctrlon,xloc,yloc,sm) 


!//#######################################################################
!//
!//     Find the x- and y-coordinates, in meters, of the south-west 
!//     corner of the analysis domain (w.r.t. the north pole).
!//     Define the SW corner as the new origin of the Cartesian grid
!//     coordinate system.
!//
!//#######################################################################

       swx = xloc
       swy = yloc
       swx = swx - real(nx-1)/2. * dxscl
       swy = swy - real(ny-1)/2. * dyscl

       if(pp.mapproj.eq.4) then
        
         do i=1,nx,1
           xx(i) = swx + dxscl * real(i-1)
         enddo
         do j=1,ny,1
           yy(j) = swy + dyscl * real(j-1)
         enddo 
  
       else

         call set_orig(sm,1,swx,swy) 

!//#######################################################################
!//
!//     Calculate the x- and y-coordinates for the rest
!//     of the analysis grid points in earth meters*sclf ,  with the
!//     SW corner as the origin.
!//
!//#######################################################################
       
         do i=1,nx,1
           xx(i) = dxscl * real(i-1)
         enddo
         do i=1,ny,1
           yy(j) = dyscl * real(j-1)
         enddo
  
       endif

       print*,'set_grd',xx(301),yy(301)

       RETURN

      END SUBROUTINE set_grd       
