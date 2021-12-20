!//#######################################################################
!//  PURPOSE:
!//  Read in the real terrain data that has been remapped onto
!//  the analysis grid.
!/* ##########################################################################
! *  Author: Wenwu Xia (CIMMS/NSSL),Jian Zhang (CIMMS/NSSL)
! *  May 10, 2000
! *
! *  Modification History:
! *  10/22/2001  Jian Zhang
! *  Changed to a simple function.
! *
! * ########################################################################*/
!
!//#######################################################################
!//
!//  INPUT :
!//
!//    nx       Number of grid points in the x-direction (east/west)
!//    ny       Number of grid points in the y-direction (north/south)
!//
!//    dx       Grid interval in x-direction
!//    dy       Grid interval in y-direction
!//
!//    terrain_file     Terrain data file name including complete path
!//
!//  OUTPUT:
!//
!//    hterain  Terrain height (m)
!//
!//#######################################################################

!#ifndef READTRN_H
!#define READTRN_H
!
!#include <fstream>
!#include <iostream>
!#include <cstdio>
!#include <cstdlib>

!using namespace std;


      SUBROUTINE readtrn_func (nx, ny,dx,dy,terrain_file,hterain)
       
       implicit none
       integer ,      intent(in)     :: nx,ny
       real    ,      intent(in)     :: dx,dy
       character*200, intent(in)     :: terrain_file
       real         , intent(out)    :: hterain(nx,ny)

       integer    nxin, nyin, idummy, i, j
       real       dxin, dyin
       logical    flag_file
       
       
       flag_file = .false.
       inquire(file=terrain_file,exist=flag_file)

       if(.not.flag_file) then
         print*,  "Error occurred when opening terrain data file "
         print*, terrain_file," Job stopped in READTRN."
         goto 999
       endif
 
       open(23,file=terrain_file,                          &
                 form='unformatted',action='read',status='old')

       read(23) idummy,nxin,nyin 

       if(nx.ne.nxin.or.ny.ne.nyin)  then
         print*," Array size in the terrain data does not match that "
         print*," of model grid. Dimensions in the data were : "
         print*,nxin,nyin
         print*," model grid size were : ", nx,ny
         print*," Job stopped in subroutine READTRN."
         goto 999
       endif

       read(23) (idummy,i=1,24)
       read(23) dxin
       read(23) dyin
       read(23) (idummy,i=1,20)

       if(    abs(int(100*((dx-dxin)/dx))).gt.1      & 
          .or.abs(int(100*((dy-dyin)/dy))).gt.1   )  then
         print*,"Grid intervals do not match "
         print*,"In the data:  ",dxin,dyin
         print*,"In the model: ",dx,dy
         print*," Job stopped in subroutine READTRN."
        goto 999
       endif

       read(23) ((hterain(i,j),i=1,nx),j=1,ny)

       close(23)

999    RETURN

      END SUBROUTINE readtrn_func 
