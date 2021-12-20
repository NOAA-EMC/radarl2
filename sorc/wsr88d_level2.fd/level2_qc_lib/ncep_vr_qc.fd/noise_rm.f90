!
!     ##################################################################
!     ##################################################################
!     ######                                                      ######
!     ######        Radar Data Quality Control System.            ######
!     ######                                                      ######
!     ######              Copyright (c) 2009                      ######
!     ######             Cooperative Institute                    ######
!     ######       for Mesoscale Meteorological Studies           ######
!     ######    University of Oklahoma.  All rights reserved.     ######
!     ######                                                      ######
!     ##################################################################
!     ##################################################################
!
!     ==================================================================
!     PURPOSE:
!       noise remove based on the paper:
!       Bergen, W. R., and S. C. Albers,1988: Two- and three-dimensional
!       de-aliasing of Doppler radar velocity.
!         J. Atmos. Oceanic Technol., vol. 5, 305-319.
!     ==================================================================
!
!     Author :  Wei Gu
!     Date   :  
!     Action :  Created.
!
!     History:
!     --------
!     name   : Kang Nai
!     date   : Nov. 13, 2007
!     action : added a beam remove process.
!
!     ------------------------------------------------------------------
      subroutine noise_rm ( nr,np,vel,nbeam,iminrng,nrang,spval )

      implicit none

      integer nr,np
      real spval
      integer nbeam,iminrng,nrang
      integer i,j,ip,jp,ims,inr,inp
      integer iii,jjj,i0,j0,npts

      real   ,dimension(nr,np) :: vel
      integer,dimension(nr,np) :: index1
 
! ----------------------------------------------------------------------
!     The first step is a point-removel process that consists of moving
!     3x3 window throughout the data in which each velocity measurement
!     becomes the central point of the window.
! ----------------------------------------------------------------------
      index1=0
      do j=1,nbeam
      do i=iminrng+1,nrang-1
        if ( abs(vel(i,j))<spval ) then
          npts  = 0
          do jjj=-1,1
          do iii=-1,1
            i0 = i + iii
            j0 = j + jjj
            if (j0.le.0) j0=nbeam+j0
            if (j0>nbeam) j0=j0-nbeam
            if (abs(vel(i0,j0))<spval) then
              npts  =  npts + 1
            end if
          end do
          end do
          if ( npts.lt.5 ) index1(i,j)=1
        endif
      enddo
      enddo
      do j=1,nbeam
      do i=iminrng,nrang
        if( index1(i,j).eq.1 ) vel(i,j)=spval 
      enddo
      enddo
! ----------------------------------------------------------------------
!     The nest step removes velocity data by search isolated velocities 
!     which can result from the previous step.
! ----------------------------------------------------------------------
      do j=1,nbeam
      do i=iminrng+1,nrang-1
        if ( abs(vel(i,j))<spval ) then
          npts  = 0
          do jjj=-1,1
          do iii=-1,1
            i0 = i + iii
            j0 = j + jjj
            if (j0.le.0) j0=nbeam+j0
            if (j0>nbeam) j0=j0-nbeam
            if (abs(vel(i0,j0))<spval) then
              npts  =  npts + 1
            end if
          end do
          end do
          if ( npts.eq.1 ) vel(i,j)=spval
        endif
      enddo
      enddo

! ----------------------------------------------------------------------
!     The last step is beam-removel process that consists of moving sole
!     one or two beam. ie. the available data should include at least
!     3 beams.
! ----------------------------------------------------------------------
      index1=0
      do i =iminrng,nrang
      do jp=1,nbeam
        npts=0
        do jjj=-2,2
          j=jp+jjj
          if ( j<=0 ) j=nbeam+j
          if ( j>nbeam) j=j-nbeam
          if ( abs(vel(i,j))<spval ) then
            npts=npts+1
          endif
        enddo
        if ( npts<3 ) index1(i,jp)=1
      enddo
      enddo
      do j=1,nbeam
      do i=iminrng,nrang
        if ( index1(i,j).eq.1 ) vel(i,j)=spval
      enddo
      enddo
      do i =iminrng,nrang
        npts=0
        do j=1,nbeam
          if ( abs(vel(i,j))<spval ) npts=npts+1
        enddo
        if ( npts<=5 ) then
          do j=1,nbeam
            vel(i,j)=spval
          enddo
        endif
      enddo

      return
      end subroutine noise_rm
