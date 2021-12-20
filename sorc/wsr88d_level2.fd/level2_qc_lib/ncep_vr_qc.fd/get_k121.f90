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
!=======================================================================
!     Purepose:
!       choice the largest Nyquist velocity scanning radial velocity
!       to represent the same tilt's radial velocity for QC. This
!       process will be in use to VCP=121, VCP=221.
!=======================================================================
!
!     Author   : Nai, Kang
!     Date     : Aug. 1, 2012
!     Action   : Created.

!     Modify History:
!
!     Kang Nai made a change to make sure the tilt sequence start from
!     the lowerest elevation angle.
!=======================================================================
      SUBROUTINE get_k121( nelv_vel,strct_in_vel )

      use sdata
      use variable_define

      implicit none

      integer                     :: nelv_vel
      type(ccout_strct)           :: strct_in_vel(nelv_vel)

      integer :: i,j,k,kk,kreal,ks,kstart,k1,i0,ii,ip,itmp
      real,dimension(30) :: tmp_thet,tmp_nyqv
      real :: a1,a2,b1,b2
      integer,dimension(nelv_vel) :: tmp_k

      ireftim = 9999999
      itime = ireftim
!----------------------------------------------------------------
!     make a index of each tilt:
!     for the almost same tilt scanning, the max Nyquist velocity
!     tilt will be the first present of this tilt(+-0.21degree).
!     the others will be second to dealiase.
!----------------------------------------------------------------
      tmp_k=-10
      k_121=-10
      do k=1,nelv_vel
         tmp_thet(k)=strct_in_vel(k)%elev_angle
         tmp_nyqv(k)=strct_in_vel(k)%nyq_vel
      enddo

      kstart=1
10    continue
      if ( tmp_thet(kstart)<spval ) then
        a1=tmp_thet(kstart)
        b1=tmp_nyqv(kstart)
        kreal=kstart
        do k=kstart,nelv_vel
          a2=tmp_thet(k)
          if ( abs(a2-a1)<=0.21 ) then
            tmp_thet(k)=spval
            b2=tmp_nyqv(k)
            if ( b2>b1 ) then
              b1=b2
              kreal=k
            endif
          endif
        enddo
        tmp_thet(kstart)=spval
      endif
      tmp_k(kreal)=kreal
      kstart=kstart+1
      if ( kstart>nelv_vel ) go to 20
      go to 10
20    continue

      do k=1,nelv_vel
         tmp_thet(k)=strct_in_vel(k)%elev_angle
         tmp_nyqv(k)=strct_in_vel(k)%nyq_vel
      enddo

      kstart=1
30    continue
      a1=100.0
      k1=0
      do k=1,nelv_vel
        if ( tmp_k(k)>0 ) then
          kk=tmp_k(k)
          a2=tmp_thet(kk)
          if ( a2<a1 ) then
            a1=a2
            kreal=kk
            k1=kk
          endif
        endif
      enddo
      if ( k1>0 ) then
        k_121(kstart)=kreal
        tmp_k(kreal)=-10
      endif
      kstart=kstart+1
      if ( kstart>nelv_vel ) go to 40
      go to 30
   40 continue
      do k=1,nelv_vel
        print*,'k121 infor:',k,k_121(k)
      enddo

      RETURN
      END SUBROUTINE get_k121
