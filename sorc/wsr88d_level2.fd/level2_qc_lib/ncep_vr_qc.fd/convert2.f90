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
!       convert the structure style data to tradition style.
!
!     Author:Shun Liu
!     time  :09/15/2003
!
!     Modify History:
!
!     Pengfei Zhang
!       put the seperated scan of radial velocity and reflectivity
!       togeter at 0.5, 1.5 elevation angle.
!
!=======================================================================

      SUBROUTINE convert2( nelv_ref,nelv_vel                         &
                          ,strct_in_ref,strct_in_vel,strct_in_sw     &
                          ,strct_in_rho,strct_in_phi,strct_in_zdr )

      use sdata
      use variable_define

      implicit none

      integer                     :: nelv_ref, nelv_vel
      type(ccout_strct)           :: strct_in_ref(nelv_ref)
      type(ccout_strct)           :: strct_in_vel(nelv_vel)
      type(ccout_strct)           :: strct_in_sw(nelv_vel)
      type(ccout_strct)           :: strct_in_rho(nelv_ref)
      type(ccout_strct)           :: strct_in_phi(nelv_ref)
      type(ccout_strct)           :: strct_in_zdr(nelv_ref)

      integer :: i,j,k,kk,kreal,ks,kstart,k1,i0,ii,ip,itmp
      real,dimension(30) :: tmp_thet,tmp_nyqv
      real :: a1,a2,b1,b2
      integer,dimension(nelv_vel) :: tmp_k

      real :: vel_range,ref_range,ri,rp

      ireftim = 9999999
      itime = ireftim

      allocate ( ref  (1:nr2,1:np,1:nt) )
      allocate ( vel  (1:nr,1:np,1:nt) )
      allocate ( swg  (1:nr,1:np,1:nt) )
      allocate ( rho  (1:nr,1:np,1:nt) )
      allocate ( kdp  (1:nr,1:np,1:nt) )
      allocate ( zdr  (1:nr,1:np,1:nt) )
      allocate ( no_mark  (1:nr,1:np,1:nt) )
      allocate ( vr_mark  (1:nr,1:np,1:nt) )
      allocate ( rf_mark  (1:nr2,1:np,1:nt) )
      allocate( wrkhgt(1:nr,1:np) )

      ref=spval
      vel=spval
      swg=spval
      rho=spval
      kdp=spval
      zdr=spval
      no_mark=0.0

      ks=0
      do kk=1,nelv_vel
        if ( k_121(kk)>0 ) then
          k=k_121(kk)
          ks=ks+1

          vcpnum  = strct_in_ref(1)%vcpnum
          iyr(ks)  = strct_in_vel(k)%year
          imon(ks) = strct_in_vel(k)%month
          iday(ks) = strct_in_vel(k)%day
          ihr(ks)  = strct_in_vel(k)%hour
          imin(ks) = strct_in_vel(k)%minute
          isec(ks) = strct_in_vel(k)%second
          radlat=strct_in_vel(k)%radlat
          radlon=strct_in_vel(k)%radlon
          radelv=strct_in_vel(k)%radhgt

          iminrng = 1
          itmp = strct_in_vel(k)%num_gate
          imaxrng(ks) = min(itmp,nr)
          thet (ks)= strct_in_vel(k)%elev_angle
          nphi (ks)= strct_in_vel(k)%num_beam
          thet_nyq_vel(ks)= strct_in_vel(k)%nyq_vel
          rfstgat =  strct_in_vel(k)%fstgatdis
          refgatsp = strct_in_vel(k)%gateWidth(100)
          DO j= 1,nphi(ks)
             phi(j,ks) = strct_in_vel(k)%azim(j)
          ENDDO
          DO j=1,nphi(ks)
            DO i = iminrng,imaxrng(ks)
              vel(i,j,ks) = strct_in_vel(k)%field(i,j)
            ENDDO
          ENDDO
          DO j=1,nphi(ks)
            DO i = iminrng,imaxrng(ks)
              swg(i,j,ks) = strct_in_sw(k)%field(i,j)
            ENDDO
          ENDDO
          DO j=1,nphi(ks)
            DO i = iminrng,imaxrng(ks)
              rho(i,j,ks) = strct_in_rho(k)%field(i,j)
            ENDDO
          ENDDO
          DO j=1,nphi(ks)
            DO i = iminrng,imaxrng(ks)
              kdp(i,j,ks) = strct_in_phi(k)%field(i,j)
            ENDDO
          ENDDO
          DO j=1,nphi(ks)
            DO i = iminrng,imaxrng(ks)
              zdr(i,j,ks) = strct_in_zdr(k)%field(i,j)
            ENDDO
          ENDDO
          write(*,*) ks,k,thet(ks),thet_nyq_vel(ks)                  &
                    ,nphi(ks),imaxrng(ks)

          iminrng2 = 1
          itmp = strct_in_ref(k)%num_gate
          imaxrng2(ks) = min(itmp,nr2)
          thet2(ks)= strct_in_ref(k)%elev_angle
          nphi2(ks)= strct_in_ref(k)%num_beam
          rfstgat2 =  strct_in_ref(k)%fstgatdis
          refgatsp2 = strct_in_ref(k)%gateWidth(100)
          DO j= 1,nphi2(ks)
             phi2(j,ks) = strct_in_ref(k)%azim(j)
          ENDDO
          DO j=1,nphi2(ks)
            ri=(float(iminrng)-1.0) 
            DO i = iminrng,imaxrng(ks)
              vel_range=rfstgat+ri*refgatsp
              a1=10000.0
              rp=(float(iminrng2)-1.0)
              DO ip = iminrng2,imaxrng2(ks)
                ref_range=rfstgat2+rp*refgatsp2
                if ( abs(vel_range-ref_range)<a1 ) then
                  a1=abs(vel_range-ref_range)
                  a2=strct_in_ref(k)%field(ip,j)
                endif
                rp=rp+1.0
              enddo
              ref(i,j,ks) = a2
              ri=ri+1.0
            ENDDO
          ENDDO
          imaxrng2(ks) = imaxrng(ks)
          rfstgat2 = rfstgat
          refgatsp2 = refgatsp
        endif
      enddo

      nthet=ks
      print*,'nthet==',nthet

      DO k=1,nthet
        DO j=1,nphi2(k)
          DO i = iminrng2,imaxrng2(k)
            IF(ref(i,j,k).le.-100.0)THEN
              ref(i,j,k) = spval
            ENDIF
          ENDDO
        ENDDO
        DO j=1,nphi(k)
          DO i = iminrng,imaxrng(k)
            IF(rho(i,j,k).le.-100.0)THEN
              rho(i,j,k) = spval
            ENDIF
          ENDDO
        ENDDO
        DO j=1,nphi(k)
          DO i = iminrng,imaxrng(k)
            IF(kdp(i,j,k).le.-100.0)THEN
              kdp(i,j,k) = spval
            ENDIF
          ENDDO
        ENDDO
        DO j=1,nphi(k)
          DO i = iminrng,imaxrng(k)
            IF(zdr(i,j,k).le.-100.0)THEN
              zdr(i,j,k) = spval
            ENDIF
          ENDDO
        ENDDO
        DO j=1,nphi(k)
          DO i = iminrng,imaxrng(k)
            IF(vel(i,j,k).le.-100.0)THEN
              vel(i,j,k) = spval
            ENDIF
          ENDDO
        ENDDO
        DO j=1,nphi(k)
          DO i = iminrng,imaxrng(k)
            IF(swg(i,j,k).le.-100.0)THEN
              swg(i,j,k) = spval
            ENDIF
          ENDDO
        ENDDO
      ENDDO
      print*,'end of convert2'

      RETURN
      END SUBROUTINE convert2
