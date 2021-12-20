      SUBROUTINE convert2_two( nelv_ref,nelv_vel                     &
                              ,strct_in_ref,strct_in_vel,extro_job )

      use sdata
      use variable_define

      implicit none

      integer            :: nelv_ref, nelv_vel
      type(ccout_strct)  :: strct_in_ref(nelv_ref)
      type(ccout_strct)  :: strct_in_vel(nelv_vel)
      logical            :: extro_job      

      integer :: i,j,k,kk,ks
      real :: a1,a2,b1,b2

      ireftim = 9999999
      itime = ireftim
!----------------------------------------------------------------
!     put the k_121=-10 tilt data to a new 3D array.
!----------------------------------------------------------------
      do k=1,nelv_vel
         if ( k_121(k)<=1 ) then
              extro_job=.true.
              exit
         endif
      enddo

      IF ( .not. extro_job ) RETURN

      ref=spval
      vel=spval

      ks=0
      do k=1,nelv_vel
        if ( k_121(k)==1 ) then
          ks=ks+1
          wrt_thet(ks)=strct_in_vel(k)%elev_angle
          vcpnum  = strct_in_ref(1)%vcpnum
          iyr(ks)  = strct_in_vel(k)%year
          imon(ks) = strct_in_vel(k)%month
          iday(ks) = strct_in_vel(k)%day
          ihr(ks)  = strct_in_vel(k)%hour
          imin(ks) = strct_in_vel(k)%minute
          isec(ks) = strct_in_vel(k)%second

          iminrng = 1
          imaxrng(ks) = strct_in_vel(k)%num_gate

          iminrng2 = 1
          imaxrng2(ks) = strct_in_ref(k)%num_gate

          thet (ks)= strct_in_vel(k)%elev_angle
          thet2(ks)= strct_in_ref(k)%elev_angle
          nphi (ks)= strct_in_vel(k)%num_beam
          nphi2(ks)= strct_in_ref(k)%num_beam
          thet_nyq_vel(ks)= strct_in_vel(k)%nyq_vel

          radlat=strct_in_vel(k)%radlat
          radlon=strct_in_vel(k)%radlon
          radelv=strct_in_vel(k)%radhgt

          rfstgat =  strct_in_vel(k)%fstgatdis
          rfstgat2 =  strct_in_ref(k)%fstgatdis
          refgatsp = strct_in_vel(k)%gateWidth(100)
          refgatsp2 = strct_in_ref(k)%gateWidth(100)

          DO j= 1,nphi(ks)
             phi(j,ks) = strct_in_vel(k)%azim(j)
          ENDDO
          DO j= 1,nphi2(ks)
             phi2(j,ks) = strct_in_ref(k)%azim(j)
          ENDDO

          DO j=1,nphi2(ks)
            DO i = iminrng2,imaxrng2(ks)
              ref(i,j,ks) = strct_in_ref(k)%field(i,j)
            ENDDO
          ENDDO
          DO j=1,nphi(ks)
            DO i = iminrng,imaxrng(ks)
              vel(i,j,ks) = strct_in_vel(k)%field(i,j)
            ENDDO
          ENDDO
        endif
      enddo

      nthet=ks
      print*,'nthet==',nthet,radlat,radlon,radelv

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
               IF(vel(i,j,k).le.-100.0)THEN
                  vel(i,j,k) = spval
               ENDIF
            ENDDO
         ENDDO
      ENDDO

      RETURN
      END SUBROUTINE convert2_two
