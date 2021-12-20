      SUBROUTINE convert4_two( nelv_ref,nelv_vel                     &
                              ,strct_in_ref,strct_in_vel )

      use sdata
      use variable_define  
 
      implicit none

      integer                     :: nelv_ref,nelv_vel
      type(ccout_strct)           :: strct_in_ref(nelv_ref)
      type(ccout_strct)           :: strct_in_vel(nelv_vel)

      integer :: i,j,k,kk,ks

      ks=0
      DO k=1,nelv_vel
        if ( k_121(k)==1 ) then
          ks=ks+1
          strct_in_vel(k)%radlat     = radlat
          strct_in_vel(k)%radlon     = radlon
          strct_in_vel(k)%radhgt     = radelv
          strct_in_vel(k)%fstgatdis  = rfstgat
          strct_in_vel(k)%num_beam   = nphi(ks)
          strct_in_vel(k)%num_gate   = imaxrng(ks)
          DO j=1,nphi(ks)
            strct_in_vel(k)%azim(j)=phi(j,ks)
            nrang=imaxrng(ks)
            DO i = iminrng,nrang
              strct_in_vel(k)%field(i,j)=vel(i,j,ks)
            ENDDO
          ENDDO
          strct_in_ref(k)%radlat     = radlat
          strct_in_ref(k)%radlon     = radlon
          strct_in_ref(k)%radhgt     = radelv
          strct_in_ref(k)%fstgatdis  = rfstgat2
          strct_in_ref(k)%num_beam   = nphi2(ks)
          strct_in_ref(k)%num_gate   = imaxrng2(ks)
          DO j=1,nphi2(ks)
            strct_in_ref(k)%azim(j)=phi2(j,ks)
            nrang2=imaxrng2(ks)
            DO i = iminrng2,nrang2
              strct_in_ref(k)%field(i,j)=ref(i,j,ks)
            ENDDO
          ENDDO
          print*,'ks=',ks,k                                        &
                ,strct_in_vel(k)%nyq_vel,strct_in_vel(k)%elev_angle
        elseif ( k_121(k)<0 ) then
          strct_in_vel(k)%field=spval
          strct_in_ref(k)%field=spval
        endif
      ENDDO

      return
      END subroutine convert4_two
