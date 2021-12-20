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
!       re-convert the tradition style data to the structure style data.
!
!     Author:Shun Liu
!     time  :09/15/2003
!
!     Modify History:
!
!=======================================================================
      SUBROUTINE convert4( nelv_ref,nelv_vel                         &
                          ,strct_in_ref,strct_in_vel,strct_in_sw     &
                          ,strct_in_rho,strct_in_phi,strct_in_zdr )

      use sdata
      use variable_define  
 
      implicit none

      integer                     :: nelv_ref,nelv_vel
      type(ccout_strct)           :: strct_in_ref(nelv_ref)
      type(ccout_strct)           :: strct_in_vel(nelv_vel)
      type(ccout_strct)           :: strct_in_sw(nelv_vel)
      type(ccout_strct)           :: strct_in_rho(nelv_ref)
      type(ccout_strct)           :: strct_in_phi(nelv_ref)
      type(ccout_strct)           :: strct_in_zdr(nelv_ref)

      integer :: i,j,k,kk,ks,ii,it

      rf_mark=0
      ks=0
      DO kk=1,nelv_vel
        if ( k_121(kk)>0 ) then
          ks=ks+1
          k=k_121(kk)
          strct_in_vel(k)%fstgatdis  = rfstgat
          strct_in_vel(k)%num_gate   = imaxrng(ks)
          strct_in_sw(k)%fstgatdis  = rfstgat
          strct_in_sw(k)%num_gate   = imaxrng(ks)
          strct_in_rho(k)%fstgatdis  = rfstgat
          strct_in_rho(k)%num_gate   = imaxrng(ks)
          strct_in_phi(k)%fstgatdis  = rfstgat
          strct_in_phi(k)%num_gate   = imaxrng(ks)
          strct_in_zdr(k)%fstgatdis  = rfstgat
          strct_in_zdr(k)%num_gate   = imaxrng(ks)
          DO j=1,nphi(ks)
            nrang=imaxrng(ks)
            DO i = iminrng,nrang
              strct_in_vel(k)%field(i,j)=vel(i,j,ks)
              strct_in_sw(k)%field(i,j)=swg(i,j,ks)
              strct_in_rho(k)%field(i,j)=rho(i,j,ks)
              strct_in_phi(k)%field(i,j)=kdp(i,j,ks)
              strct_in_zdr(k)%field(i,j)=zdr(i,j,ks)
            ENDDO
          ENDDO

          strct_in_ref(k)%num_beam   = nphi2(ks)
          strct_in_ref(k)%num_gate   = imaxrng2(ks)
          strct_in_ref(k)%fstgatdis  = rfstgat2
          strct_in_ref(k)%gateWidth(100) = refgatsp2

          if(1==0)then
          DO j=1,nphi2(ks)
            nrang2=imaxrng2(ks)
            DO i = iminrng2,nrang2
!             strct_in_ref(k)%field(i,j)=ref(i,j,ks)
             if(j<=nphi(ks))then
              ii=(i-1)*4+1
              if(ii<=nrang-5)then
                strct_in_ref(k)%field(i,j)=ref(ii,j,ks)
              end if
             end if
            ENDDO
          ENDDO
          end if

          IF(dual_index_qc)THEN
          nrang2=imaxrng2(ks)
          DO j=1,nphi2(ks)
            DO i = iminrng2,nrang2
            if(j<=nphi(ks))then
              ii=(i-1)*4+1
              if(ii<=nrang-5)then
                rf_mark(i,j,k)=0
                do it=ii,ii+3
!                 if(no_mark(it,j,k)>spval.and.strct_in_ref(k)%field(i,j)<10.0)rfmark=1
!                 if(no_mark(it,j,k)==999.2 .and. i<30 )rfmark=1
!                 if(no_mark(it,j,k)==999.7)rfmark=1
!                 if(no_mark(it,j,k)==999.6)rfmark=1
                  if ( no_mark(it,j,k)==999.7 .or. no_mark(it,j,k)==999.2) rf_mark(i,j,k)=1
                end do
!               if(rf_mark(i,j,k)==1)strct_in_ref(k)%field(i,j)=spval
              end if
            end if
            ENDDO
          ENDDO
          END IF

        endif
      ENDDO

      return
      END subroutine convert4
