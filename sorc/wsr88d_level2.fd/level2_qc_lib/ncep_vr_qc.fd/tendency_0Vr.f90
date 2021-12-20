!#######################################################################
!#######################################################################
!####                                                               ####
!####                Subroutine tendency_0Vr                        ####
!####                                                               ####
!#######################################################################
!#######################################################################

      subroutine tendency_0Vr( priority_small_phi                    &
                              ,priority_large_phi                    &
                              ,priority_crit                         &
                              ,priority_level                        &
                              ,extend_small_phi                      &
                              ,extend_large_phi                      &
                              ,no_extend )
 
!     ==================================================================
!     PURPOSE:
!       Put the data around the 0Vr into the statistics box. then get
!       the united 0Vr points.
!     ==================================================================
!
!     Author :  Kang Nai
!     Date   :  Oct. 15, 2007
!     Action :  Created.
!
!     History:
!     --------
!
!     ------------------------------------------------------------------

      use variable_define
      implicit none
!
      real    :: priority_small_phi
      real    :: priority_large_phi
      real    :: priority_crit
      integer :: priority_level
      integer :: extend_small_phi
      integer :: extend_large_phi
      integer :: no_extend

      integer :: i,ii,j,jj,k,m,msum,n,nsum

      integer :: jm
      integer :: jj1,jj2
      real    :: a1,a2,a3,a4,a5,b1,b2
      real    :: small_phi,large_phi
      real    :: alfa,beta1,beta2,rr

      real, dimension(1:np) :: veltmp1

      real, dimension(500) :: vel_small,vel_large,phi_small,phi_large
      character(len=40) :: name1,name2

!     ##################################################################
!     Execute.
!     ##################################################################
!*
      if ( (ilevel-priority_level)>=20 ) then
           no_extend=20
           RETURN
      endif

!     ==================================================================
!     get the initial counter value.
!     ==================================================================
      msum=0
      nsum=0

!     ==================================================================
!     do tilt loop, from higher to lower.
!     ==================================================================
      do k_tilt=start_tilt,end_tilt,step_tilt
        k=k_tilt
        if ( select_circ(k,ilevel) > 0 ) then       ! if select_circ>0
          nbeam = nphi(k)
          nrang = imaxrng(k)
          elvng = thet(k)

          ii = select_circ(k,ilevel)
          do j=1,nbeam
             veltmp1(j)=vel(ii,j,k)
          enddo

!         ==============================================================
!         searching the phi which is nearest the priority_small_phi.
!         ==============================================================
          a1=360.0
          do jm=1,nbeam
            a2=phi(jm,k)
            if ( abs(a2-priority_small_phi)<a1 ) then
              a1 = abs(a2-priority_small_phi)
              jj1=jm
            endif
          enddo

!         =============================================================
!         searching the phi which is nearest the priority_large_phi.
!         =============================================================
          a1=360.0
          do jm=1,nbeam
             a2=phi(jm,k)
             if ( abs(a2-priority_large_phi)<a1 ) then
               a1 = abs(a2-priority_large_phi)
               jj2=jm
             endif
          enddo

!         ==============================================================
!         put the data near the small phi into statistics one box.
!         ==============================================================
          do jm=-15,15
             j=jj1+jm
             if ( j<=0 ) then
                  j=j+nbeam
                  if ( abs(veltmp1(j))<spval ) then
                       msum=msum+1
                       vel_small(msum)=veltmp1(j)
                       phi_small(msum)=phi(j,k)-360.0
                  endif
             elseif ( j>nbeam ) then
                  j=j-nbeam
                  if ( abs(veltmp1(j))<spval ) then
                       msum=msum+1
                       vel_small(msum)=veltmp1(j)
                       phi_small(msum)=phi(j,k)+360.0
                  endif
             else
                  if ( abs(veltmp1(j))<spval ) then
                       msum=msum+1
                       vel_small(msum)=veltmp1(j)
                       phi_small(msum)=phi(j,k)
                  endif
             endif
          enddo

!         ==============================================================
!         put the data near the large phi into statistics two box.
!         ==============================================================
          do jm=-15,15
             j=jj2+jm
             if ( j<=0 ) then
                  j=j+nbeam
                  if ( abs(veltmp1(j))<spval ) then
                       nsum=nsum+1
                       vel_large(nsum)=veltmp1(j)
                       phi_large(nsum)=phi(j,k)-360.0
                  endif
             elseif ( j>nbeam ) then
                  j=j-nbeam
                  if ( abs(veltmp1(j))<spval ) then
                       nsum=nsum+1
                       vel_large(nsum)=veltmp1(j)
                       phi_large(nsum)=phi(j,k)+360.0
                  endif
             else
                  if ( abs(veltmp1(j))<spval ) then
                       nsum=nsum+1
                       vel_large(nsum)=veltmp1(j)
                       phi_large(nsum)=phi(j,k)
                  endif
             endif
          enddo

        endif                ! endif select circle
      enddo           ! enddo k_tilt

!     ------------------------------------------------------------------
!     write the select data.
!     ------------------------------------------------------------------
!     write(name1,'(i4.4)') ilevel
!     name2='chos_small'//name1(1:4)//'.dat'
!     open(31,file=name2)
!     do i=1,msum
!        write(31,'(i6,2f10.3)') i,vel_small(i),phi_small(i)
!     enddo
!     close(31)
!     name2='chos_large'//name1(1:4)//'.dat'
!     open(31,file=name2)
!     do i=1,nsum
!        write(31,'(i6,2f10.3)') i,vel_large(i),phi_large(i)
!     enddo
!     close(31)

      if ( msum<30 .and. nsum<30 ) then    ! both phis are not quality
           no_extend=no_extend+1
           RETURN
      endif

!     ==================================================================
!     check the choise data's distribution.
!     ==================================================================
      a1=0.0
      do i=1,msum
         if ( abs(vel_small(i))<=10.0 ) a1=a1+1.0
      enddo
      a2=a1/float(msum)

      if ( a2<0.6 ) RETURN

      a1=0.0
      do i=1,nsum
         if ( abs(vel_large(i))<=10.0 ) a1=a1+1.0
      enddo
      a2=a1/float(nsum)

      if ( a2<0.6 ) RETURN

!     ==================================================================
!     searching the small phi and large phi.
!     ==================================================================
      if ( msum<30 .and. nsum>=30 ) then   ! large phi is quality
           call line_regression ( nsum,vel_large,phi_large           &
                                 ,large_phi,spval                    &
                                 ,vnyq_min                           &
                                 ,alfa,beta2,rr )
           a1=large_phi-priority_large_phi
           if ( priority_small_phi>large_phi ) then
                a1=large_phi-priority_large_phi+360.0
           endif
           small_phi=priority_small_phi+a1
           extend_small_phi=extend_small_phi+1
           no_extend=0

      elseif ( msum>=30 .and. nsum<30 ) then  ! small_phi is qualify
           call line_regression ( msum,vel_small,phi_small           &
                                 ,small_phi,spval                    &
                                 ,vnyq_min                           &
                                 ,alfa,beta1,rr )
           a1=small_phi-priority_small_phi
           if ( priority_large_phi<small_phi ) then
                a1=small_phi-priority_small_phi-360.0
           endif
           large_phi=priority_large_phi+a1
           extend_large_phi=extend_large_phi+1
           no_extend=0

      else                                     ! both phis are qualify

           call line_regression ( msum,vel_small,phi_small           &
                                 ,small_phi,spval                    &
                                 ,vnyq_min                           &
                                 ,alfa,beta1,rr )

           call line_regression ( nsum,vel_large,phi_large           &
                                 ,large_phi,spval                    &
                                 ,vnyq_min                           &
                                 ,alfa,beta2,rr )
           extend_small_phi=0
           extend_large_phi=0
           no_extend=0

      endif

      if ( small_phi>900.0 .or. large_phi>900.0 ) RETURN
      if ( abs(small_phi-large_phi)<90.0 ) then
           no_extend=20
           RETURN
      endif

!     ==================================================================
!     adjust the small phi and large phi in case one of them
!     cross the 360 degree.
!     ==================================================================
      if ( small_phi>large_phi ) then
           b1=small_phi
           b2=large_phi
           small_phi=b2
           large_phi=b1
           priority_crit=-priority_crit
           b1=beta1
           b2=beta2
           beta1=b2
           beta2=b1
      endif

!     if ( abs(small_phi-priority_small_phi)>=40.0 .or.              &
!          abs(large_phi-priority_large_phi)>=40.0 ) then
!          no_extend=20
!          RETURN
!     endif

!     ==================================================================
!     the small_phi and large_phi will be the reference for next level. 
!     ==================================================================
      priority_small_phi=small_phi
      priority_large_phi=large_phi
      priority_level=ilevel
      check_small_phi(ilevel)=small_phi
      check_large_phi(ilevel)=large_phi
      check_crit(ilevel)=priority_crit

!     write(999,'(i4,3f10.3,2i6)') ilevel,small_phi,large_phi        &
!                                 ,priority_crit,msum,nsum
      return
      end subroutine tendency_0Vr
