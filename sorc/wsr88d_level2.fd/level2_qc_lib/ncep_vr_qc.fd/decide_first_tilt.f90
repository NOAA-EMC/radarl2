!#######################################################################
!#######################################################################
!####                                                               ####
!####                Subroutine decide_first_tilt                   ####
!####                                                               ####
!#######################################################################
!#######################################################################

      subroutine decide_first_tilt( priority_small_phi               &
                                   ,priority_large_phi               &
                                   ,priority_crit                    &
                                   ,first_tilt )
 
!     ==================================================================
!     PURPOSE:
!       Put the data around the 0Vr into the statistics box. then get
!       the united 0Vr points.
!     ==================================================================
!
!     Author :  Kang Nai
!     Date   :  Dec. 07, 2007
!     Action :  Created.
!
!     History:
!     --------
!     Dec. 18, 2007     Kang Nai added the data's distribution check.
!     ------------------------------------------------------------------

      use variable_define
      implicit none
!
      real    :: priority_small_phi
      real    :: priority_large_phi
      real    :: priority_crit
      logical :: first_tilt

      integer :: i,ii,j,jj,k,kp,m,msum,n,nsum

      integer :: jm
      integer :: jj1,jj2
      real    :: a1,a2,a3,a4,a5,a6
      real    :: small_phi,large_phi
      real    :: alfa,beta1,beta2,rr

      real, dimension(1:np) :: veltmp1

      real, dimension(500) :: vel_small,vel_large,phi_small,phi_large
      character(len=40) :: name1,name2

!     ##################################################################
!     Execute. Give some parameters.
!     ##################################################################
      msum=0
      nsum=0

!     ==================================================================
!     do tilt loop, from higher to lower.
!     ==================================================================
      do kp=start_tilt,1,step_tilt
        k=kp
        if ( select_circ(k,ilevel) > 0 ) then       ! if select_circ>0
          nbeam = nphi(k)
          nrang = imaxrng(k)
          elvng = thet(k)

          ii = select_circ(k,ilevel)
          range = ran(ii)
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

      if ( msum<30 .or. nsum<30 ) RETURN

!     ==================================================================
!     check the choise data's distribution.
!     ==================================================================
      a1=0.0
      a2=0.0
      a3=0.5*(vnyq_min+vnyq_max)
      a4=0.5*vnyq_max
      do i=1,msum
        if ( abs(vel_small(i))<=a3 ) a1=a1+1.0
        if ( abs(vel_small(i))<=a4 ) a2=a2+1.0
      enddo
      a5=a1/float(msum)
      a6=a2/float(msum)
      print*,'small points, available==', a5,ilevel,' half==',a6

      if ( a5<=0.85 ) RETURN
      if ( a6<=0.60 ) RETURN

      a1=0.0
      a2=0.0
      do i=1,nsum
        if ( abs(vel_large(i))<=a3 ) a1=a1+1.0
        if ( abs(vel_large(i))<=a4 ) a2=a2+1.0
      enddo
      a5=a1/float(nsum)
      a6=a2/float(nsum)
      print*,'large points, available==', a5,ilevel,' half==',a6

      if ( a5<=0.85 ) RETURN
      if ( a6<=0.60 ) RETURN

!     write(name1,'(i4.4)') ilevel
!     name2='slct_small'//name1(1:4)//'.dat'
!     open(31,file=name2)
!       do i=1,msum
!          write(31,'(i4,2f10.3)') i,vel_small(i),phi_small(i)
!       enddo
!     close(31)
!     name2='slct_large'//name1(1:4)//'.dat'
!     open(31,file=name2)
!       do i=1,msum
!          write(31,'(i4,2f10.3)') i,vel_large(i),phi_large(i)
!       enddo
!     close(31)

      call line_regression ( msum,vel_small,phi_small           &
                            ,small_phi,spval                    &
                            ,vnyq_min                           &
                            ,alfa,beta1,rr )

      call line_regression ( nsum,vel_large,phi_large           &
                            ,large_phi,spval                    &
                            ,vnyq_min                           &
                            ,alfa,beta2,rr )

      priority_crit=sign(1.0,beta1)

      if ( small_phi>900.0 .or. large_phi>900.0 ) RETURN

      first_tilt=.false.

      return
      end subroutine decide_first_tilt
