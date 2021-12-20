!#######################################################################
!#######################################################################
!####                                                               ####
!####                Subroutine gradient_fts_0Vr                    ####
!####                                                               ####
!#######################################################################
!#######################################################################

      subroutine gradient_fst_0Vr( first_tilt                        &
                                  ,priority_small_phi                &
                                  ,priority_large_phi                &
                                  ,priority_crit                     &
                                  ,priority_level )
 
!     ==================================================================
!     PURPOSE:
!       Using the gradient of the small Vr to decide if the 0Vr is
!       the real one.
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
      logical :: first_tilt
      real    :: priority_small_phi
      real    :: priority_large_phi
      real    :: priority_crit
      integer :: priority_level

      integer, dimension(np)  :: indx_minvr
      real,    dimension(np)  :: phi_minvr
      integer :: j1_minvr,j2_minvr
      real    :: small_phi,large_phi,crit
      integer :: jm,ncount,jj1,jj2
      real    :: a1,a2,a3,a4,a5,b1,b2,b3

      integer :: i,ii,j,jj,k,m,n
      real, dimension(1:np) :: veltmp1,veltmp2
      integer :: goodp,goodm,n1,n2,nmax,nmin,msum
      real,dimension(20) :: vp_gradient,pp_gradient
      real,dimension(20) :: vm_gradient,pm_gradient
      real,dimension(500) :: vel_small,phi_small
      real :: alfa,beta,rr
      logical :: dojob
      real,dimension(20) :: good_small_phi,good_large_phi
      real,dimension(20) :: good_small_beta,good_large_beta
      character(len=40)  :: name1,name2

!     ##################################################################
!     Execute. Give some parameters.
!     ##################################################################
      j1_minvr=999
      j2_minvr=999
      small_phi=spval
      large_phi=spval
      dojob=.false.

!     ==================================================================
!     do tilt loop, from higher to lower. which may be better to get
!     the 0 Vr points.
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
             veltmp2(j)=veltmp1(j)
          enddo

!         ==============================================================
!         choise all the minimum abs(vr).
!         ==============================================================
          ncount=0
   10     continue
          a1=100.0
          do jm=1, nbeam
            if ( abs(veltmp1(jm)).le.a1 ) then
              a1 = abs(veltmp1(jm))
              m=jm
            endif
          enddo

          if ( a1 > 2.0 ) go to 20    !!! exit gate (need more test)

          ncount=ncount+1
          phi_minvr(ncount)=phi(m,k)
          indx_minvr(ncount)=m

!         ==============================================================
!         filling in the miss value around the min Vr point to avoid
!         second chosen in searching process.
!         ==============================================================
          do jm =-7,7
            j=m+jm
            if ( j<=0 ) j=nbeam+j
            if ( j>nbeam) j=j-nbeam
              veltmp1(j)=spval
          enddo
          go to 10
   20     continue     ! out of searching, do next step.

          if ( ncount>1 .and. ncount<=15 ) then   ! min Vr number is OK

!           ============================================================
!           calculate the gradient at the 0Vr points.
!           ============================================================
            goodp=0
            goodm=0
            do n=1, ncount
               m=indx_minvr(n)
               msum=0
               a3=0.0
               do jm=-15,15
!              do jm=-7,7
                  a3=a3+1.0
                  j=m+jm
                  if ( j>0 .and. j<nbeam ) then
                       if ( abs(veltmp2(j))<spval ) then
                            msum=msum+1
                            vel_small(msum)=veltmp2(j)
                            phi_small(msum)=phi(j,k)
                       endif
                  elseif ( j<=0 ) then
                       j=j+nbeam
                       if ( abs(veltmp2(j))<spval ) then
                            msum=msum+1
                            vel_small(msum)=veltmp2(j)
                            phi_small(msum)=phi(j,k)-360.0
                       endif
                  elseif ( j>nbeam ) then
                       j=j-nbeam
                       if ( abs(veltmp2(j))<spval ) then
                            msum=msum+1
                            vel_small(msum)=veltmp2(j)
                            phi_small(msum)=phi(j,k)+360.0
                       endif
                  endif
               enddo
!              =========================================================
!              sign check.
!              =========================================================
               a1=0.0
               a2=0.0
               do i=1,msum
                  if ( vel_small(i)>=0.0 ) then
                       a1=a1+1.0
                  else
                       a2=a2+1.0
                  endif
               enddo

               if ( a1/a3<0.8 .and. a2/a3<0.8 ) then
                 if ( msum>10 ) then
                    call line_regression( msum,vel_small,phi_small   &
                                         ,small_phi,spval            &
                                         ,vnyq_min                   &
                                         ,alfa,beta,rr )
                    if ( beta>=0.1 ) then
                        goodp=goodp+1
                        vp_gradient(goodp)=beta
                        pp_gradient(goodp)=small_phi
                    endif
                    if ( beta<=-0.1 ) then
                        goodm=goodm+1
                        vm_gradient(goodm)=beta
                        pm_gradient(goodm)=small_phi
                    endif
                 endif
               endif
            enddo

            if ( goodp==0 .or. goodm==0 ) go to 200

!           ============================================================
!           sort following v_gradient.
!           ============================================================
            call sort2(goodp,vp_gradient,pp_gradient)
            call sort2(goodm,vm_gradient,pm_gradient)
            print*,' '
            do n=1,goodp
               print*,'pp=',vp_gradient(n),pp_gradient(n),ilevel,k
            enddo
            print*,' '
            do n=1,goodm
               print*,'mm=',vm_gradient(n),pm_gradient(n),ilevel,k
            enddo
            print*,' '

!           ============================================================
!           searching the bigest abs(beta) and nearest opposition.
!           ============================================================
            if ( vp_gradient(goodp)>abs(vm_gradient(1)) ) then
                 a1=pp_gradient(goodp)
                 do n=1,goodm
                    a2=pm_gradient(n)
                    if ( a1>a2 ) then
                         a3=a2+180.0
                         if ( abs(a1-a3)<=7.5 ) then
                              small_phi=a2
                              large_phi=a1
                              dojob=.true.
                              exit
                         endif
                    else
                         a3=a2-180.0
                         if ( abs(a1-a3)<=7.5 ) then
                              small_phi=a1
                              large_phi=a2
                              dojob=.true.
                              exit
                         endif
                    endif
                 enddo
            else
                 a1=pm_gradient(1)
                 do n=goodp,1,-1
                    a2=pp_gradient(n)
                    if ( a1>a2 ) then
                         a3=a2+180.0
                         if ( abs(a1-a3)<=7.5 ) then
                              small_phi=a2
                              large_phi=a1
                              dojob=.true.
                              exit
                         endif
                    else
                         a3=a2-180.0
                         if ( abs(a1-a3)<=7.5 ) then
                              small_phi=a1
                              large_phi=a2
                              dojob=.true.
                              exit
                         endif
                    endif
                 enddo
            endif

            if ( dojob ) then

!             ==========================================================
!             the suitable couple minimum Vr points and the tendency
!             of the field was found. Put the angle and critial value
!             as the referent value for next level.
!             ==========================================================
              priority_small_phi=small_phi
              priority_large_phi=large_phi
              priority_crit=1.0

              call decide_first_tilt( priority_small_phi             &
                                     ,priority_large_phi             &
                                     ,priority_crit                  &
                                     ,first_tilt )

              if ( first_tilt ) then
                   dojob=.false.
                   small_phi=spval
                   large_phi=spval
                   j1_minvr=999
                   j2_minvr=999
                   priority_small_phi=spval
                   priority_large_phi=spval
                   priority_crit=spval
                   priority_level=999
              else
                   priority_level=ilevel
                   print*,' '
                   print*,small_phi,large_phi,k,priority_crit
                   print*,' '
                   end_tilt=1

!                  name2='grdt.dat'
!                  open(31,file=name2)
!                  write(31,'(3f10.3,2i4)') small_phi,large_phi      &
!                                          ,priority_crit,ilevel,k
!                  write(31,*)
!                  do n=1,goodp
!                    write(31,'(i4,e15.5,f10.3)') n,vp_gradient(n)   &
!                                                ,pp_gradient(n)
!                  enddo
!                  write(31,*)
!                  do n=1,goodm
!                    write(31,'(i4,e15.5,f10.3)') n,vm_gradient(n)   &
!                                                ,pm_gradient(n)
!                  enddo
!                  close(31)

                   RETURN
              endif

            endif       ! endif angle searching
  200     continue
          else
              count_toomuch_0Vr(k,ilevel)=1
          endif        ! endif the suitable ncount
        endif      ! endif select circle
      enddo      ! enddo k_tilt

      return
      end subroutine gradient_fst_0Vr
