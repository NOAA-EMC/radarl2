!#######################################################################
!#######################################################################
!####                                                               ####
!####                Subroutine get_u0v0                            ####
!####                                                               ####
!#######################################################################
!#######################################################################

      subroutine get_u0v0( vmax,crit,consistency,mcount )
 
!     ==================================================================
!     PURPOSE:
!       using the fine abs(V) to get the u0, v0,w0. 
!     ==================================================================
!
!     Author :  Kang Nai
!     Date   :  Oct. 16, 2007
!     Action :  Created.
!
!     History:
!     --------
!     Nov. 13, 2007  Kang Nai added a consistent check.
!
!     ------------------------------------------------------------------

      use variable_define
      implicit none
!
      real    :: vmax,crit
      logical :: consistency
      integer :: mcount

      integer :: i,ii,j,jj,k,kk,m,mm,n,nn
      real    :: cos_elv,twonyq,thrths
      real    :: a1,a2,a3,a4,a5,b1,b2,c1,c2,c3,d1,d2,d3
      character(len=80) :: name1,name2

      real, dimension(1:np) :: veltmp1
      real, dimension(1:2*np) :: veltmp2,veltmp3
      real    :: alfa0,alfa1,beta0,beta1
      real    :: tmp_angle1,tmp_angle2,w0,dlta_v
      real    :: vr_sign
      integer :: ja,jb,jk,jm,jn,jo,jp,jq,jr,js,jspval,jfalse
      integer :: pc

!     ##################################################################
!     Execute. Give some parameters.
!     ##################################################################
      vr_sign=1.0
      if ( vmax*crit<0.0 ) then
           vr_sign=-1.0
      endif

      alfa0=check_small_phi(ilevel)
      alfa1=check_large_phi(ilevel)
      beta0=0.5*(alfa1+alfa0-180.0)
      beta1=0.5*(alfa1+alfa0)
      tmp_angle2=(alfa0-beta0)*rdn
!     vabs(ilevel)=vr_sign*vmax
      ustor(ilevel)=vr_sign*vmax*cos((90.0-beta1)*rdn)
      vstor(ilevel)=vr_sign*vmax*sin((90.0-beta1)*rdn)
      w0=-vr_sign*vmax*sin(tmp_angle2)

!     if ( vnyq_min>5.0 ) return

!     ==================================================================
!     calculate the a0 at differ tilt.
!     ==================================================================
!     do k_tilt=start_tilt,end_tilt,step_tilt
!       k=k_tilt
!       if ( select_circ(k,ilevel)>0 ) then

!         nbeam = nphi(k)
!         nrang = imaxrng(k)
!         elvng = thet(k)
!         twonyq=2.0*thet_nyq_vel(k)
!         thrths=0.5*twonyq
!         cos_elv=cos(thet(k)*rdn)
!         wstor(k,ilevel)=w0*cos_elv

!         ii = select_circ(k,ilevel)
!         range = ran(ii)
!         veltmp1=spval
!         do j=1,nbeam
!            veltmp1(j)=vel(ii,j,k)
!         enddo

!         veltmp2=spval
!         veltmp3=spval
!         do j=1,nbeam
!            tmp_angle1=(phi(j,k)-beta0)*rdn
!            a1= sin(tmp_angle1)-sin(tmp_angle2)
!            a2= vr_sign*vmax*a1*cos_elv
!            veltmp3(j)=a2
!            if ( abs(veltmp1(j))<spval ) then
!              dlta_v=veltmp1(j)-a2
!              if ( dlta_v>=0.0 ) then
!                   n=int(dlta_v/twonyq+0.5)
!              else
!                   n=int(dlta_v/twonyq-0.5)
!              endif
!              b2=(dlta_v-float(n)*twonyq)
!              b1=b2+a2
!              veltmp2(j)=b1
!              a5=phi(j,k)
!            endif
!            if (ilevel==6) then
!            write(31,'(4f10.3,i4)') phi(j,k),veltmp1(j),veltmp2(j)  &
!                                   ,veltmp3(j),j
!            endif
!         enddo
!         do j=1,nbeam
!            jj=nbeam+j
!            veltmp2(jj)=veltmp2(j)
!            veltmp3(jj)=veltmp3(j)
!         enddo

!         ==============================================================
!         consistent check.
!         ==============================================================
!         do j=1,nbeam
!            if ( abs(veltmp2(j))<spval ) then
!                 a1=veltmp2(j)                 ! first available point
!                 ja=j
!                 exit
!            endif
!         enddo

!         consistency=.false.
!  10     continue

!         jb=ja+1                          ! loop parameter

!         if ( abs(veltmp2(jb))<spval ) then

!           a3=abs(veltmp2(ja)-veltmp2(jb))
!           if ( a3<=thrths ) then
!             ja=jb
!             consistency=.true.
!             if ( ja>=nbeam ) go to 20       ! pass consistent check
!             go to 10
!           else          !!! found one inconsistent points

!             -------------------------------------------------
!             do forward searching for a while.
!             -------------------------------------------------
!             jspval=0
!             jfalse=0
!             do jm=jb+1,2*nbeam
!                if ( abs(veltmp2(jm))<spval ) then
!                  c3=abs(veltmp2(ja)-veltmp2(jm))
!                  if ( c3<thrths ) then
!                    ja=jm
!                    consistency=.true.
!                    jspval=0
!                    jfalse=0
!                    exit
!                  else
!                    jfalse=jfalse+1
!                    consistency=.false.
!                    jspval=0
!                    if ( jfalse>=20 ) exit
!                  endif
!                else
!                  jspval=jspval+1
!                  consistency=.false.
!                  if ( jspval>=10 ) then
!                    jo=0
!                    do jn=jm,2*nbeam
!                       if ( abs(veltmp2(jn))<spval ) then
!                            jo=jn
!                            exit
!                       endif
!                    enddo
!                    if ( abs(veltmp2(jo)-veltmp3(jo))<0.5*thrths ) then
!                         ja=jo
!                         consistency=.true.
!                         exit
!                    else
!                         print*,'jo',veltmp2(jo),veltmp3(jo),jo,ja
!                         consistency=.false.
!                         exit
!                    endif
!                  endif
!                endif
!             enddo
!             if ( consistency ) then
!                  jp=ja+1
!                  if ( jp>nbeam ) go to 20    ! pass consistent check
!                  go to 10
!             else
!                  go to 20
!             endif
!           endif
!         else
!           ------------------------------------------------------------
!           missing data searching forward.
!           ------------------------------------------------------------
!           do jr=jb+1,2*nbeam
!              if ( abs(veltmp2(jr))<spval ) exit
!           enddo

!           d1=0.0
!           d2=0.0
!           do js=jr,jr+5
!              if ( abs(veltmp2(js))<spval ) then
!                   d1=d1+veltmp2(js)
!                   d2=d2+1.0
!              endif
!           enddo

!           d3=d1/d2

!           if ( abs(d3-veltmp3(jr))<0.5*thrths ) then
!                ja=jr
!                consistency=.true.
!           else
!                print*,'jr',jr,veltmp2(jr),veltmp3(jr),ja
!                consistency=.false.
!           endif
!           if ( consistency ) then
!                if (ja>=nbeam) go to 20
!                go to 10                           ! continue check
!           else
!                go to 20                           ! stop check
!           endif
!         endif
!  20     continue

!         if ( .not. consistency ) exit

!       endif      ! endif select and check_small_phi
!     enddo        ! enddo k_tilt

!     if ( .not. consistency ) then
!          vabs(ilevel)=spval
!          ustor(ilevel)=spval
!          vstor(ilevel)=spval
!     endif

      return
      end subroutine get_u0v0
