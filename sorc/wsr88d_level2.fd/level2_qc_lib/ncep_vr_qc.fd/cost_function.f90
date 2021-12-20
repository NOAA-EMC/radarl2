!#######################################################################
!#######################################################################
!####                                                               ####
!####                Subroutine cost_function                       ####
!####                                                               ####
!#######################################################################
!#######################################################################

      subroutine cost_function( vmax,sigma,crit )
 
!     ==================================================================
!     PURPOSE:
!       Using the estimate V to get the cost function.
!       The function's equation is:
!       J=sum {KAPA(dltav,nyqv)}**2
!     ==================================================================
!
!     Author :  Kang Nai
!     Date   :  Oct. 9, 2007
!     Action :  Created.
!
!     History:
!     --------
!
!     ------------------------------------------------------------------

      use variable_define
      implicit none
!
      real    :: vmax
      real    :: sigma
      real    :: crit

      integer :: i,ii,j,jj,k,kk,m,mm,n,nn,nsum,knum
      real    :: cos_elv,twonyq,thrths
      real    :: a1,a2,a3,a4,a5,b1,b2
      character(len=80) :: name1,name2

      real, dimension(1:np) :: veltmp1,veltmp2,veltmp3,veltmp4
      real    :: alfa0,alfa1,beta0,beta1
      real    :: tmp_angle1,tmp_angle2,dlta_v
      real    :: vr_sign

!     ##################################################################
!     Execute. calculate the cost function.
!     ##################################################################
      vr_sign=1.0
      if ( vmax*crit<0.0 ) then
           vr_sign=-1.0
      endif

      b1=0.0
      a5=0.0
      a4=0.0
      do k_tilt=start_tilt,end_tilt,step_tilt
        k=k_tilt
        if ( select_circ(k,ilevel)>0 ) then     ! available circle

             nbeam = nphi(k)
             nrang = imaxrng(k)
             elvng = thet(k)
             twonyq=2.0*thet_nyq_vel(k)
             thrths=0.5*twonyq
             cos_elv=cos(thet(k)*rdn)

             alfa0=check_small_phi(ilevel)
             alfa1=check_large_phi(ilevel)
             beta0=0.5*(alfa1+alfa0-180.0)
             tmp_angle2=(alfa0-beta0)*rdn

             ii = select_circ(k,ilevel)
             range = ran(ii)
             veltmp1=spval
             do j=1,nbeam
                veltmp1(j)=vel(ii,j,k)
             enddo

             do j=1,nbeam
                if ( abs(veltmp1(j))<spval ) then
                  tmp_angle1=(phi(j,k)-beta0)*rdn
                  a1= sin(tmp_angle1)-sin(tmp_angle2)
                  a2=vr_sign*vmax*a1*cos_elv
                  dlta_v=veltmp1(j)-a2
                  if ( dlta_v>=0.0 ) then
                       n=int(dlta_v/twonyq+0.5)
                  else
                       n=int(dlta_v/twonyq-0.5)
                  endif
                  b2=(dlta_v-float(n)*twonyq)
                  if ( abs(b2)<2.0*sigma ) then
                       b1=b1+b2*b2
                       a4=a4+2.0*b2*(-a1)*cos_elv
                       a5=a5+1.0
                  endif
                endif
             enddo
        endif
      enddo
      if ( a5<1.0 ) then
           cost_f(1)=b1
           cost_f(2)=-spval
           cost_f(3)=-spval
      else
           cost_f(1)=b1                     ! real J
           cost_f(2)=b1/a5                  ! average J
           cost_f(3)=a4/a5*10.0             ! gradient
      endif

      return
      end subroutine cost_function
