!#######################################################################
!#######################################################################
!####                                                               ####
!####                Subroutine get_residual                        ####
!####                                                               ####
!#######################################################################
!#######################################################################

      subroutine get_residual( vmax,crit,sigma                       &
                              ,mcount )
 
!     ==================================================================
!     PURPOSE:
!       Using the estimate V to get the rasidual variance.
!     ==================================================================
!
!     Author :  Kang Nai
!     Date   :  Oct. 24, 2007
!     Action :  Created.
!
!     History:
!     --------
!
!     ------------------------------------------------------------------

      use variable_define
      implicit none
!
      real    :: vmax,crit,sigma
      integer :: mcount

      integer :: i,ii,j,jj,k,kk,m,mm,n,nn,nsum,knum
      real    :: cos_elv,twonyq,thrths
      real    :: a1,a2,a3,a4,a5,b1,b2

      real, dimension(1:np) :: veltmp1
      real    :: alfa0,alfa1,beta0,beta1
      real    :: tmp_angle1,tmp_angle2,dlta_v
      real    :: vr_sign
      character(len=40) :: name1,name2

!     ##################################################################
!     Execute.
!     ##################################################################
      vr_sign=1.0
      if ( vmax*crit<0.0 ) then
           vr_sign=-1.0
      endif

!     ------------------------------------------------------------------
!     get the residual variance.
!     ------------------------------------------------------------------
      b1=0.0
      a5=0.0
      do k_tilt=start_tilt,end_tilt,step_tilt
        k=k_tilt
        if ( select_circ(k,ilevel)>0 ) then    ! available circle

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
                  a2= vr_sign*vmax*a1*cos_elv
                  dlta_v=veltmp1(j)-a2
                  if ( dlta_v>=0.0 ) then
                       n=int(dlta_v/twonyq+0.5)
                  else
                       n=int(dlta_v/twonyq-0.5)
                  endif
                  b2=(dlta_v-float(n)*twonyq)
                  b1=b1+b2*b2
                  a5=a5+1.0
                endif
             enddo
        endif
      enddo
      if ( a5>5.0 ) then
           sigma=sqrt(b1/(a5-2.0))
      else
           print*,'no enough data, will be RETURN'
           RETURN
      endif

!     ------------------------------------------------------------------
!     write out the results.
!     ------------------------------------------------------------------
      write(name1,'(i4.4,i2.2)') ilevel,mcount
      name2='rsdl'//name1(1:6)//'.dat'
      open(31,file=name2)
      do k_tilt=start_tilt,end_tilt,step_tilt
        k=k_tilt
        if ( select_circ(k,ilevel)>0 ) then    ! available circle

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
                  a2= vr_sign*vmax*a1*cos_elv
                  dlta_v=veltmp1(j)-a2
                  if ( dlta_v>=0.0 ) then
                       n=int(dlta_v/twonyq+0.5)
                  else
                       n=int(dlta_v/twonyq-0.5)
                  endif
                  b2=(dlta_v-float(n)*twonyq)
                  b1=b2+a2
                  a3= a2-2.0*sigma
                  a4= a2+2.0*sigma
                  a5=phi(j,k)
                  write(31,'(f10.0,5e15.5)') a5,veltmp1(j),b1,a2,a3,a4
                endif
             enddo
        endif
      enddo
      close(31)

      return
      end subroutine get_residual
