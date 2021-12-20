
SUBROUTINE search_min( test_style,kmin,tmp_cost,tmp_v                &
                      ,vbest,sigma,crit )

! ======================================================================
!   PURPOSE:
!     Using the estimate abs(V) to calculate the residual variance. Then
!     using the residual variance to filter the data and re-calculate
!     residual variance.
! ======================================================================
!
!   Author   : Nai, Kang
!   Date     : Oct. 23, 2007
!   Action   : Created.
!
!   History  :
!  -----------
!
! ----------------------------------------------------------------------
  use variable_define

  implicit none

  integer  :: test_style,kmin
  real,dimension(10) :: tmp_cost,tmp_v
  real     :: vbest,sigma
  real     :: crit

  integer  :: i,j,k,kk,kp
  integer  :: ncount,mcount
  real     :: a1,a2,b1,b2,g1,g2,f1,f2,f3,c1
  real     :: dltav1,dltav2,dltav3
  character(len=40) :: name1,name2

! ######################################################################
!  Excuete
! ######################################################################
  if ( test_style==1 ) then

!      -----------------------------------------------------------------
!      Using 0Vr's gradient doing parabola regression to get the
!      minimun cost_function.
!      -----------------------------------------------------------------
       ncount=1
   10  continue
       dltav1=tmp_v(kmin+1)-tmp_v(kmin)
       g2=(tmp_cost(kmin+1)-tmp_cost(kmin))/dltav1**2 - g1/dltav1
       f1=tmp_v(kmin)-g1/(2.0*g2)             ! v0
       if ( f1>tmp_v(kmin) ) then
            tmp_v(kmin)=f1
            call cost_function( f1,sigma,crit )
            tmp_cost(kmin)=cost_f(2)
            g1=cost_f(3)
            if ( abs(f1-tmp_v(kmin))/tmp_v(kmin)<0.05 ) go to 20
       endif
       if ( f1<tmp_v(kmin) ) then
            tmp_v(kmin+1)=tmp_v(kmin)
            tmp_cost(kmin+1)=tmp_cost(kmin)
            tmp_v(kmin)=f1
            call cost_function( f1,sigma,crit )
            tmp_cost(kmin)=cost_f(2)
            g1=cost_f(3)
            if ( abs(f1-tmp_v(kmin))/tmp_v(kmin)<0.05 ) go to 20
       endif
       ncount=ncount+1
       go to 10
   20  continue
       vbest=tmp_v(kmin)

  elseif ( test_style==2 ) then

!      ---------------------------------------------------------------
!      Using three points doing  parabola regression to get the 
!      minimun cost_function.
!      ---------------------------------------------------------------
       ncount=1
   30  continue
       f3=tmp_v(kmin)
       dltav1=tmp_v(kmin-1)-tmp_v(kmin)
       dltav2=tmp_v(kmin+1)-tmp_v(kmin)
       dltav3=tmp_v(kmin+1)-tmp_v(kmin-1)
       a1=tmp_cost(kmin-1)-tmp_cost(kmin)
       a2=tmp_cost(kmin+1)-tmp_cost(kmin)
       b1=a1*dltav2**2-a2*dltav1**2
       b2=dltav1*dltav2*dltav3
       if ( b2==0.0 ) go to 40               ! points overlap

       g1=b1/b2
       if ( g1==0.0 ) g1=g1+0.5

       g2=(a1-g1*dltav1)/dltav1**2
       if ( abs(g2)<0.01 ) then              ! line function
         f1=tmp_v(kmin)-a1/g1
         c1=1.0
       else
         c1=g1/(2.0*g2)
         f1=tmp_v(kmin)-c1                         ! new v0
       endif

       if ( abs(c1)<0.001 ) go to 40

       call cost_function( f1,sigma,crit )

       if ( cost_f(2)<tmp_cost(kmin) ) then
         if ( f1>f3 ) then
           f2=tmp_cost(kmin)
           tmp_v(kmin-1)=tmp_v(kmin)
           tmp_cost(kmin-1)=tmp_cost(kmin)
           tmp_v(kmin)=f1
           tmp_cost(kmin)=cost_f(2)
           if ( abs(f2-tmp_cost(kmin))/f2<0.01 ) go to 40 ! abs(V)
           if ( abs(f1-tmp_v(kmin-1))/tmp_v(kmin-1)<0.01 ) go to 40 ! abs(V)
         endif
         if ( f1<=f3 ) then
           f2=tmp_cost(kmin)
           tmp_v(kmin+1)=tmp_v(kmin)
           tmp_cost(kmin+1)=tmp_cost(kmin)
           tmp_v(kmin)=f1
           tmp_cost(kmin)=cost_f(2)
           if ( abs(f2-tmp_cost(kmin))/f2<0.01 ) go to 40  ! abs(V)
           if ( abs(f1-tmp_v(kmin+1))/tmp_v(kmin-1)<0.01 ) go to 40 ! abs(V)
         endif
       else
         go to 40
       endif
       ncount=ncount+1
       if ( ncount>200 ) then
            print*,' '
            print*,'unlimited iter'
            print*,ncount,tmp_v(kmin-1),tmp_v(kmin),tmp_v(kmin+1)
            print*,tmp_cost(kmin-1),tmp_cost(kmin),tmp_cost(kmin+1),g1,g2
            print*,c1,f1,ncount
            print*,' '
            go to 40
       endif
       go to 30
   40  continue
       vbest=tmp_v(kmin)
  endif

  return

END SUBROUTINE search_min
