!#######################################################################
!#######################################################################
!####                                                               ####
!####                Subroutine line_regression                     ####
!####                                                               ####
!#######################################################################
!#######################################################################

      subroutine line_regression( nn,velarray,phiarray                &
                                 ,phi_work,spval                      &
                                 ,vnyq_min                            &
                                 ,alfa,beta,rr )
 
!     ==================================================================
!     PURPOSE:
!       line_regression analysis.( Vr=alfa+beta*phi )
!     ==================================================================
!
!     Author :  Kang Nai
!     Date   :  Oct. 16, 2007
!     Action :  Created.
!
!     History: 
!     --------
!     Oct. 19, 2007    Kang Nai added the believable monitor.
!     ------------------------------------------------------------------

      implicit none
!
      integer :: nn
      real,dimension(nn) :: velarray
      real,dimension(nn) :: phiarray
      real               :: phi_work
      real    :: spval,vnyq_min,alfa,beta,rr

      integer :: i,j,k
      real    :: a1,a2,a3,a4,a5,b1,b2
      real    :: residual

!     ##################################################################
!     Execute. Give some parameters.
!     ##################################################################
      a1=0.0
      a2=0.0
      b1=0.0
      do i =1,nn
         if ( abs(velarray(i))<spval ) then
              b1=b1+1.0
              a1=a1+velarray(i)
              a2=a2+phiarray(i)
         endif
      enddo
      a1=a1/b1
      a2=a2/b1
      a3=0.0
      a4=0.0
      a5=0.0
      b2=0.0
      do i=1,nn
         if ( abs(velarray(i))<spval ) then
              b2=b2+1.0
              a3=a3+(velarray(i)-a1)*(velarray(i)-a1)
              a4=a4+(phiarray(i)-a2)*(phiarray(i)-a2)
              a5=a5+(velarray(i)-a1)*(phiarray(i)-a2)
         endif
      enddo
      
      beta=a5/a4
      alfa=a1-beta*a2
      rr=a5/sqrt(a4*a3)
      residual=sqrt((a3-beta*a5)/(b2-2.0))

!     ==================================================================
!     put a criterion of the residual.
!     ==================================================================
      a1=2.0*residual
      a2=vnyq_min/2.0
      residual=min(a1,a2)

!     ==================================================================
!     be relievable monitor(0.95).
!     ==================================================================
      do i=1,nn
         if ( abs(velarray(i))<spval ) then
              b1=alfa+beta*phiarray(i)                ! regression line
              a1=b1-residual
              a2=b1+residual
              if ( velarray(i)<a1 .or. velarray(i)>a2 ) then
                   velarray(i)=spval
              endif
         endif
      enddo

!     ==================================================================
!     re-do the line regression.
!     ==================================================================
      a1=0.0
      a2=0.0
      b1=0.0
      do i =1,nn
         if ( abs(velarray(i))<spval ) then
              b1=b1+1.0
              a1=a1+velarray(i)
              a2=a2+phiarray(i)
         endif
      enddo
      a1=a1/b1
      a2=a2/b1
      a3=0.0
      a4=0.0
      a5=0.0
      b2=0.0
      do i=1,nn
         if ( abs(velarray(i))<spval ) then
              b2=b2+1.0
              a3=a3+(velarray(i)-a1)*(velarray(i)-a1)
              a4=a4+(phiarray(i)-a2)*(phiarray(i)-a2)
              a5=a5+(velarray(i)-a1)*(phiarray(i)-a2)
         endif
      enddo

      beta=a5/a4
      alfa=a1-beta*a2
      rr=a5/sqrt(a4*a3)
      residual=sqrt((a3-beta*a5)/(b2-2.0))

      phi_work=-alfa/beta
      if ( phi_work<0.0 ) phi_work=spval
      if ( phi_work>360.0 ) phi_work=spval

      return
      end subroutine line_regression
