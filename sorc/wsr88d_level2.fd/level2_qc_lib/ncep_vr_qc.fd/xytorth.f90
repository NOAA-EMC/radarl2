!     ##################################################################
!     ##################################################################
!     ######                                                      ######
!     ######              SUBROUTINE xytorth                      ######
!     ######                                                      ######
!     ######                                                      ######
!     ##################################################################
!     ##################################################################
!
!     General Information
!
!#######################################################################
!
!      Purpose: transform Cartesian coordinate to polar coordinate
!
!#######################################################################

!#######################################################################
!
!     AUTHOR: Shun Liu
!     05/13/2002.
!
!     MODIFICATION HISTORY:
!     05/13/2002 (Shun Liu)
!     05/15/2002 (Shun Liu)
!     output directory cosin th and rd.
!
!#######################################################################
!
!     INPUT:
!     x value in Cartesian coordinate
!     y value in Cartesian coordinate
!
!
!
!######################################################################
      Subroutine xytorth(x,y,th,rd)

      implicit none
!######################################################################
!
!      Variable Declarations.
!
!######################################################################
       
       real,parameter::pi=3.1415926
       real:: x, y, th, rd

!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
!     Beginning of executable code...
!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

       if(x==0.0 .and. y==0.0)then

         rd=0.0
         th=0.0

       else 

         rd = sqrt(x**2+y**2)
       
         th = atan2(x,y)
         if(th<0.0)then
           th=2*pi+th
         end if

       end if

       th = th*180.0/pi

      End subroutine xytorth
