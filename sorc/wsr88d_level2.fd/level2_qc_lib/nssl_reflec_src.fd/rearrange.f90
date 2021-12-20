!#######################################################################
!     ####                                                          ####
!     ####                SUBROUTINE rearrange.f                    ####
!     ####                                                          ####
!     ####             Developed by Cooperative Institute           ####
!     ####            for Mesoscale Meteorological Studies          ####
!     ####                  University of Oklahoma                  ####
!     ####                                                          ####
!     ##################################################################
!
!-----------------------------------------------------------------------
!
!     PURPOSE:
!         This file is a fortran file designed to rearrange the radar
!         data along the clockwise from the north direction.
!
!-----------------------------------------------------------------------
!
!     AUTHOR: Kang Nai
!     12/22/2005
!
!     MODIFICATION HISTORY:
!
!-----------------------------------------------------------------------
      subroutine rearrange( nr,np,npmax                             &
                           ,nbeam,nrange,azim,field                 &
                           ,frstvel,velgatsp                        &
                           ,fieldout,dltphi                         &
                           ,spval)
!
      implicit none
!
      integer :: nr
      integer :: np
      integer :: npmax
      integer :: nrange
      integer :: nbeam
      real, dimension(np) :: azim
      real, dimension(nr,np) :: field
      real    :: frstvel,velgatsp
      real, dimension(nr,npmax) :: fieldout
      real    :: dltphi
      real    :: spval
      real, dimension(nr,npmax) :: cont
!
      integer :: i,j,k,ii,jj
      real :: a1
!     @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!     excute
!     @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
      fieldout = 0.0
      cont = 0.0
      do j =1, nbeam
         if ( azim(j) .ge. 0.0 .and. azim(j) .lt. 360.0 ) then
              jj = int(azim(j)/dltphi) + 1
              do i =1, nrange
                 if ( abs(field(i,j)) .lt. 200.0 ) then
                      a1 = frstvel + float(i-1) * velgatsp
                      ii = int(a1/velgatsp) + 1
                      if ( ii .ge. 1 ) then
                           fieldout(ii,jj)=field(i,j)
                           cont(ii,jj)=cont(ii,jj) + 1.0
                      endif
                 endif
              enddo
         endif
      enddo
!
!     do j =1,npmax
!     do i =1,nr
!        if ( cont(i,j) .ge. 1.0 ) then
!             fieldout(i,j) =fieldout(i,j)/cont(i,j)
!        else
!             fieldout(i,j) =spval
!        endif
!     enddo
!     enddo
!
      return
      end subroutine rearrange
