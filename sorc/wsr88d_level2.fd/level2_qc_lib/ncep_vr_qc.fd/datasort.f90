!     ##################################################################
!     ##################################################################
!     ######                                                      ######
!     ######              SUBROUTINE datasort                     ######
!     ######                                                      ######
!     ######                                                      ######
!     ##################################################################
!     ##################################################################
!
!     General Information
!
!#######################################################################
!
!      Purpose: sorting the observed value(reflectivity or radia velocity) 
!               into the order of ascending azimuth.
!
!#######################################################################

!#######################################################################
!
!     AUTHOR: Shun Liu
!     05/25/2002.
!
!     MODIFICATION HISTORY:
!     05/15/2002 (Shun Liu)
!
!#######################################################################
!
!     INPUT:
!     nazim :: the number of observed azimuth 
!     ngt   :: the number of gate in radia directory
!     azimdata :: azimth value
!     field_raw:: the obervation in one elevation
!
!     OUTPUT: 
!     azim_sort  :: azimuth after sorting
!     field_sort :: the result after sorting 
!#######################################################################

      Subroutine datasort(naz,ngt,na,np,azimdata,azim_sort,    &
                                field_raw,field_sort,indx)
      implicit none

      integer                 :: naz,ngt,na,np,i
      real,dimension(naz)     :: azimdata,azim_sort
      integer,dimension(naz)  :: indx
      real,dimension(ngt,naz) :: field_raw, field_sort
      
!*  Beginning of executalbe code...  
      
      call indexx(naz,na,azimdata,indx)

      do i=1,na
        azim_sort(i)=azimdata(indx(i))
        field_sort(:,i)=field_raw(:,indx(i)) 
      end do

      End
