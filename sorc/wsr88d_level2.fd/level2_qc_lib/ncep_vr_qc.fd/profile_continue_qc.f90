
SUBROUTINE PROFILE_CONTINUE_QC

!     ==================================================================
!     PURPOSE:
!        vertical connected profile's continuity check.
!     ==================================================================
!     Author : Kang Nai
!     date   : Jan 9, 2008
!     Action : Created.
!
!     Modify History:
!
!     ==================================================================

      use variable_define

      implicit none

      integer :: i,ii,j,jj,k,kk,idif,nsum,kst,ken,jp,ibgn
      real    :: c1,c2

!     ------------------------------------------------------------------
!     choising the start level of the profile.
!     ------------------------------------------------------------------
      ken=0
      ibgn=1
   10 continue
      do ilevel=ibgn,zlevel
        if ( abs(ustor(ilevel))<spval ) then
          ii=ilevel; exit   
        endif
      enddo

      if ( ilevel>=zlevel ) RETURN

!     ------------------------------------------------------------------
!     get the end level of the profile continue with start level.
!     ------------------------------------------------------------------
      kk=ii
      do jj=ii+1,zlevel
        if ( abs(ustor(jj))>900.0 ) then
             kk=jj-1
             exit
        endif
      enddo

      if ( kk==ii ) then
           ibgn=kk+1
           go to 10
      endif

!     ------------------------------------------------------------------
!     vertical continue check.
!     ------------------------------------------------------------------
      ken=kk
      do i=ii,kk-1
         c1=sqrt(ustor(i)*ustor(i)+vstor(i)*vstor(i))
         c2=sqrt(ustor(i+1)*ustor(i+1)+vstor(i+1)*vstor(i+1))
         if ( abs(c1-c2)>6.0 ) then
              ken=i
              exit
         endif
      enddo
      if ( ken==kk ) then
        ibgn=kk+1
        go to 10
      endif

!     ------------------------------------------------------------------
!     filling in spval above the ken level
!     ------------------------------------------------------------------
      do i=ken+1,zlevel
         ustor(i)=spval
         vstor(i)=spval
         cf1stor(i) = spval
         cf2stor(i) = spval
         cf3stor(i) = spval
      enddo
      RETURN

END SUBROUTINE PROFILE_CONTINUE_QC
