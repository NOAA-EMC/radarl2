
SUBROUTINE PROFILE_DIFFER_QC

!     ==================================================================
!     PURPOSE:
!        vertical profile differ's check.
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
!     excuete
!     ------------------------------------------------------------------
      do ilevel=1,zlevel
        if ( abs(ustor(ilevel))<spval ) then
          ii=ilevel; exit   
        endif
      enddo

      if ( ilevel>=zlevel ) RETURN

      c1=sqrt(ustor(ii)*ustor(ii)+vstor(ii)*vstor(ii))
      do k=ii+1,zlevel
         if ( abs(ustor(k))<spval ) then
              c2=sqrt(ustor(k)*ustor(k)+vstor(k)*vstor(k))
              if ( abs(c2-c1)<6.0 ) then
                   c1=c2
              else
                   ken=k
                   exit
              endif
         else
              ken=k
         endif
      enddo
!     ------------------------------------------------------------------
!     filling in spval above the ken level
!     ------------------------------------------------------------------
      do i=ken,zlevel
         ustor(i)=spval
         vstor(i)=spval
         cf1stor(i) = spval
         cf2stor(i) = spval
         cf3stor(i) = spval
      enddo
      RETURN

END SUBROUTINE PROFILE_DIFFER_QC
