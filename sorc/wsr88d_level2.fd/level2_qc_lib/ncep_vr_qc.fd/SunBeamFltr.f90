subroutine SunBeamFltr(na,ng,nelev,        &
                    strct_in_ref,strct_in_vel    &
                    )
!------------------------------------------------------------------------
!  subroutine to remove sun beam and interference echoes.
!
!  History:
!  Date       Name          Action
!  ---------  ------------  --------------------------------------------
!  Sep. 11, 06  Pengfei Zhang          Created.
!
!-----------------------------------------------------------------------

     use sdata

     implicit none
      integer :: na,ng,nelev
      integer :: i,j,k 
      integer :: num_beamref,num_gateref 
      integer :: num_beamvel,num_gatevel 
      integer :: nZt(na)
      real    :: spval
      real    :: rDiffL1,rDiffR1,rDiffL2,rDiffR2,rZtt
      type(ccout_strct) :: strct_in_ref,strct_in_vel

       spval=999.0 
       num_beamref=strct_in_ref%num_beam
       num_gateref=strct_in_ref%num_gate

       num_beamvel=strct_in_vel%num_beam
       num_gatevel=strct_in_vel%num_gate

!+++++++++++++++Filter out Sun Beam in reflectivity field+++++++++++++++

      do j=1,num_beamref
       nZt(j)=1
       do i=1,num_gateref
        if ( strct_in_ref%field(i,j) .ne. spval) then
         nZt(j) = nZt(j) + 1
        endif
       enddo
      enddo

      rZtt=1.4
     do j=3,num_beamref-2
       rDiffL1=float(nZt(j))/float(nZt(j-1))
       rDiffL2=float(nZt(j))/float(nZt(j-2))
       rDiffR1=float(nZt(j))/float(nZt(j+1))
       rDiffR2=float(nZt(j))/float(nZt(j+2))

       if ( nZt(j) > 150 ) then
        if ( rDiffL1 > rZtt .or. rDiffR1 > rZtt &
          .or. rDiffL2 > rZtt .or. rDiffR2 > rZtt) then

!  print*,' BIG  ZL=',rDiffL1,' ZR=',rDiffR1
!  print*,' BIG  ZL=',rDiffL2,' ZR=',rDiffR2
!  print*,' azm=',strct_in_ref%azim(j)

         strct_in_ref%field(:,j) = spval
!        do i=1,num_gateref
!         strct_in_ref%field(i,j) = spval
!        enddo
        endif
       endif

     enddo

!+++++++++++++++Filter out Sun Beam in Doppler velocity field+++++++++++++++
  

end subroutine SunBeamFltr
