subroutine RemvPattern(na,ng,nelev,        &
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
      integer :: nt,ii,nf,nlnth, ist
      integer :: nava,np,njbad
      real    :: spval
      real    :: Zt
      real    :: ra,rp
      real    :: thra,thrp
      real    :: avgZ(ng)
      type(ccout_strct) :: strct_in_ref,strct_in_vel

       spval=999.0 
       num_beamref=strct_in_ref%num_beam
       num_gateref=strct_in_ref%num_gate

       num_beamvel=strct_in_vel%num_beam
       num_gatevel=strct_in_vel%num_gate

!+++++++++++++++Filter out test pattern in reflectivity field+++++++++++++++

!       j=100
!       do i=1,num_gateref
!print*,'ref=',strct_in_ref%field(i,j),' i=',i
!       enddo

       nlnth=20

        njbad=0
      do j=1,num_beamref
       nf=num_gateref/nlnth
!print*,' nf=',nf
        nava=0
       do ii = 1,nf
        Zt=0.0
        nt=0
         ist=(ii-1)*nlnth+1
        do i=ist,ist+nlnth-1
         if ( strct_in_ref%field(i,j) .ne. spval) then
          Zt=Zt+strct_in_ref%field(i,j)
          nt=nt+1
         endif
        enddo   !i loop
         if ( nt .ne. 0 ) then
          avgZ(ii) = Zt/float(nt)
          nava=nava+1
         else
          avgZ(ii) = spval
         endif
! print*,'avgZ(ii)=',avgZ(ii),' ii=',ii,' nt=',nt
       enddo    !ii loop

         np=0
       do ii = 1,nf-1
         if ( avgZ(ii+1)-avgZ(ii) .gt. 0.0) then
          np=np+1
         endif   
       enddo    !ii loop
! print*,'nava=',nava,' np=',np

! calculate available sectors rate
         ra=float(nava)/float(nf)
! calculate average value within a sector that increases with range 
         rp=float(np)/float(nf)
!print*,'ra=',ra,' rp=',rp

        thra = 0.8
        thrp = 0.6
       if ( ra .gt. thra .and. rp .gt. thrp) then
        strct_in_ref%field(:,j)=spval
        njbad=njbad+1
       endif
      enddo     !j loop
  print*,'njbad=',njbad
!++++++++++++++++++++++++++++++
!   if ( njbad .gt. num_beamref/2) then
!     strct_in_ref%field(:,:)=spval
!     strct_in_vel%field(:,:)=spval
!   endif
end subroutine RemvPattern
