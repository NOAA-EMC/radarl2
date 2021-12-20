subroutine NoiseFltr(na,ng,nelev,        &
                    strct_in_ref,strct_in_vel    &
                    )
!------------------------------------------------------------------------
!  subroutine to remove isolated noise echoes.
!
!  History:
!  Date       Name          Action
!  ---------  ------------  --------------------------------------------
!  Aug. 13, 06  Pengfei Zhang          Created.
!
!-----------------------------------------------------------------------

     use sdata

     implicit none
      integer :: na,ng,nelev
      integer :: i,j,k 
      integer :: ii,jj,nbad_Z,nt_Z,nbad_V,nt_V 
      integer :: jsign,jt,jbad 
      integer :: isc,istart,iend 
      integer :: num_beamref,num_gateref 
      integer :: num_beamvel,num_gatevel 
      real    :: spval
      real    :: pbad_Z,pbad_V
      real    :: sgn,rsign,thrsign,thrv 
      real    :: rjbad,thrjbad 
      type(ccout_strct) :: strct_in_ref,strct_in_vel

       spval=999.0 
       num_beamref=strct_in_ref%num_beam
       num_gateref=strct_in_ref%num_gate

       num_beamvel=strct_in_vel%num_beam
       num_gatevel=strct_in_vel%num_gate

!+++++++++++++++Filter out noisy data in reflectivity field+++++++++++++++

      do j=1,num_beamref
      do i=1,num_gateref

      if ((strct_in_ref%field(i,j)).ne.spval) then
        nbad_Z=0
        nt_Z=0
        do jj=j-2,j+2
        do ii=i-2,i+2
         if(abs(strct_in_ref%field(ii,jj)).gt.900) then   
          nbad_Z=nbad_Z+1
         end if 
          nt_Z=nt_Z+1
        enddo
        enddo

         pbad_Z=float(nbad_Z)/float(nt_Z)

         if (pbad_Z > 0.67 ) then
!print*,'pbad_Z=',pbad_Z,' azm=',strct_in_ref%azim(j), ' i=',i
          strct_in_ref%field(i,j)=spval
         end if

      end if ! end of '(strct_in_ref%field(i,j)).ne.spval'

      enddo
      enddo

!+++++++++++++++Filter out noisy data in Doppler velocity field+++++++++++++++
  
      do j=1,num_beamvel
      do i=1,num_gatevel
       if ((strct_in_vel%field(i,j)).ne.spval) then
        nbad_V=0
        nt_V=0
        do jj=j-2,j+2
        do ii=i-2,i+2
         if(abs(strct_in_vel%field(ii,jj)).gt.900) then
          nbad_V=nbad_V+1
         end if
          nt_V=nt_V+1
        enddo
        enddo

         pbad_V=float(nbad_V)/float(nt_V)

         if (pbad_V > 0.67 ) then
          strct_in_vel%field(i,j)=spval
         end if

       end if ! end of '(strct_in_vel%field(i,j)).ne.spval'

      enddo
      enddo

       thrsign=0.1
       thrv=2.0

      do isc=1,2
       if (isc .eq. 1) then
        istart=1
        iend=200
       else 
        istart=201
        iend=num_gatevel-1
       endif
       jbad=0
!      do j=100,100
      do j=1,num_beamvel
        jsign=0
        jt=0
      do i=istart,iend
!      do i=1,num_gatevel-1

       if ((strct_in_vel%field(i,j)).ne.spval &
        .and. (strct_in_vel%field(i+1,j)).ne.spval) then
         jt=jt+1
         if ((strct_in_vel%field(i,j+1)).ne. 0.0 ) then
           sgn=strct_in_vel%field(i,j)/strct_in_vel%field(i+1,j)
           if ( sgn .lt. 0.0 &
               .and. abs(strct_in_vel%field(i,j)) .gt. thrv) then
             jsign=jsign+1
           endif
         endif
       endif

      enddo
       if (jt .ne. 0) then
        rsign=float(jsign)/float(jt)
       endif
!   print*,'rsign=',rsign
        if (rsign .gt. thrsign) then
          jbad=jbad+1
         strct_in_vel%field(istart:iend,j) = spval
        endif
      enddo
     enddo ! isc loop

         thrjbad = 0.5 
         rjbad=float(jbad)/float(num_beamvel)
!  print*,'rjbad=',rjbad
        if ( rjbad .gt. thrjbad ) then
         strct_in_vel%field(:,:) = spval
        endif

end subroutine NoiseFltr
