c**************************************************************
      subroutine num_sign(fld,nr,np,
     &           iminrg,nrang,spval,numSC,prct_total,prct2)
c___________________________________________________
c
c    Purpose: count the number of sign change along a beam 
c
c    Author: Pengfei Zhang
c    Date:   Oct. 29, 2002

c
c    revised: Shun Liu
c    Date:   Nov. 01, 2002
c___________________________________________________

c___________________________________________________
c
c    Input: fld,nr,np,beam,iminrg, nrang,spval
c    output: numSC : number of Sign Change 
c
c___________________________________________________

       implicit none
       integer :: nr,np
       real fld(nr,np)
       real fld_bd(nr,np)
       real beamv(nr)
       integer iminrg,nrang
       integer i,j,iv,inum
       real spval,prct,prct_total,prct2
       integer numSC
       real bd_sum
       real tbd_sum
       real a, b

c-------------------------------------------------------
        numSC = 0

        prct_total=0.0
        inum=0
        fld_bd=0.0 

        do j=1,np

        iv=0
        do i=iminrg,nrang
           if (fld(i,j) .ne. spval) then
              iv=iv+1
              beamv(iv)=fld(i,j)
           end if
        end do

        if(iv .ne. 0) then

        numSC=0
        do i=2,iv
         if ( beamv(i-1) .ne. 0.0 ) then
          a = sign(beamv(i-1),beamv(i))
          b = a/beamv(i-1)
!         if ( b .eq. -1.0) then
          if ( b < -0.01) then
           fld_bd(i,j)=1.0
           numSC = numSC + 1
          end if
         end if
        end do

        inum=inum+1
        prct=numsc/real(iv)
        prct_total=prct_total+prct
        end if

        end do

        bd_sum=sum(fld_bd,mask=abs(fld)>2.0.and.abs(fld)<5.0) 
        fld_bd=1.0
        tbd_sum=sum(fld_bd,mask=abs(fld)>2.0.and.abs(fld)<5.0) 
        prct2=real(bd_sum)/real(tbd_sum)
!       print*,bd_sum
!       print*,tbd_sum
        if(inum>0)then
        prct_total=prct_total/inum
        else
        prct_total=99.0
        end if

        return
        end
c******End of subroutine  num_sign************************************
