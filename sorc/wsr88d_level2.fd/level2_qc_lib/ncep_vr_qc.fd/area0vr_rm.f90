      subroutine area0vr_rm ( nr,np,vel,nbeam,iminrng,nrang,spval    &
                             ,tmp1,tmp2 )
!     ==================================================================
!     PURPOSE:
!       remove the bulk 0Vr.
!     ==================================================================
!     Author : Kang Nai
!     date   : 02/08/2008
!     Action : Created
!
!     Modify History:
!
!     ------------------------------------------------------------------
      implicit none

      integer nr,np
      real spval
      integer nbeam,iminrng,nrang
      integer i,j,ip,jp,ims,inr,inp
      integer iii,jjj,i0,j0,npts
      integer k,kk

      real,    dimension(nr,np)   :: vel
      real,    dimension(nr,np)   :: tmp1
      real,    dimension(nr,np)   :: tmp2
      integer, dimension(nr,np)   :: index0
      real                        :: a1,a2,a3
 
!     ------------------------------------------------------------------
!     first, index all Vr<1.0 points.
!     ------------------------------------------------------------------
      tmp1=spval
      do j=1,nbeam
      do i=iminrng,nrang
        if ( abs(vel(i,j))<spval ) then
             tmp1(i,j)= 10.0
        endif
        if ( abs(vel(i,j))<=1.5 ) then
             tmp1(i,j)=-10.0
        endif
      enddo
      enddo

      index0=999
      do j=1,nbeam
      do i=iminrng,nrang
        if ( abs(vel(i,j))<spval ) then
          index0(i,j)=0
        endif
      enddo
      enddo

!     ----------------------------------------------------------------
!     second, count the number which is tmp2<0.0 in a 21x5 window. 
!     ----------------------------------------------------------------
      tmp2=tmp1

      do j=1,nbeam
      do i=iminrng,nrang
        if ( tmp2(i,j)<0.0 ) then
          a1=0.0
          a2=0.0
          a3=0.0
          do jp=-10,10
            j0=j+jp
            if ( j0<=0 ) j0=nbeam+j0
            if ( j0>nbeam ) j0=j0-nbeam
            do ip=-2,2
              i0=i+ip
              i0=max(i0,1)
              i0=min(i0,nrang)
              if ( tmp2(i0,j0)<0.0 ) then
                a1=a1+1.0
                a3=a3+1.0
              endif
              if ( tmp2(i0,j0)>0.0 .and. tmp2(i0,j0)<spval ) then
                a2=a2+1.0
                a3=a3+1.0
              endif
            enddo
          enddo

!         ------------------------------------------------------------
!         mark the index when it is a bulk 0Vr area.
!         ------------------------------------------------------------
          if ( (a1/a3)>=0.75 ) then
            do jp=-10,10
              j0=j+jp
              if ( j0<=0 ) j0=nbeam+j0
              if ( j0>nbeam ) j0=j0-nbeam
              do ip=-2,2
                i0=i+ip
                i0=max(i0,1)
                i0=min(i0,nrang)
                if ( tmp2(i0,j0)<0.0 ) then
                  index0(i0,j0)=888
                endif
              enddo
            enddo
          endif
        endif
      enddo
      enddo

!     ------------------------------------------------------------------
!     filling in spval when it is a bulk 0Vr area.
!     ------------------------------------------------------------------
      tmp2=tmp1
      do j=1,nbeam
      do i=iminrng,nrang
        if ( index0(i,j)==888 ) then
          vel(i,j)=spval
          tmp2(i,j)=-52.0
        endif
      enddo
      enddo

      return
      end subroutine area0vr_rm
