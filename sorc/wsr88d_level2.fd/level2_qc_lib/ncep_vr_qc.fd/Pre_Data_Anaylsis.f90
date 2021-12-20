!     ##################################################################
!     ##################################################################
!     ######                                                      ######
!     ######        Radar Data Quality Control System.            ######
!     ######                                                      ######
!     ######              Copyright (c) 2009                      ######
!     ######             Cooperative Institute                    ######
!     ######       for Mesoscale Meteorological Studies           ######
!     ######    University of Oklahoma.  All rights reserved.     ######
!     ######                                                      ######
!     ##################################################################
!     ##################################################################
!=======================================================================
!     Purepose:
!       Get the median points.
!=======================================================================
!     Author : Yuan Jiang
!     Date   : OCT 23, 2011
!     Action : Created.
!***********************************************************************
      subroutine median( NPT,fld,nr,np,iminrg,nrang,missing,med_fld )

      implicit none
   
      integer nr,np,nt   
      integer npt         
      integer iminrg,nrang     
      integer i,j
      real    fld(nr,np)
      real    med_fld(nr,np)   
      real    a(npt),aa,missing
      integer l_half
      integer m,ii,jj,ic

      l_half=int((npt-1)/2)
      med_fld=missing
 
      do j=1, np
        do i=iminrg+l_half,nrang-l_half
          ic=0
          do m=i-l_half,i+l_half
            ic=ic+1
            a(ic)=fld(m,j)     ! a is a temperory array
          end do
          do jj = 2,npt
            aa = a(jj)
            do ii = jj -1, 1, -1
              if ( a(ii) .le. aa ) go to 11
              a(ii+1) = a(ii)
            end do  ! ii loop
            ii = 0
11          a(ii+1) = aa
          end do ! jj loop
          med_fld(i,j) = a((npt+1)/2)     
        end do  ! i loop
      end do   ! j loop

      return
      end subroutine median

!     ##################################################################
!     ##################################################################
!     ######                                                      ######
!     ######        Radar Data Quality Control System.            ######
!     ######                                                      ######
!     ######              Copyright (c) 2009                      ######
!     ######             Cooperative Institute                    ######
!     ######       for Mesoscale Meteorological Studies           ######
!     ######    University of Oklahoma.  All rights reserved.     ######
!     ######                                                      ######
!     ##################################################################
!     ##################################################################
!=======================================================================
!     Purepose:
!       get the median phi along a radar beam as phi 0-360.
!========================================================================
!
!     Author   : Jiang, Yuan
!     Date     : Oct. 23, 2011
!     Action   : Created.
!========================================================================
      subroutine median_phi(NPT,fld,nr,np,iminrg,nrang,missing,med_fld)

      implicit none

      integer nr,np,nt
      integer npt
      integer iminrg,nrang
      integer i,j
      real    fld(nr,np)
      real    med_fld(nr,np)
      real    a(npt),aa,missing
      integer l_half
      integer m,ii,jj,ic

      l_half=int((npt-1)/2)
      med_fld=missing

      do j=1, np
        do i=iminrg+l_half,nrang-l_half
          ic=0
          do m=i-l_half,i+l_half
            ic=ic+1
            a(ic)=fld(m,j)     ! a is a temperory array
          end do
          do jj = 2,npt
            aa = a(jj)
            do ii = jj -1, 1, -1
              if ( a(ii) .le. aa ) go to 11
              a(ii+1) = a(ii)
            end do  ! ii loop
            ii = 0
11          a(ii+1) = aa
          end do ! jj loop
          med_fld(i,j) = a((npt+1)/2)
        end do  ! i loop
      end do   ! j loop

      return
      end subroutine median_phi

!     ##################################################################
!     ##################################################################
!     ######                                                      ######
!     ######        Radar Data Quality Control System.            ######
!     ######                                                      ######
!     ######              Copyright (c) 2009                      ######
!     ######             Cooperative Institute                    ######
!     ######       for Mesoscale Meteorological Studies           ######
!     ######    University of Oklahoma.  All rights reserved.     ######
!     ######                                                      ######
!     ##################################################################
!     ##################################################################
!=======================================================================
!     Purepose:
!       calculate the TDBZ along radial.
!=======================================================================
!
!     Author   : Jiang, Yuan
!     Date     : Oct. 23, 2011
!     Action   : Created.
!
!=======================================================================
      subroutine SDpara(fld,nr,np,iminrg,nrang,mgate,missing,tdbz)

      implicit none

      integer  nr,np,mgate, mgate_hf
      real     fld(nr,np),tdbz(nr,np),ttdbz,diff,avg,num,missing
      integer  i,j, m, ma,mr,iminrg, nrang

      mgate_hf = (mgate-1)/2
      if ( iminrg-mgate_hf<1 .or. iminrg+mgate_hf>nr ) then
         write(*,*) "error iminrg or nrang in SDpara"
         return
      end if

      do j = 1, np
        do i = iminrg, nrang
          tdbz(i,j)=missing
          if ( fld(i,j)<missing ) then
            num=0
            ttdbz=0.0
            avg=0.0
            do mr = i-mgate_hf, i +mgate_hf
              if ( fld(mr,j) < missing ) then
                num=num+1
                avg=avg+fld(mr,j)
              end if
            end do
            if ( num<2 ) goto 11
            avg=avg/num
            do mr = i-mgate_hf, i+mgate_hf
              if ( fld(mr,j)<missing ) then
                diff = fld(mr,j)-avg
                ttdbz = ttdbz+ (diff*diff)
              end if
            end do
            if ( num >=1 ) tdbz(i,j)=ttdbz/(num*num)
          end if
  11      continue
        end do
      end do

      return
      end subroutine SDpara

!     ##################################################################
!     ##################################################################
!     ######                                                      ######
!     ######        Radar Data Quality Control System.            ######
!     ######                                                      ######
!     ######              Copyright (c) 2009                      ######
!     ######             Cooperative Institute                    ######
!     ######       for Mesoscale Meteorological Studies           ######
!     ######    University of Oklahoma.  All rights reserved.     ######
!     ######                                                      ######
!     ##################################################################
!     ##################################################################
!=======================================================================
!     Purepose:
!       Calculating membership
!=======================================================================
!
!     Author   : Jiang, Yuan
!     Date     : Nov. 1, 2011
!     Action   : Created.
!
!=======================================================================
      subroutine mfun(para,x,y)

      implicit none

      real    para(4),x,y

      y=0
      if ( x.le.para(1) .or. x.ge.para(4) ) then
        y=0
      elseif( x.gt.para(1) .and. x.le.para(2) ) then
        y=(x-para(1))/(para(2)-para(1))
      elseif( x.gt.para(3) .and. x.le.para(4) )then
        y=(para(4)-x)/(para(4)-para(3))
      else
        y=1
      endif

      return
      end subroutine mfun
