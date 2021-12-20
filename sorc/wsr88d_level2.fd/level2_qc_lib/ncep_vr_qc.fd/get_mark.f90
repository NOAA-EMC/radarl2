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
!       Mark the birds, ground clutter and sea clutter.
!=======================================================================
!
!     Author   : Jiang, Yuan
!     Date     : Mar. 9, 2011
!     Action   : Created.
!
!     Modify History:
!     ---------------
!     Aug. 1, 2012: Kang Nai change it from the Pro_radar to get_mark.
!
!=======================================================================
      subroutine get_mark

      use sdata
      use  variable_define

      implicit none

      integer :: i,j,k
      real,allocatable,dimension(:,:) :: wrkvr,wrkkdp,wrkrho,tmp
      real,allocatable,dimension(:,:) :: wrkrf,wrkswg,wrkzdr

!     ------------------------------------------------------------------
!     ------------------------------------------------------------------
      print*,'enter get_mark' 
      allocate( velref(1:nr,1:np,1:nt) )
      allocate( tmp(1:nr,1:np) )        ! work array
      allocate( wrkkdp(1:nr,1:np) )     ! kdp present
      allocate( wrkvr(1:nr,1:np) )     ! vel present
      allocate( wrkrf(1:nr,1:np) )     ! ref present
      allocate( wrkswg(1:nr,1:np) )     ! sw present
      allocate( wrkrho(1:nr,1:np) )     ! rho present
      allocate( wrkzdr(1:nr,1:np) )     ! zdr present
! ----------------------------------------------------------------------
!     noise_remove
! ----------------------------------------------------------------------
      velref=vel

      do k=1,nthet
        nbeam = nphi(k)
        nrang = imaxrng(k)
        tmp=spval
        do j=1,nbeam
        do i=1,nrang
          tmp(i,j)=velref(i,j,k)       ! velocity
        enddo
        enddo
        call noise_rm( nr,np,tmp,nbeam,1,nrang,spval )
        do j=1,nbeam
        do i=1,nrang
          velref(i,j,k)=tmp(i,j)
        enddo
        enddo

        tmp=spval
        do j=1,nbeam
        do i=1,nrang
          tmp(i,j)=swg(i,j,k)          ! spetrum width
        enddo
        enddo
        call noise_rm( nr,np,tmp,nbeam,1,nrang,spval )
        do j=1,nbeam
        do i=1,nrang
          swg(i,j,k)=tmp(i,j)
        enddo
        enddo

      print*,'get_mark1' 
        tmp=spval
        do j=1,nbeam
        do i=1,nrang
          tmp(i,j)=zdr(i,j,k)          ! zdr
        enddo
        enddo
        call noise_rm( nr,np,tmp,nbeam,1,nrang,spval )
        do j=1,nbeam
        do i=1,nrang
          zdr(i,j,k)=tmp(i,j)
        enddo
        enddo

        tmp=spval
        do j=1,nbeam
        do i=1,nrang
          tmp(i,j)=rho(i,j,k)          ! rho
        enddo
        enddo
        call noise_rm( nr,np,tmp,nbeam,1,nrang,spval )
        do j=1,nbeam
        do i=1,nrang
          rho(i,j,k)=tmp(i,j)
        enddo
        enddo

        tmp=spval
        do j=1,nbeam
        do i=1,nrang
          tmp(i,j)=kdp(i,j,k)          ! kdp
        enddo
        enddo
        call noise_rm( nr,np,tmp,nbeam,1,nrang,spval )
        do j=1,nbeam
        do i=1,nrang
          kdp(i,j,k)=tmp(i,j)
        enddo
        enddo

        nbeam = nphi2(k)
        nrang = imaxrng2(k)
        tmp=spval
        do j=1,nbeam
        do i=1,nrang
          tmp(i,j)=ref(i,j,k)          ! reflevtivity
        enddo
        enddo
        call noise_rm( nr,np,tmp,nbeam,1,nrang,spval )
        do j=1,nbeam
        do i=1,nrang
          ref(i,j,k)=tmp(i,j)
        enddo
        enddo
      enddo

      print*,'get_mark2' 

      do k=1,nthet
        nbeam = nphi(k)
        nrang = imaxrng(k)

        wrkvel=spval
        do j=1,nbeam
        do i=1,nrang
          wrkvr(i,j)=velref(i,j,k)    ! velref
        enddo
        enddo

        wrkswg=spval
        do j=1,nbeam
        do i=1,nrang
          wrkswg(i,j)=swg(i,j,k)       ! swg
        enddo
        enddo

        wrkrho=spval
        do j=1,nbeam
        do i=1,nrang
          wrkrho(i,j)=rho(i,j,k)       ! rho
        enddo
        enddo

        wrkkdp=spval
        do j=1,nbeam
        do i=1,nrang
          wrkkdp(i,j)=kdp(i,j,k)       ! kdp
        enddo
        enddo

        wrkzdr=spval
        do j=1,nbeam
        do i=1,nrang
          wrkzdr(i,j)=zdr(i,j,k)       ! zdr
        enddo
        enddo

        nbeam = nphi2(k)
        nrang = imaxrng2(k)
        wrkrf=spval
        write(*,*)'in get_mark:',nr,np,nphi(k),imaxrng(k),nphi2(k),imaxrng2(k)
        do j=1,nbeam
        do i=1,nrang
          wrkrf(i,j)=ref(i,j,k)       ! ref1
        enddo
        enddo

      print*,'get_mark3' 
      print*,np,nr,nphi2(k),imaxrng2(k),nphi(k),imaxrng(k)
        tmp=0.0
        call qc_nonm( np,nr,nphi2(k),imaxrng2(k),wrkrf              &
                     ,nphi(k),imaxrng(k),wrkvr                      &
                     ,wrkswg,wrkzdr,wrkkdp,wrkrho,tmp                &
                     ,spval )
      print*,'get_mark4' 
        do j=1,np
        do i=1,nr
          no_mark(i,j,k)=tmp(i,j)
        enddo
        enddo

      end do

      deallocate(velref,tmp)
      deallocate(wrkvr,wrkrf,wrkswg)
      deallocate(wrkrho,wrkkdp,wrkzdr)

      print*,'end get_mark' 
      return
      END subroutine get_mark
