
 SUBROUTINE SETUP_NOISE_REMOVE

! ======================================================================
!   PURPOSE:
!     remove the noise data in the raw data.
! ======================================================================
!   Author   : Gong, Jiandong et al.
!   Date     :
!   Action   : Created. Based on Gu, Wei's method.
!
!   History  :
!  -----------
!   Aug.20, 2007    Kang Nai modified it to remove soled one or two beam
!                   data.
!
!   Nov.20, 2007    Kang Nai added to remove the 0Vr noise which shows
!                   near the 150 km at lower the 2.5 tilts, and some
!                   time on the higher tilts in the calm weather day.
!
!   Feb.08, 2008    Kang Nai added to remove the bulk 0Vr area.
! ----------------------------------------------------------------------

  use variable_define

  IMPLICIT NONE

  INTEGER :: i,j,k
  integer :: ii,ispecial,iii
  real,dimension(1:np) :: veltmp
  real    :: a1,a2
  character(len=40) :: name1,name2,name3
  real,dimension(nr,np) :: tmp1,tmp2

! ----------------------------------------------------------------------
! filling in the missing value to no-available point.
! ----------------------------------------------------------------------
  do k=1,nthet
     nbeam = nphi(k)
     nrang = imaxrng(k)
     a1=thet_nyq_vel(k)
     do j=1,nbeam
     do i=iminrng,nrang
        if ( abs(vel(i,j,k))>a1 ) vel(i,j,k)=spval
     enddo
     enddo
  enddo

! ----------------------------------------------------------------------
! noise_remove
! ----------------------------------------------------------------------
  do k=1,nthet
    nbeam = nphi(k)
    nrang = imaxrng(k)
    call noise_rm( nr,np,vel(1,1,k),nbeam,iminrng,nrang,spval)
    call area0vr_rm( nr,np,vel(1,1,k),nbeam,iminrng,nrang,spval     &
                    ,tmp1,tmp2)
   
!   ii=int( wrt_thet(k)*100 )
!   write(name1,'(i4.4)') ii
!   name2='idba'//name1(1:4)//'.dat'
!   open(31,file=name2,form='formatted')
!     write(31,'(a4)') radid
!     write(31,'(i8)') vcpnum
!     write(31,'(6i8)') iyr(k),imon(k),iday(k),ihr(k),imin(k),isec(k)
!     write(31,'(2f10.3,f10.1)') radlat,radlon,radelv
!     write(31,'(2f8.1)') rfstgat,refgatsp
!     write(31,'(f8.1)') thet(k)
!     write(31,'(2i8)') nphi(k),imaxrng(k)
!     write(31,'(f6.1)') thet_nyq_vel(k)
!     write(31,'(15f6.1)') (phi(j,k),j=1,nphi(k))
!     write(31,'(20f6.1)') ((tmp1(i,j),i=1,imaxrng(k)),j=1,nphi(k))
!   close(31)
!   name2='idbb'//name1(1:4)//'.dat'
!   open(31,file=name2,form='formatted')
!     write(31,'(a4)') radid
!     write(31,'(i8)') vcpnum
!     write(31,'(6i8)') iyr(k),imon(k),iday(k),ihr(k),imin(k),isec(k)
!     write(31,'(2f10.3,f10.1)') radlat,radlon,radelv
!     write(31,'(2f8.1)') rfstgat,refgatsp
!     write(31,'(f8.1)') thet(k)
!     write(31,'(2i8)') nphi(k),imaxrng(k)
!     write(31,'(f6.1)') thet_nyq_vel(k)
!     write(31,'(15f6.1)') (phi(j,k),j=1,nphi(k))
!     write(31,'(20f6.1)') ((tmp2(i,j),i=1,imaxrng(k)),j=1,nphi(k))
!   close(31)
  enddo

! ----------------------------------------------------------------------
! remove the 0vr noise which usually happened about 150km at 0.5 and
! 1.5 tilts.
! ----------------------------------------------------------------------
  do k=1,nthet
     nbeam = nphi(k)
     nrang = imaxrng(k)
     do i=1,nrang
        veltmp=spval
        do j=1,nbeam
           veltmp(j)=vel(i,j,k)
        enddo
        a1=0.0
        a2=0.0
        do j=1,nbeam
           if ( abs(veltmp(j))<0.05 ) then
                a1=a1+1.0
                a2=a2+1.0
           else
                a2=0.0
           endif
           if ( a2>=10 ) then
                veltmp=spval
                exit
           endif
           if ( a1>20 ) then
                veltmp=spval
                exit
           endif
        enddo
        do j=1,nbeam
           vel(i,j,k)=veltmp(j)
        enddo
     enddo
!    -------------------------------------------------------------------
!    write out the original noise remove radar data.
!    -------------------------------------------------------------------
!    ii=int( thet(k)*100 )

!    if ( ii<500 ) then
!      write(name1,'(i4.4)') ii
!      name2='vadv'//name1(1:4)//'.dat'
!      open(31,file=name2,form='formatted')
!        write(31,'(a4)') radid
!        write(31,'(i8)') vcpnum
!        write(31,'(6i8)') iyr(k),imon(k),iday(k),ihr(k),imin(k),isec(k)
!        write(31,'(2f10.3,f10.1)') radlat,radlon,radelv
!        write(31,'(2f8.1)') rfstgat,refgatsp
!        write(31,'(f8.1)') thet(k)
!        write(31,'(2i8)') nphi(k),imaxrng(k)
!        write(31,'(f6.1)') thet_nyq_vel(k)
!        write(31,'(15f6.1)') (phi(j,k),j=1,nphi(k))
!        write(31,'(20f6.1)') ((vel(i,j,k),i=1,imaxrng(k)),j=1,nphi(k))
!      close(31)
!    endif
  enddo

  print*, 'finished noise remove'
  print*,' ' 

 END SUBROUTINE SETUP_NOISE_REMOVE
