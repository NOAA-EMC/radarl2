
 SUBROUTINE READ_RADAR

  use variable_define

  IMPLICIT NONE

  INTEGER    :: i,j,k
  REAL,DIMENSION(:,:,:),ALLOCATABLE :: array
!
! read radar data
!
  open(unit=47,file=inrad,form='unformatted',status='unknown')

  read(47) radid
  read(47) ireftim,itime,vcpnum
  read(47) iyr,imon,iday,ihr,imin,isec
  read(47) radlat,radlon,radelv

  read(47) irfstgat2
  read(47) irefgatsp2
  read(47) irfstgat
  read(47) irefgatsp

  read(47) nthet
  read(47) (thet(i),i=1,nthet)
  read(47) (nphi(i),i=1,nthet)
  read(47) ((phi(j,i),j=1,nphi(i)),i=1,nthet)

  read(47) iminrng2
  read(47) (imaxrng2(i),i=1,nthet)

  read(47) iminrng
  read(47) (imaxrng(i),i=1,nthet)

  isize = maxval(imaxrng,nthet)
  jsize = maxval(nphi,nthet)
  ksize = nthet
  isize2 = maxval(imaxrng2,nthet)

  print*,'isize,jsize,ksize,isize2',isize,jsize,ksize,isize2

  allocate ( ref  (1:isize2,1:jsize,1:ksize) )
  allocate ( vel  (1:isize ,1:jsize,1:ksize) )
  allocate ( array(1:isize ,1:jsize,1:ksize) )
  allocate ( swg  (1:isize ,1:jsize,1:ksize) )

  ref=spval
  vel=spval
  array=spval

  do k=1,nthet
    nbeam = nphi(k)
    nrang2 = imaxrng2(k)
    nrang = imaxrng(k)
    do j=1,nbeam
      read(47) (ref  (i,j,k),i=iminrng2,nrang2)
      read(47) (vel  (i,j,k),i=iminrng,nrang)
      read(47) (array(i,j,k),i=iminrng,nrang)
!     read(47) (swg  (i,j,k),i=iminrng,nrang)
    enddo
  enddo

  close(47)

  thet_nyq_vel=spval
  do k=1,nthet
   nbeam = nphi(k)
   nrang = imaxrng(k)
   if (k /= 1 .and. k /= 3 ) then
    do j=1,nbeam
    do i=iminrng,nrang
     if( vel(i,j,k) /= spval .and. array(i,j,k) /= spval ) then
!      vnyq=array(i,j,k)
       thet_nyq_vel(k)=array(i,j,k)
       go to 3
     endif
    enddo
    enddo
3   continue
   endif
  enddo

! 3 continue

  print*,'passing read vnyq=',(thet_nyq_vel(k),k=1,nthet)
! print*,'passing read vnyq=',vnyq
  print*,'thet=',(thet(k),k=1,nthet)
  print*,'radid==',radid
  print*, iyr,imon,iday,ihr,imin,isec

!kang nai added
  trulon=radlon
!end kang nai added
  deallocate ( array )
  deallocate ( swg )
 
 RETURN

 END SUBROUTINE READ_RADAR
