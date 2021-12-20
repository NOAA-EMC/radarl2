
 SUBROUTINE WRITE_RADAR

  use variable_define

  IMPLICIT NONE

  INTEGER    :: i,j,k
  REAL,DIMENSION(:,:),ALLOCATABLE :: array
  integer    :: ii
  character(len=40) :: name1,name2
!
  allocate ( array(1:isize ,1:jsize) )

  open(unit=47,file=outrad,form='unformatted')

  write(47) radid
  write(47) ireftim,itime,vcpnum
  write(47) iyr,imon,iday,ihr,imin,isec
  write(47) radlat,radlon,radelv

  write(47) irfstgat2
  write(47) irefgatsp2
  write(47) irfstgat
  write(47) irefgatsp

  write(47) nthet
  write(47) (thet(i),i=1,nthet)
  write(47) (nphi(i),i=1,nthet)
  write(47) ((phi(j,i),j=1,nphi(i)),i=1,nthet)

  write(47) iminrng2
  write(47) (imaxrng2(i),i=1,nthet)

  write(47) iminrng
  write(47) (imaxrng(i),i=1,nthet)

  do k=1,nthet
  nbeam = nphi(k)
  nrang2 = imaxrng2(k)
  nrang = imaxrng(k)
  do j=1,nbeam
    do i=iminrng,nrang
     array(i,j)=vnyq
    enddo
    write(47) (ref  (i,j,k),i=iminrng2,nrang2)
    write(47) (vel  (i,j,k),i=iminrng,nrang)
    write(47) (array(i,j  ),i=iminrng,nrang)
!   write(47) (array(i,j  ),i=iminrng,nrang)
  enddo
  enddo

 close(47)
!nkang
 rfstgat=irfstgat
 refgatsp=irefgatsp
 print*
 print*,'radid==',radid
 print*,iyr,imon,iday,ihr,imin,isec
 print*
 do k =1,nthet
    if ( k /= 1 .and. k /= 3 ) then
         ii=int( thet(k)*100 )
         if ( ii < 500 ) then          ! 5.0 degree
              write(name1,10) ii
10            format(i4.4,'.dat')
              name2='deals'//name1(1:8)
              open(31,file=name2,form='formatted')
                write(31,'(a4)') radid
                write(31,'(i8)') vcpnum
                write(31,'(6i8)') iyr,imon,iday,ihr,imin,isec
                write(31,'(3f8.1)') radlat,radlon,radelv
                write(31,'(2f8.1)') rfstgat,refgatsp
                write(31,'(f8.1)') thet(k)
                write(31,'(2i8)') nphi(k),920
                write(31,'(f6.1)') thet_nyq_vel(k)
                write(31,'(15f6.1)') (phi(j,k),j=1,nphi(k))
                write(31,'(20f6.1)') ((vel(i,j,k),i=1,920),j=1,nphi(k))
              close(31)
         endif
    endif
 enddo

 deallocate ( array )

 END SUBROUTINE WRITE_RADAR
