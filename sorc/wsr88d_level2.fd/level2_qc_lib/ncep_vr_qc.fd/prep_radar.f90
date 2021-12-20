
 SUBROUTINE PREP_RADAR
!
! ======================================================================
!   PURPOSE:
!     shift array if first gate less than zero.
! ======================================================================
!
!   Author   : Gong, Jiandong etal
!   Date     :
!   Action   : Created.
!
!   History  :
!  -----------
!   Aug. 14, 2007   Nai, Kang added sort to re-arrange the radar data's
!                  first beam is the northest beam.
!
! ----------------------------------------------------------------------
  use variable_define

  IMPLICIT NONE

  INTEGER    :: i,j,k,iskip,ii,iii
  real, allocatable, dimension(:) :: vel1cyc,ref1cyc,wrksum,wrktmp
  character(len=50) :: name1,name2
  real       :: a1,a2
  integer    :: low_tilt
! ----------------------------------------------------------------------
! if this volume data is qualify to do further job.
! ----------------------------------------------------------------------
  no_qualify_job=.false.
  if ( nthet<=3 ) then
!      open(31,file='no_select_circ.dat')
!      name1='this volume data is not qualify to do dealiase'
!      write(31,'(a46)') name1
!      close(31)
       no_qualify_job=.true.
       RETURN
  endif
! ----------------------------------------------------------------------
! shift radial velocity
! ----------------------------------------------------------------------
  if ( rfstgat.lt.0.0 ) then
       iskip=0
       do i=1,nr
         rfstgat=rfstgat+refgatsp
         if (rfstgat.gt.1.0) exit
       enddo
       iskip=i

       do k=1,nthet
         nbeam = nphi(k)
         nrang = imaxrng(k)
         do j=1,nbeam
         do i=iminrng,nrang-iskip
           vel(i,j,k)=vel(i+iskip,j,k)
         enddo
         enddo
         imaxrng(k)=imaxrng(k)-iskip
!        i=int(150000.0/refgatsp)       ! the max range is 150 km
!        imaxrng(k)=min(i,imaxrng(k))
       enddo
! else
!   i=int(150000.0/refgatsp)
!   do k=1,nthet
!      imaxrng(k)=min(i,imaxrng(k))
!   enddo
  endif
  do i=1,nr
     ran(i) = rfstgat + float(i-1)*refgatsp
  enddo

! ----------------------------------------------------------------------
! write out the original radar data.
! ----------------------------------------------------------------------
! do k=1,nthet
!    nbeam = nphi(k)
!    nrang = imaxrng(k)

!    ii=int( thet(k)*100 )

!    if ( ii<500 ) then
!      write(name1,'(i4.4)') ii
!      name2='orig'//name1(1:4)//'.dat'
!      open(31,file=name2,form='formatted')
!        write(31,'(a4)') radid
!        write(31,'(i8)') vcpnum
!        write(31,'(6i8)') iyr(k),imon(k),iday(k),ihr(k),imin(k),isec(k)
!        write(31,'(2f10.3,f10.1)') radlat,radlon,radelv
!        write(31,'(2f8.1)') rfstgat,refgatsp
!        write(31,'(f8.3)') thet(k)
!        write(31,'(2i8)') nphi(k),imaxrng(k)
!        write(31,'(f8.3)') thet_nyq_vel(k)
!        write(31,'(15f6.1)') (phi(j,k),j=1,nphi(k))
!        write(31,'(20f6.1)') ((vel(i,j,k),i=1,imaxrng(k)),j=1,nphi(k))
!      close(31)
!    endif
! enddo

! ----------------------------------------------------------------------
! shift reflectivity
! ----------------------------------------------------------------------
  if ( rfstgat2.lt.0.0 ) then
       iskip=0
       do i=1,nr2
         rfstgat2=rfstgat2+refgatsp2
         if (rfstgat2.gt.1.0) exit
       enddo
       iskip=i

       do i=1,nr2
         ran2(i) = rfstgat2 + float(i-1)*refgatsp2
       enddo

       do k=1,nthet
         nbeam = nphi2(k)
         nrang2= imaxrng2(k)
         do j=1,nbeam
         do i=iminrng2,nrang2-iskip
           ref(i,j,k)=ref(i+iskip,j,k)
         enddo
         enddo
         imaxrng2(k)=imaxrng2(k)-iskip
!        i=int(150000.0/refgatsp2)           ! the max range is 150 km 
!        imaxrng2(k)=min(i,imaxrng2(k))
       enddo
! else
!   i=int(150000.0/refgatsp2)
!   do k=1,nthet
!     imaxrng2(k)=min(i,imaxrng2(k))
!   enddo
  endif
  do i=1,nr2
     ran2(i) = rfstgat2 + float(i-1)*refgatsp2
  enddo

! ----------------------------------------------------------------------
! sort the data along the azimth( kang nai added )
! ----------------------------------------------------------------------
  allocate( vel1cyc(1:np) )
  allocate( ref1cyc(1:np) )
  allocate( wrksum(1:np) )
  allocate( wrktmp(1:np) )

  do k=1,nthet
     nbeam = nphi(k)
     nrang = imaxrng(k)
     wrksum=spval
     do j=1,nbeam
        wrksum(j)=phi(j,k)
     enddo
     do i=iminrng,nrang
        vel1cyc=spval
        do j=1,nbeam
           vel1cyc(j)=vel(i,j,k)
        enddo
        wrktmp=wrksum; call sort2(nbeam,wrktmp,vel1cyc)
        do j=1,nbeam
           vel(i,j,k)=vel1cyc(j)
        enddo
     enddo
     do j=1,nbeam
        phi(j,k)=wrktmp(j)
     enddo
  enddo
  do k=1,nthet
     nbeam = nphi2(k)
     nrang = imaxrng2(k)
     wrksum=spval
     do j=1,nbeam
        wrksum(j)=phi2(j,k)
     enddo
     do i=iminrng2,imaxrng2(k)
        ref1cyc=spval
        do j=1,nbeam
           ref1cyc(j)=ref(i,j,k)
        enddo
        wrktmp=wrksum; call sort2(nbeam,wrktmp,ref1cyc)
        do j=1,nbeam
           ref(i,j,k)=ref1cyc(j)
        enddo
     enddo
     do j=1,nbeam
        phi2(j,k)=wrktmp(j)
     enddo
  enddo

! ----------------------------------------------------------------------
! get the minimun Nyquist velocity.
! ----------------------------------------------------------------------
  a1=100.0
  a2=0.0
  do k=1,nthet
     if ( thet_nyq_vel(k)<a1 ) then
          a1=thet_nyq_vel(k)
     endif
     if ( thet_nyq_vel(k)>a2 ) then
          a2=thet_nyq_vel(k)
     endif
  enddo
  vnyq_min=a1
  vnyq_max=a2
  print*,' min Nyqv==',vnyq_min,' max Nyqv==', vnyq_max

! ----------------------------------------------------------------------
! get the start_tilt, end_tilt, and step_tilt.
! ----------------------------------------------------------------------
  if ( vcpnum==31 .or. vcpnum==32 ) then
    low_tilt=100
  else
    low_tilt=400
  endif

  start_tilt=nthet
  do k=1,nthet
     ii=int( thet(k)*100 )
     if ( ii>low_tilt ) then
         end_tilt=k
         exit
     endif
  enddo
  step_tilt=-1

  deallocate( vel1cyc,ref1cyc,wrksum,wrktmp )

  return
 END SUBROUTINE PREP_RADAR
