
 SUBROUTINE Setup_circle

! ======================================================================
!   PURPOSE:
!     choice the circle gate number which fitted the minimun available
!     data number on the circle on the standard height from the ground.
! ======================================================================
!
!   Author   :  Gong, Jiandong et al.
!   Date     : 
!   Action   :  Created.
!
!   History  :
! ------------
!   Aug. 24,2007  Nai, Kang change the minimum available value number
!                 from 0.6 to 0.4. Added height differ critical value.
! ----------------------------------------------------------------------

  use variable_define

  implicit none

  integer :: i,j,k,ii,jj,npts,nd,nc
  logical :: found
  real    :: tmp1,tmp2,a1
! real,dimension(:,:),allocatable :: wrksfc
  character(len=50) :: name1

! ----------------------------------------------------------------------
! setup high for wind profile
! ----------------------------------------------------------------------
  hstor(1) = 50.0
  zstep    = 50.0
  do ilevel=2,zlevel
    hstor(ilevel) = hstor(1) + (ilevel-1) * zstep
  enddo

! ----------------------------------------------------------------------
! filling in the missing value to the profile.
! ----------------------------------------------------------------------
  ustor=spval; vstor=spval
  cf1stor=spval; cf2stor=spval; cf3stor=spval

! ----------------------------------------------------------------------
! setup cycles 
! ----------------------------------------------------------------------
  allocate ( wrksfc( 1:nr,1:nt) )

  do k=1,nthet
     nbeam = nphi(k)
     nrang = imaxrng(k)
     elvng = thet(k)
     do i=iminrng,nrang
        range=ran(i)
        call beamhgt(elvng,range,hgtrad,sfcrng)
        wrkhgt(i,k)=hgtrad 
        wrksfc(i,k)=range       ! Nai, Kang added
     enddo
  enddo
     
  select_circ=0
  do ilevel=1,zlevel-1
     do k=1,nthet
        nbeam = nphi(k)
        nrang = imaxrng(k)
        elvng = thet(k)
        tmp2 =10000.0
        do i=iminrng,nrang
           tmp1=abs(wrkhgt(i,k)-hstor(ilevel))
           if ( tmp1<tmp2 ) then
                tmp2=tmp1
                ii=i
           endif
        enddo
        sfcrng=wrksfc(ii,k)
        if ( tmp2<25.0 .and. sfcrng.le.80000.0 ) then
             select_circ(k,ilevel) = ii
        endif
     enddo
  enddo

! =====================================================================
! select cycle
! =====================================================================
  do ilevel=1,zlevel
     do k=1,nthet
        nbeam = nphi(k)
        nrang = imaxrng(k)

        ii=select_circ(k,ilevel)
        if ( ii/=0 ) then
             found=.false.
             do i=ii,ii-2,-1
                npts=0
                do j=1,nbeam
                   if ( abs(vel(i,j,k))<spval ) npts=npts+1
                enddo
                if ( real(npts)/nbeam >= 0.40 ) then
                     select_circ(k,ilevel)=i
                     found=.true.
                     exit
                endif
             enddo

             if ( .not. found ) then
                  do i=ii+1,ii+2
                     npts=0
                     do j=1,nbeam
                        if ( abs(vel(i,j,k))<spval ) npts=npts+1
                     enddo
                     if ( real(npts)/nbeam >= 0.40 ) then
                          select_circ(k,ilevel)=i
                          found=.true.
                          exit
                     endif
                  enddo
             endif

             if ( .not. found ) then
                  select_circ(k,ilevel)=0
             endif
        endif
     enddo
  enddo

! ======================================================================
! count the selected  circle's number which is higher the 200 meters and
! lower then 800 meters. (Kang Nai added)
! ======================================================================
  no_qualify_job=.false.
  nd=0
  do ilevel=4,16
     nc=0
     do k=1,nthet
        if ( select_circ(k,ilevel)>0.0 ) nc=nc+1
     enddo
     if ( nc>2 ) then
          nd=nd+1
     endif
  enddo
  if ( nd<=2 ) then
       no_qualify_job=.true.
       print*,' '
       print*,' This volume radar data is not qualify to dealise.'
       print*,' '
!      name1='this volume data is not qualify to do dealiase'
!      open(31,file='no_select_circ.dat')
!        write(31,'(a46)') name1
!      close(31)
       RETURN
  endif

! ======================================================================
! get the max height of the select circ.
! ======================================================================
! a1=0.0
! do ii=1,zlevel
! do k=1,nthet
!    i=select_circ(k,ii)
!    if ( i>0 ) then
!         if ( wrkhgt(i,k)>a1 ) then
!              a1=wrkhgt(i,k)
!         endif
!    endif
! enddo
! enddo

! print*,' '
! print*,' Max height ==',a1
! print*,' '
! do ii=1,zlevel
!    write(*,'(20i4)') (select_circ(k,ii),k=1,nthet)
! enddo
! print*,' '

  deallocate ( wrksfc )

END SUBROUTINE Setup_circle
