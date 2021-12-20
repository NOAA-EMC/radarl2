
SUBROUTINE Multi_check_driver

! ======================================================================
!   PURPOSE:
!     Using the wind profile to check the whole wind field if it need
!     to unfold.
! ======================================================================
!
!   Author   : Gong, Jiandong et al.
!   Date     :
!   Action   : Created.
!
!   History  :
!  -----------
!   name     : Nai, Kang
!   date     : Aug. 15, 2007
!   action   : Modified it to fit the new two step dealiase agorithm.
!
! ----------------------------------------------------------------------

  use variable_define

  implicit none

  integer  :: i,j,k,kp,kpp
  character(len=40) :: name1,name2,name3

  real, dimension(1:nr,1:np) :: wrkchek
  real, dimension(1:nr,1:np) :: chekwrk
  integer  :: need_unfold_check
  integer  :: ii,iend

  allocate ( obsvel( 1:nr, 1:np ) )
  allocate ( vadvel( 1:nr, 1:np ) )
  allocate ( wrkvel( 1:nr, 1:np ) )
  allocate ( index ( 1:nr, 1:np ) )
  allocate ( unfvel( 1:nr, 1:np ) )


  do k_tilt=1,nthet
     k=k_tilt
     nbeam = nphi(k)
     nrang = imaxrng(k)
     elvng = thet(k)
     vnyq  = thet_nyq_vel(k)
     kp=int( thet(k)*100.0 )
     write(name1,10) kp
10   format(i4.4,'.dat')

!    -------------------------------------------------------------------
!    filling in the missing value in the tmp work array.
!    -------------------------------------------------------------------
     obsvel=spval
     vadvel=spval
     wrkvel=spval
     index=0
     unfvel=spval

!    -------------------------------------------------------------------
!    put the fold field to a tmp work field.
!    -------------------------------------------------------------------
     need_unfold_check=0
     do j=1,nbeam; do i=iminrng,nrang
        obsvel(i,j)=vel(i,j,k)
        if(abs(obsvel(i,j))<spval .and. abs(obsvel(i,j))>0.8*vnyq) then
           need_unfold_check=need_unfold_check+1
        endif
     enddo; enddo

     if ( need_unfold_check==0 ) then
!         name2='keep'//name1(1:8)
!         name3='no need to unfold'
!         open(31,file=name2)
!           write(31,'(a17)') name3
!         close(31)
          do j=1,nbeam; do i=iminrng,nrang
             unfvel(i,j)=vel(i,j,k)
          enddo; enddo
          go to 20
     endif

     if ( ivadflag>0 ) then

       if ( vcpnum==31 .or. vcpnum==32 ) then
!        do j=1,nbeam; do i=iminrng,nrang
         do j=1,nbeam; do i=121,nrang                 ! 30km
           if ( abs(obsvel(i,j))<0.6 ) obsvel(i,j)=spval
         enddo; enddo
       endif
!      -----------------------------------------------------------------
!      output the adjusted original field.
!      -----------------------------------------------------------------
!      name2='vadv'//name1(1:8)
!      call write_radar(name2,obsvel)

!      -----------------------------------------------------------------
!      calculate reference vad speed from wind profile.
!      -----------------------------------------------------------------
       call cal_vadvel

!      -----------------------------------------------------------------
!      write out the VAD field if needed.
!      -----------------------------------------------------------------
!      name2='vadr'//name1(1:8)
!      call write_radar(name2,vadvel)
 
!      -----------------------------------------------------------------
!      reference check; do <80 km unfold and make a index array.
!      -----------------------------------------------------------------
       call reference_check(wrkchek)

!      -----------------------------------------------------------------
!      write out the results if needed.
!      -----------------------------------------------------------------
!      chekwrk=spval
!      do j=1,nbeam; do i=iminrng,nrang
!        chekwrk(i,j)=wrkvel(i,j)
!        if ( index(i,j) == 10 ) then
!             chekwrk(i,j) = spval
!        endif
!      enddo; enddo
!      name2='vadf'//name1(1:8)
!      call write_radar(name2,chekwrk)
!      name2='vadn'//name1(1:8)
!      call write_radar(name2,wrkchek)

!      -----------------------------------------------------------------
!      whole body check if the wind needs to be unfold.
!      -----------------------------------------------------------------
       call continue_check

     else              ! ivadflag<=0

!      -----------------------------------------------------------------
!      body check, filling in the missing value to special points.
!      -----------------------------------------------------------------
       call filling_spval

     endif

 20  continue
!    -------------------------------------------------------------------
!    replace the fold filed by the unfold field.
!    -------------------------------------------------------------------
     ii=int(thet(k)*100.0)
     if ( ii<300 ) then
          iend=20
     endif
     if ( ii>=300 ) then
          iend=10
     endif

     do j=1,nbeam
        do i=iminrng,nrang
           if ( i<=iend ) then
                vel(i,j,k)=spval
           else
                vel(i,j,k)=unfvel(i,j)
           endif
        enddo
     enddo

  enddo

  deallocate ( obsvel )
  deallocate ( vadvel )
  deallocate ( unfvel )
  deallocate ( wrkvel )
  deallocate ( index  )

END SUBROUTINE Multi_check_driver
