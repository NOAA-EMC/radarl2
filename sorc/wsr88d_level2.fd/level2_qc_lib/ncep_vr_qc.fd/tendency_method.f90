
SUBROUTINE tendency_method

! ======================================================================
!   PURPOSE:
!     Using 0Vr line and the tendance around the 0Vr line to unfold
!     the sellect circle, then using classical VAD method to get the
!     wind profile.
! ======================================================================
!
!   Author   : Nai, Kang
!   Date     : Aug. 15, 2007
!   Action   : Created.
!
!   History  :
!  -----------
!   name     : Nai,Kang
!   date     : Oct. 8, 2007
!   Action   : Changed the method to max/min Vr method.
!
!   name     : Nai,Kang
!   date     : Oct.16, 2007
!   action   : Using line_regression to get the united 0Vr.
!              (Qin Xu's idea).
!
! ----------------------------------------------------------------------
  use variable_define

  implicit none

  logical :: first_tilt
  real    :: priority_small_phi
  real    :: priority_large_phi
  real    :: priority_crit
  integer :: priority_level
  integer :: extend_small_phi
  integer :: extend_large_phi
  integer :: no_extend

  real    :: vbest
  real    :: sigma

  integer :: i,j,k,kk,kp,kmin
  integer :: mcount,test_style
  integer :: start_level,end_level
  real    :: a1,a2,b1,b2
  real,dimension(13) :: tmp_cost,tmp_v,test_v
  character(len=40) :: name1,name2
  logical :: consistency

! ######################################################################
! Execute.
! ######################################################################
  print*,' '
  print*,'wind profile by tendence method'
  print*,' '

! ----------------------------------------------------------------------
! get some initial value to parameters.
! ----------------------------------------------------------------------
  test_v(1)=1.0
  test_v(2)=2.0
  test_v(3)=5.0
  test_v(4)=10.0
  test_v(5)=20.0
  test_v(6)=30.0
  test_v(7)=40.0
  test_v(8)=50.0
  test_v(9)=60.0
  test_v(10)=70.0
  test_v(11)=80.0
  test_v(12)=90.0
  test_v(13)=100.0

  cost_f=spval

  check_small_phi=spval
  check_large_phi=spval
  check_crit=spval

  first_tilt=.true.
  priority_small_phi=spval
  priority_large_phi=spval
  priority_crit=spval
  priority_level=999
  extend_small_phi=0
  extend_large_phi=0
  no_extend=0

  start_level=5                ! the bottom height=start_level*50 meters
! start_level=10                ! the bottom height=start_level*50 meters
  end_level=zlevel             ! the top height=end_level*50 meters

  do ilevel=start_level,end_level

     if ( first_tilt ) then
!      =================================================================
!      get the first tilt's angles which has a remarkable 0Vr.
!      =================================================================
       call gradient_fst_0Vr ( first_tilt                            &
                              ,priority_small_phi                    &
                              ,priority_large_phi                    &
                              ,priority_crit                         &
                              ,priority_level )

     endif

     if ( .not. first_tilt ) then

!      =================================================================
!      put the available data into statistics box to get the united 0Vr.
!      =================================================================
       call tendency_0Vr( priority_small_phi                         &
                         ,priority_large_phi                         &
                         ,priority_crit                              &
                         ,priority_level                             &
                         ,extend_small_phi                           &
                         ,extend_large_phi                           &
                         ,no_extend )

       if ( no_extend.ge.10 ) exit

       if ( extend_small_phi>4 .or. extend_large_phi>4 ) then
!           open(31,file='extend_over.dat')
!             write(31,'(i6)') 4
!           close(31)
            exit
       endif

       if ( check_small_phi(ilevel)<spval .and.                     &
            check_large_phi(ilevel)<spval) then

!        ---------------------------------------------------------------
!        do rough abs(V) estimate.
!        ---------------------------------------------------------------
         sigma=spval
!        mcount=1
   10    continue
         a1=1.0E+10
         kmin=0
         kk=0
         do k=1,13
            kk=kk+1
            tmp_v(kk)=test_v(k)
            call cost_function( test_v(k),sigma,priority_crit )
            tmp_cost(kk)=cost_f(2)
            if ( cost_f(2)<a1 ) then
                 a1=cost_f(2)
                 kmin=kk
            endif
         enddo

!        if ( kmin==1 ) then
!             ustor(ilevel)=spval
!             vstor(ilevel)=spval
!             go to 20
!        endif

!        ---------------------------------------------------------------
!        choice the parabola regression style:
!        test_style=1;  using 0Vr gradient to do parabola regression.
!        test_style=2;  using three points to do parabola regression.
!        ---------------------------------------------------------------
         test_style=2
         call search_min( test_style,kmin,tmp_cost,tmp_v             &
                         ,vbest,sigma,priority_crit )

!        ---------------------------------------------------------------
!        using the vbest to get the field's u0, v0.
!        ---------------------------------------------------------------
         call get_u0v0( vbest,priority_crit,consistency,mcount )
         print*,'ustor==',ustor(ilevel),' vstor==',vstor(ilevel),ilevel

!        if ( consistency ) then
!          -------------------------------------------------------------
!          calculate the thoery curve based on cost_function2.
!          -------------------------------------------------------------
!          call get_residual( vbest,priority_crit,sigma              &
!                            ,mcount )
    20     continue
!          mcount=mcount+1
!          if ( mcount==5 ) go to 10
!        endif
       endif
     endif
  enddo

END SUBROUTINE tendency_method
