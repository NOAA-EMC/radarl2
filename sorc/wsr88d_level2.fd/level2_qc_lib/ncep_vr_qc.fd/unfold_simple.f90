 SUBROUTINE unfold_simple( maxgate,maxazim,ngate,nazim,rvel,ovel,vnyq &
                          ,unfvel,ivadflag,vad,index,enangle          &
                          ,tmp3,spval,k )
! ======================================================================
!   PURPOSE:
!     Unfold and Quality Control Doppler radial velocities.
! ======================================================================
!
!   Author   : Nai, Kang
!   Date     : Sep. 20, 2007
!   Action   : Created.( used some code copyed from Gong's code)
!
!   History  :
!  -----------
!  name      : Nai, Kang
!  date      : Nov.26, 2007
!  action    : Added final noise remove process.
!
!  name      : Nai, Kang
!  date      : Jun. 9, 2008
!  action    : Modified the check process.
!
!-----------------------------------------------------------------------
!
!  INPUT :
!
!    maxgate  Maximum number of gates in a radial
!    maxazim  Maximum number of radials in a tilt
!    ngate    Number of gates in radial
!    nazim    Number of radials
!    rvel     Doppler radial velocity
!    ovel     Doppler radial velocity
!    vnyq     Nyquist velocity
!    enangle  tilt
! 
!  WORK ARRAYS:
!     
!    tmp_forw
!    indx_forw     
!    tmp_back
!    indx_back
!
!-----------------------------------------------------------------------
!
!  Variable Declarations.
!
!-----------------------------------------------------------------------
!
  IMPLICIT NONE
!
  INTEGER, INTENT(IN) :: maxgate
  INTEGER, INTENT(IN) :: maxazim
  INTEGER, INTENT(IN) :: ngate
  INTEGER, INTENT(IN) :: nazim
  INTEGER, INTENT(IN) :: ivadflag
  INTEGER :: index(maxgate,maxazim)
  
  REAL, INTENT(IN) :: rvel(maxgate,maxazim)
  REAL, INTENT(IN) :: ovel(maxgate,maxazim)
  REAL, INTENT(IN) :: vad(maxgate,maxazim)
  REAL, INTENT(IN) :: vnyq,spval,enangle
  
  REAL, INTENT(IN) :: tmp3(maxgate)
  REAL, INTENT(OUT) :: unfvel(maxgate,maxazim)
  INTEGER, INTENT(IN) :: k

  INTEGER :: igate,iray
  REAL    :: twonyq,inv2nyq,thrpri
  REAL    :: sum,sum2,vavg,tstvel,sum1
  REAL    :: refvel,tstdev,thresh
  integer :: i,ii,j,jj,ip,ik
  integer :: step_step(11)
  logical :: back_check,fwrd_check,near_check
  real    :: a1,a2,a3,a4,a5
  real,allocatable,dimension(:,:) :: tmp_forw,tmp_back
  real,allocatable,dimension(:,:) :: tmp1,tmp2
  integer,allocatable,dimension(:,:) :: indx_forw,indx_back
  real    :: near_thp,back_thp,shield_bottom,limit_number
  character(len=80) :: name1,name2

!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
!  Beginning of executable code...
!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
  twonyq = 2.0*vnyq
  inv2nyq = 1./twonyq
  thrpri = 0.49*vnyq

  near_thp=vnyq/3.0
  near_thp=min(6.0,near_thp)
  back_thp=vnyq/4.0
  back_thp=min(5.0,back_thp)
  shield_bottom=2.6
! print*,'near_thp==',near_thp,vnyq,back_thp,shield_bottom
!
  step_step(1)=0;step_step(2)=-1;step_step(3)=1
  step_step(4)=-2;step_step(5)=2
  step_step(6)=-3;step_step(7)=3
  step_step(8)=-4;step_step(9)=4
  step_step(10)=-5;step_step(11)=5

  ii=int( enangle*100.0 )
  write(name1,'(i4.4)') ii

  allocate( tmp_forw(maxgate,maxazim) )
  allocate( tmp_back(maxgate,maxazim) )
  allocate( indx_forw(maxgate,maxazim) )
  allocate( indx_back(maxgate,maxazim) )
  allocate( tmp1(maxgate,maxazim) )
  allocate( tmp2(maxgate,maxazim) )
!-----------------------------------------------------------------------
! Set up tmp_forward and tmp_back to be quality arrays. 
! set up indx_forward, indx_back; -20=good 10=bad/missing.
!-----------------------------------------------------------------------
  tmp_forw=spval
  tmp_back=spval
  DO iray=1,nazim
    DO igate=1,ngate
      if (abs(rvel(igate,iray))<spval .and. index(igate,iray)==0 ) then
        tmp_forw(igate,iray)=rvel(igate,iray)
        indx_forw(igate,iray)=-20
        tmp_back(igate,iray)=rvel(igate,iray)
        indx_back(igate,iray)=-20
      else
        indx_forw(igate,iray)=10
        indx_back(igate,iray)=10
      endif
    END DO
  END DO
 
!-----------------------------------------------------------------------
! clockwise check.
!-----------------------------------------------------------------------
  tmp2=spval
  Do igate=1,ngate                   
  DO iray=1,nazim                                     ! do 1
    if ( abs(ovel(igate,iray))<spval .and.                           &
         indx_forw(igate,iray)>0 ) then
      a1=0.0
      a2=0.0
      do ii=igate-1,igate-40,-1
        i=max(1,ii)
        do jj=1,11
          j=iray+step_step(jj)
          if ( j<=0 ) j=nazim+j
          if ( j>nazim ) j=j-nazim
          if ( abs(tmp_forw(i,j))<spval .and. indx_forw(i,j)<0 .and.  &
               (tmp3(igate)-tmp3(i))<500.0 ) then
            a1=a1+tmp_forw(i,j)
            a2=a2+1.0
          endif
        enddo
      enddo

      limit_number=40.0
      if ( igate<80 ) limit_number=20.0

      IF ( a2<limit_number ) THEN
        tmp_forw(igate,iray)=spval
      ELSE
        refvel=a1/a2
        tmp2(igate,iray)=refvel
        if ( abs(ovel(igate,iray) - refvel) < thrpri ) then
          tmp_forw(igate,iray)=ovel(igate,iray)
          indx_forw(igate,iray)=-20
        else
          tstdev=twonyq*NINT((ovel(igate,iray)-refvel)*inv2nyq)
          tstvel=ovel(igate,iray)+tstdev
          IF ( abs(tstvel-refvel ) < thrpri ) THEN         ! if D
            tmp_forw(igate,iray)=tstvel
            indx_forw(igate,iray)=0
          ENDIF
        endif

        if ( indx_forw(igate,iray)>0 ) then
          tstvel=ovel(igate,iray)+twonyq
          IF ( abs(tstvel-refvel ) < thrpri ) THEN
            tmp_forw(igate,iray)=tstvel
            indx_forw(igate,iray)=0
          ENDIF
        endif

        if ( indx_forw(igate,iray)>0 ) then
          tstvel=ovel(igate,iray)-twonyq
          IF ( abs(tstvel-refvel ) < thrpri ) THEN
            tmp_forw(igate,iray)=tstvel
            indx_forw(igate,iray)=0
          ENDIF
        endif

        if ( abs(refvel)>near_thp .and.                              &
             abs(tmp_forw(igate,iray))<=shield_bottom ) then
          tmp_forw(igate,iray)=spval
          indx_forw(igate,iray)=10
          tmp2(igate,iray)=spval
        endif

        if ( abs(refvel)>near_thp .and.                              &
             (refvel*tmp_forw(igate,iray))<=0.0 ) then
          tmp_forw(igate,iray)=spval
          indx_forw(igate,iray)=10
          tmp2(igate,iray)=spval
        endif

        if ( indx_forw(igate,iray)==0 ) then
!         --------------------------------------------------------------
!         small continuety check:
!         1), back to radar check. The Vr - the nearest point's Vr
!             must < 10.0 m/s.
!         2), counterclockwise check. Do ligatee 1) but along the circle.
!         --------------------------------------------------------------
          if ( igate>35 ) then
            near_check=.false.
            back_check=.false.
            fwrd_check=.false.
            a3=spval
            do ii=1,10
              i=igate-ii
              if ( abs(tmp_forw(i,iray))<spval ) then
                a3=tmp_forw(i,iray)
                exit
              endif
            enddo

            if (abs(a3-tmp_forw(igate,iray))<near_thp ) then
              near_check=.true.

              a4=spval
              do jj=1,5
                j=iray-jj
                i=igate
                if ( j<=0 ) then
                  j=nazim+j
                  i=igate-1
                endif
                if ( abs(tmp_forw(i,j))<spval) then
                  a4=tmp_forw(i,j)
                  exit
                endif
              enddo
              a5=spval
              do jj=1,5
                j=iray+jj
                if ( j>nazim ) j=j-nazim
                i=igate-1
                if ( abs(tmp_forw(i,j))<spval) then
                  a5=tmp_forw(i,j)
                  exit
                endif
              enddo

              if ( abs(a4)<spval ) then
                if ( abs(a4-tmp_forw(igate,iray))<back_thp ) then
                  back_check=.true.
                endif
              endif

              if ( abs(a5)<spval ) then
                if ( abs(a4-tmp_forw(igate,iray))<back_thp ) then
                  fwrd_check=.true.
                endif
              endif

            else
              near_check=.false.

              a4=spval
              do jj=1,5
                j=iray-jj
                i=igate
                if ( j<=0 ) then
                  j=nazim+j
                  i=igate-1
                endif
                if ( abs(tmp_forw(i,j))<spval) then
                  a4=tmp_forw(i,j)
                  exit
                endif
              enddo
              a5=spval
              do jj=1,5
                j=iray+jj
                if ( j>nazim ) j=j-nazim
                i=igate-1
                if ( abs(tmp_forw(i,j))<spval) then
                  a5=tmp_forw(i,j)
                  exit
                endif
              enddo

              if ( abs(a4)<spval ) then
                if ( abs(a4-tmp_forw(igate,iray))<back_thp ) then
                  back_check=.true.
                endif
              endif
 
              if ( abs(a5)<spval ) then
                if ( abs(a4-tmp_forw(igate,iray))<back_thp ) then
                  fwrd_check=.true.
                endif
              endif

            endif

            if ( near_check ) then
              if ( back_check ) then
                indx_forw(igate,iray)=-20
              else
                if ( fwrd_check ) then
                  indx_forw(igate,iray)=-20
                else
                  indx_forw(igate,iray)=10
                  tmp_forw(igate,iray)=spval
                endif
              endif
            else
              if ( back_check .or. fwrd_check ) then
!             if ( back_check .and. fwrd_check ) then
                indx_forw(igate,iray)=-20
              else
                indx_forw(igate,iray)=10
                tmp_forw(igate,iray)=spval
              endif
            endif
          endif
        endif

      ENDIF
    endif
  enddo    !enddo 1
  enddo    !enddo 0

! write out the check results.
! name2='vadv'//name1(1:4)//'.dat'
! call write_radar(name2,tmp_forw)
! name2='idba'//name1(1:4)//'.dat'
! call write_radar(name2,tmp2)

! tmp1=float(indx_forw)
! name2='vadr'//name1(1:4)//'.dat'
! call write_radar(name2,tmp1)

!-----------------------------------------------------------------------
! counter-clockwise check.
!-----------------------------------------------------------------------
  tmp2=spval
  Do igate=1,ngate
  DO ip=nazim,1,-1                                     ! do 1
    iray=ip
    if ( iray<=0 ) iray=nazim+iray
    if ( abs(ovel(igate,iray))<spval .and.                           &
         indx_back(igate,iray)>0 ) then
      a1=0.0
      a2=0.0
      do ii=igate-1,igate-40,-1
        i=max(1,ii)
        do jj=1,11
          j=iray+step_step(jj)
          if ( j<=0 ) j=nazim+j
          if ( j>nazim ) j=j-nazim
          if ( tmp_back(i,j)<spval .and. indx_back(i,j)<0 .and.      &
               (tmp3(igate)-tmp3(i))<500.0 ) then
            a1=a1+tmp_back(i,j)
            a2=a2+1.0
          endif
        enddo
      enddo

      limit_number=50.0
      if ( igate<80 ) limit_number=20.0

      IF ( a2<limit_number ) THEN
        tmp_back(igate,iray)=spval
      ELSE
        refvel=a1/a2
        tmp2(igate,iray)=refvel
        if ( abs(ovel(igate,iray) - refvel) < thrpri ) then
          tmp_back(igate,iray)=ovel(igate,iray)
          indx_back(igate,iray)=-20
        else
          tstdev=twonyq*NINT((ovel(igate,iray)-refvel)*inv2nyq)
          tstvel=ovel(igate,iray)+tstdev
          IF ( abs(tstvel-refvel ) < thrpri ) THEN         ! if D
            tmp_back(igate,iray)=tstvel
            indx_back(igate,iray)=0
          ENDIF
        endif

        if ( indx_back(igate,iray)>0 ) then
          tstvel=ovel(igate,iray)+twonyq
          IF ( abs(tstvel-refvel ) < thrpri ) THEN
            tmp_back(igate,iray)=tstvel
            indx_back(igate,iray)=0
          ENDIF
        endif

        if ( indx_back(igate,iray)>0 ) then
          tstvel=ovel(igate,iray)-twonyq
          IF ( abs(tstvel-refvel ) < thrpri ) THEN
            tmp_back(igate,iray)=tstvel
            indx_back(igate,iray)=0
          ENDIF
        endif

        if ( abs(refvel)>near_thp .and.                              &
             abs(tmp_back(igate,iray))<=shield_bottom ) then
          tmp_back(igate,iray)=spval
          indx_back(igate,iray)=10
          tmp2(igate,iray)=spval
        endif

        if ( abs(refvel)>near_thp .and.                              &
             (refvel*tmp_back(igate,iray))<=0.0 ) then
          tmp_back(igate,iray)=spval
          indx_back(igate,iray)=10
          tmp2(igate,iray)=spval
        endif

        if ( indx_back(igate,iray)==0 ) then
!         --------------------------------------------------------------
!         small continuety check:
!         1), back to radar check. The Vr - the nearest point's Vr
!             must < 10.0 m/s.
!         2), counterclockwise check. Do ligatee 1) but along the circle.
!         --------------------------------------------------------------
          if ( igate>35 ) then
            near_check=.false.
            back_check=.false.
            fwrd_check=.false.
            a3=spval
            do ii=1,10
              i=igate-ii
              if ( abs(tmp_back(i,iray))<spval ) then
                a3=tmp_back(i,iray)
                exit
              endif
            enddo

            if (abs(a3-tmp_back(igate,iray))<near_thp ) then
              near_check=.true.

              a4=spval
              do jj=1,5
                j=iray+jj
                i=igate
                if ( j>nazim ) then
                  j=j-nazim
                  i=igate-1
                endif
                if ( abs(tmp_back(i,j))<spval) then
                  a4=tmp_back(i,j)
                  exit
                endif
              enddo
              a5=spval
              do jj=1,5
                j=iray-jj
                if ( j<=0) j=j+nazim
                i=igate-1
                if ( abs(tmp_back(i,j))<spval) then
                  a5=tmp_back(i,j)
                  exit
                endif
              enddo

              if ( abs(a4)<spval ) then
                if ( abs(a4-tmp_back(igate,iray))<back_thp ) then
                  back_check=.true.
                endif
              endif

              if ( abs(a5)<spval ) then
                if ( abs(a4-tmp_back(igate,iray))<back_thp ) then
                  fwrd_check=.true.
                endif
              endif

            else
              near_check=.false.

              a4=spval
              do jj=1,5
                j=iray+jj
                i=igate
                if ( j>nazim ) then
                  j=j-nazim
                  i=igate-1
                endif
                if ( abs(tmp_back(i,j))<spval) then
                  a4=tmp_back(i,j)
                  exit
                endif
              enddo
              a5=spval
              do jj=1,5
                j=iray-jj
                if ( j<=0 ) j=j+nazim
                i=igate-1
                if ( abs(tmp_back(i,j))<spval) then
                  a5=tmp_back(i,j)
                  exit
                endif
              enddo

              if ( abs(a4)<spval ) then
                if ( abs(a4-tmp_back(igate,iray))<back_thp ) then
                  back_check=.true.
                endif
              endif

              if ( abs(a5)<spval ) then
                if ( abs(a4-tmp_back(igate,iray))<back_thp ) then
                  fwrd_check=.true.
                endif
              endif

            endif

            if ( near_check ) then
              if ( back_check ) then
                indx_back(igate,iray)=-20
              else
                if ( fwrd_check ) then
                  indx_back(igate,iray)=-20
                else
                  indx_back(igate,iray)=10
                  tmp_back(igate,iray)=spval
                endif
              endif
            else
              if ( back_check .or. fwrd_check ) then
!             if ( back_check .and. fwrd_check ) then
                indx_back(igate,iray)=-20
              else
                indx_back(igate,iray)=10
                tmp_back(igate,iray)=spval
              endif
            endif
          endif
        endif

      ENDIF
    endif
  enddo    !enddo 1
  enddo    !enddo 0

! write out the check results.
! name2='vadc'//name1(1:4)//'.dat'
! call write_radar(name2,tmp_back)
! name2='idbb'//name1(1:4)//'.dat'
! call write_radar(name2,tmp2)

! name2='vadf'//name1(1:4)//'.dat'
! tmp1=float(indx_back)
! call write_radar(name2,tmp1)

!-----------------------------------------------------------------------
! combine the check results.
!-----------------------------------------------------------------------
  unfvel=spval
  DO iray=1,nazim                                     ! do 1
    DO igate=1,ngate                                   ! do 2

      IF ( indx_forw(igate,iray)==-20 ) then
        if ( indx_back(igate,iray)==-20 ) then
          a1=tmp_forw(igate,iray)-tmp_back(igate,iray)
          if ( abs(a1)<0.1 ) then
            unfvel(igate,iray)=tmp_forw(igate,iray)
          else
            unfvel(igate,iray)=spval
          endif
        else
          unfvel(igate,iray)=tmp_forw(igate,iray)
        endif
      ELSE
        IF ( indx_back(igate,iray)==-20 ) then
          unfvel(igate,iray)=tmp_back(igate,iray)
        ENDIF
      ENDIF

    ENDDO
  ENDDO

! write out the results.
! name2='vadn'//name1(1:4)//'.dat'
! call write_radar(name2,unfvel)

  deallocate ( tmp_forw,tmp_back,indx_forw,indx_back,tmp1,tmp2 )
 
  RETURN
  END SUBROUTINE unfold_simple
