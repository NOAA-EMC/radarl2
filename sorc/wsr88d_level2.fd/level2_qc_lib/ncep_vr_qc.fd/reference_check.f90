
      SUBROUTINE REFERENCE_CHECK(wrkchek)

      USE variable_define

      IMPLICIT NONE

      real, dimension(1:nr,1:np) :: wrkchek
      integer :: ii,k,iii
      integer :: iend
      character(len=40) :: name1,name2
      real    :: limit_range,vrqcthrs

      k=k_tilt

      limit_range=80000.0
!     vrqcthrs=0.5*thet_nyq_vel(k)       ! test 1
      vrqcthrs=0.25*thet_nyq_vel(k)
      ii=int(thet(k)*100.0)
      if ( ii<110 ) then
           limit_range=30000.0
      endif
      if ( ii>110 .and. ii<300 ) then
           limit_range=50000.0
      endif
!     ------------------------------------------------------------------
!     get the start gate number and end gate number.
!     ------------------------------------------------------------------
      if ( ii<300 ) then
           iend=20
      endif
      if ( ii>=300 ) then
           iend=0
      endif

      CALL dealias ( nr,np,obsvel,wrkvel,nbeam,iminrng,nrang         &
                    ,vadvel,vnyq,spval,index,ipoint                  &
                    ,wrkchek,iend,vrqcthrs )
 
      CALL vadtilt_check ( nr,np,wrkvel,nbeam,iminrng,nrang          &
                          ,vadvel,vnyq,spval,ran,elvng,index         &
                          ,wrkchek,iend,limit_range )

      END SUBROUTINE REFERENCE_CHECK
