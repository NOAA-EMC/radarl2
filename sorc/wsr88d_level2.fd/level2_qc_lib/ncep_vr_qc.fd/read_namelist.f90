
 SUBROUTINE READ_NAMELIST

  use variable_define

  IMPLICIT NONE

  integer :: i,len
  character :: char1
!
! read name list
!
  open(11,file='contr.input3d')

! stop using data file as input for dealiasing package 
!  read(11,*) inrad
  write(*,*)
  write(6,'(/a)')'Input the map projection option'
  write(6,'(a)')'   0:no map projection'
  write(6,'(a)')'   1:polar projection '
  write(6,'(a)')'   2:Lambert projection'
  write(6,'(a)')'   3:Mercator projection'
  read(11,*) mapproj
  write(*,*)
  read(11,*) trulat1
  read(11,*) trulat2
!  read(11,*) trulon
  read(11,*) sclfct
  read(11,*) 
  read(11,*) profile_case
  read(11,*) output_result
  write(*,*)
  write(6,'(/a)')'input profile_case'
  write(6,*) 'profile case ',profile_case
  write(*,*)
  write(6,'(/a)')'input output_result'
  write(6,*) 'output result ',output_result

  close(11)

  if( profile_case /= 'slope_method' .and. &
      profile_case /= 'bayes_method' .and. &
      profile_case /= 'covar_method' .and. &
      profile_case /= 'conti_method' ) then
    stop ' profile case wrong'
  endif

  if( output_result /= 'only_vad' .and. &
      output_result /= 'only_ref' .and. &
      output_result /= 'only_unf' ) then
    stop 'output_result wrong'
  endif

!  outrad='dealiased.dat'

 END SUBROUTINE READ_NAMELIST
