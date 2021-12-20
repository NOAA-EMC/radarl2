
 subroutine dealias( nr,np,obsvel,wrk,nbeam,iminrng,nrang            &
                    ,vadvel,vnyq,spval,index,ipoint                  &
                    ,wrkchek,iend,vrqcthrs )
!
! dealiasing using reference check method
!
  implicit none

  integer nr,np
  real,   dimension(nr,np) :: obsvel,wrk
  integer                  :: nbeam, iminrng, nrang
  real,   dimension(nr,np) :: vadvel
  real                     :: vnyq, spval
  integer,dimension(nr,np) :: index
  integer                  :: ipoint
  real,   dimension(nr,np) :: wrkchek
  integer                  :: iend
  real                     :: vrqcthrs

  integer :: i,j,ip,jp,ims,inr,inp
  real    :: vrdiff,avrdiff,nyq2x
  integer :: nycor

  ipoint=0
  wrk=obsvel

  do j=1,nbeam
  do i=iminrng,nrang

    if ( i<=iend ) then
         wrk(i,j)=spval
    elseif ( abs(wrk(i,j))<spval .and. abs(vadvel(i,j))>900.0 )then
        index(i,j)=10        ! should reconsider these points.
        ipoint=ipoint+1
    else
      if ( abs(wrk(i,j))<spval .and. abs(vadvel(i,j))<spval )then

        nyq2x = 2.*vnyq
        vrdiff = wrk(i,j) - vadvel(i,j)
        nycor = nint(vrdiff/nyq2x)             ! Nyquest interval
        wrk(i,j) = wrk(i,j) - float(nycor)*nyq2x
        vrdiff = wrk(i,j) - vadvel(i,j)
        avrdiff = abs(vrdiff)

        if(avrdiff > vrqcthrs) then
          if(     abs(wrk(i,j)+nyq2x-vadvel(i,j)) <= vrqcthrs ) then
            wrk(i,j) = wrk(i,j) + nyq2x
!           index(i,j)=2

          elseif( abs(wrk(i,j)-nyq2x-vadvel(i,j)) <= vrqcthrs ) then
            wrk(i,j) = wrk(i,j) - nyq2x
!           index(i,j)=2

          else
            wrk(i,j) = obsvel(i,j)
            index(i,j)=10     ! should reconsider these points.
            ipoint=ipoint+1
          endif
        endif
        if ( abs(vadvel(i,j))>10.0 .and. abs(wrk(i,j))<=3.5 ) then
             wrk(i,j)=spval
             index(i,j)=10
        end if
      end if
    endif
  enddo
  enddo

  wrkchek=spval
  do j=1,nbeam
  do i=iend+1,nrang
     if ( abs(wrk(i,j))<spval ) then
       if ( abs(vadvel(i,j))<spval )then
         wrkchek(i,j)=-10.0
         if ( index(i,j)/=0 ) then
           wrkchek(i,j)=10.0
         endif
       else
         wrkchek(i,j)=10.0
       endif
     endif
  enddo
  enddo

 return
 end  subroutine dealias
