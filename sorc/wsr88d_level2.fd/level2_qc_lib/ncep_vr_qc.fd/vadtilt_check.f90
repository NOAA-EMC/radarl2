
 subroutine vadtilt_check( nr,np,wrk,nbeam,iminrng,nrang             &
                          ,vadvel,vnyq,spval,ran,elvng,index         &
                          ,wrkchek,iend,limit_range )

  implicit none

  integer nr,np
  real,   dimension(nr,np) :: wrk
  integer                  :: nbeam,iminrng,nrang
  real,   dimension(nr,np) :: vadvel
  real                     :: vnyq, spval
  real,   dimension(nr)    :: ran
  real                     :: elvng
  integer,dimension(nr,np) :: index
  real,   dimension(nr,np) :: wrkchek
  integer :: iend
  real                     :: limit_range

  integer,dimension(:,:),allocatable :: iindex
  real    :: vel2,velmean,velvar
  real    :: range,hgtrad,sfcrng

  integer :: i,j,ip,jp,ims,inr,inp,ipst,ipen
  integer :: nycor,ipoint
  integer :: npts,iii,jjj,i0,j0
  real    :: a1,a2,refvel
  integer :: j1,j2

! ----------------------------------------------------------------------
! This step is a point-mark process that is suitable with a 3x3 window.
! ----------------------------------------------------------------------

  allocate ( iindex(1:nr,1:np) )

  iindex=0
  ipoint=0

if (1==1 ) then
  do j=1,nbeam
  do i=iend+1,nrang
    if ( abs(wrk(i,j))<spval ) then

      npts  = 0
      velmean=0.0
      do jjj=-1,1
         j0=j + jjj
         if ( j0<=0 ) j0=j0+nbeam
         if ( j0>nbeam) j0=j0-nbeam
         do iii=-1,1
            i0 = i + iii
            i0=max(1,i0)
            i0=min(i0,nrang)
            if ( abs(wrk(i0,j0))<spval .and. index(i0,j0)==0 ) then
                 npts  =  npts + 1
                 velmean=velmean+wrk(i0,j0)
            end if
         end do
      end do

      if(npts >= 5) then
       velmean = velmean/npts
       if( abs(wrk(i,j)-velmean) > 5.0 ) iindex(i,j)=10
      else
       iindex(i,j)=10
      endif

    endif
  enddo
  enddo

  do j=1,nbeam
  do i=iend+1,nrang
    if( index(i,j) == 0 .and. iindex(i,j).eq.10 ) then
      index(i,j)=10
      wrkchek(i,j)=10.0
      ipoint=ipoint+1
    endif
  enddo
  enddo
endif

  deallocate ( iindex )

if (1==0) then
! ----------------------------------------------------------------------
! Mark points in beam less than 5 gates.
! ----------------------------------------------------------------------
  do j=1,nbeam

    ip=iend+1                         ! Nai,Kang added
1   continue
    ipst=0
    if(ip < nrang) then
      do i=ip,nrang
        if(abs(wrk(i,j))<spval .and. index(i,j)==0 ) then
           ipst=i
           exit
        endif
      enddo

      if(ipst > 0 .AND. ipst < nrang) then
        ipen=nrang
        do i=ipst+1,nrang
          if ( abs(wrk(i,j))<spval .and. index(i,j)==10 .OR.         &
               abs(wrk(i,j))>900.0  ) then
            ipen=i-1
            exit
          endif
        enddo

!       if( ipen-ipst <= 1+int(j/40.0) ) then
        if( ipen-ipst <= 5 ) then

          jp=0
          j1=j+1
          if (j1>nbeam) j1=j1-nbeam
          j2=j-1
          if(j2<=0) j2=j2+nbeam
          do i=ipst,ipen
            if(abs(wrk(i,j1))<spval.and.index(i,j1)==0) jp=jp+1
            if(abs(wrk(i,j2))<spval.and.index(i,j2)==0) jp=jp+1
          enddo
!         if(jp <=  2+int(j/40.0) ) then
          if(jp <= 5 ) then
            do i=ipst,ipen
              index(i,j) =10
              wrkchek(i,j) =10
            enddo
          endif

        endif

      endif
      ip=i
      go to 1
    endif

  enddo
endif

! ----------------------------------------------------------------------
! distance limit check.
! ----------------------------------------------------------------------
  do j=1,nbeam
  do i=1,nrang
    range = ran(i)
    if( range>limit_range ) then
      index(i,j) = 10
      wrkchek(i,j) =10
    endif
  enddo
  enddo

! ----------------------------------------------------------------------
! tendency mark.
! ----------------------------------------------------------------------

  do j=1,nbeam
  do i=iend+1,nrang
     if ( index(i,j)==0 ) then
          refvel=vadvel(i,j)
          if ( wrk(i,j)*refvel<0.0 ) then
               index(i,j)=10
               wrkchek(i,j) =10
          endif
     endif
  enddo
  enddo

  return
 end subroutine vadtilt_check
