!################################################################
!     Author: Shun Liu
!     Purpose: read in SRC product using 8 CPUs

!     Record of revisions:
!     Date            Programmer         Descrition of Change
!     ======================================================
!     09/12/2007      Shun Liu           Create

!################################################################

      Program read
        use mpi
        implicit none

        type mosaic_head
            integer(4):: nx
            integer(4):: ny
            integer(4):: nz
            real(4):: dx
            real(4):: dy
            real(4):: ctrl_lat, ctrl_lon
            real(4):: zp(31)
            real(4):: missing_value
            real(4):: no_radar_cover
        end type mosaic_head

        type(mosaic_head):: mhead
        real(4), allocatable :: ref(:,:,:)
        integer, allocatable :: band_sum(:)
        integer(4) i,j,k,nnx,nny,nnz, Iloc,Jloc
        integer(4) pnx(8),pny(8)
        integer(4) nbytes_in

        real(4), allocatable :: CREF(:,:), CREFH(:,:), ETP(:,:), SHI(:,:)
        real(4), allocatable :: POSH(:,:), MEHS(:,:), HSR(:,:), HSRH(:,:)
        real(4), allocatable :: LCREF(:,:), LCREFH(:,:), VIL(:,:), VILD(:,:)
        integer(2), allocatable :: PCPFLAG(:,:)
        real(4), allocatable :: REFall(:,:),array(:,:),CREFall(:,:), REFall3d(:,:,:)
        integer(4) :: flsize(8), this_flsize
        integer(4),dimension(8):: nx_domain, ny_domain
        integer(4),dimension(8):: nx_domain1, ny_domain1
        integer(4) :: nx1,nx2,ny1,ny2,kk, nnx1,nny1
        real(4), allocatable :: ref1(:,:),ref2(:,:),ref3(:,:),ref4(:,:)
        real(4), allocatable :: ref5(:,:),ref6(:,:),ref7(:,:),ref8(:,:)
        real(4), allocatable :: reftmp(:,:),ref_tmp6(:,:)

        !* parameter for MPI
        integer :: mpe, npe, ierror
        real :: isend(2001),irecv(14002)
        integer :: ircnt(0:7),idisp(0:7),iscnt

        !* parameter for data
        integer :: aaa, bbb
        character(100) filenm1,filenm,filetmp
        character(13) mtime

        !* parameter for grib format
        character(len=2):: yr, mo,dy,hr
        integer:: gnx,gny
        

        read(5,*)yr,mo,dy,hr

        call MPI_INIT(ierror)
        call MPI_COMM_SIZE(MPI_COMM_WORLD,npe,ierror)
        call MPI_COMM_RANK(MPI_COMM_WORLD,mpe,ierror)

        flsize(1)=522609334; flsize(2)=522609334
        flsize(3)=261435334; flsize(4)=522609334
        flsize(5)=696696334; flsize(6)=696696334
        flsize(7)=348522334; flsize(8)=696696334
          
        open(1,file='card')
        read(1,*)mtime
        read(1,'(a100)')filetmp
        close(1)
        write(8,*)filetmp
        write(filenm1,100)trim(filetmp),mpe+1
!100     format('/ptmp/wx22hl/REFprod/tile',i1)
100     format(a,i1)
        filenm=trim(filenm1)//'/'//mtime//'.mosaic'
        filenm=trim(filenm)
        write(8,*)trim(filenm)

        this_flsize=flsize(mpe+1)
        open(1,file=trim(filenm),access='direct', recl=this_flsize,&
                 action='read',status='old')
        read(1,rec=1,num=nbytes_in) mhead
        close(1)

        nnx=mhead%nx
        nny=mhead%ny
        nnz=mhead%nz

        call mpi_barrier(MPI_COMM_WORLD,ierror)


!        print*,nnz,nny,nnx

        allocate(ref(nnz,nny,nnx))
        allocate(CREF(nny,nnx), CREFH(nny,nnx), ETP(nny,nnx), SHI(nny,nnx))
        allocate(POSH(nny,nnx), MEHS(nny,nnx), HSR(nny,nnx), HSRH(nny,nnx))
        allocate(LCREF(nny,nnx), LCREFH(nny,nnx), VIL(nny,nnx), VILD(nny,nnx))
        allocate(PCPFLAG(nny,nnx))

        open(21,file=filenm,access='direct', recl=this_flsize ,&
                   action='read',status='old')
           read(21,rec=1,num=nbytes_in) mhead, (((ref(k,j,i),i=1,nnx),j=1,nny),k=1,nnz), &
                                                 ((CREF(j,i),i=1,nnx),j=1,nny)         , &
                                                 ((CREFH(j,i),i=1,nnx),j=1,nny)        , &
                                                 ((ETP(j,i),i=1,nnx),j=1,nny)          , &
                                                 ((SHI(j,i),i=1,nnx),j=1,nny)          , &
                                                 ((POSH(j,i),i=1,nnx),j=1,nny)         , &
                                                 ((MEHS(j,i),i=1,nnx),j=1,nny)         , &
                                                 ((HSR(j,i),i=1,nnx),j=1,nny)          , &
                                                 ((HSRH(j,i),i=1,nnx),j=1,nny)         , &
                                                 ((LCREF(j,i),i=1,nnx),j=1,nny)        , &
                                                 ((LCREFH(j,i),i=1,nnx),j=1,nny)       , &
                                                 ((PCPFLAG(j,i),i=1,nnx),j=1,nny)      , &
                                                 ((VIL(j,i),i=1,nnx),j=1,nny)          , &
                                                 ((VILD(j,i),i=1,nnx),j=1,nny)

        close(21)

        call mpi_gather(nny,1,mpi_integer, &
                        pny,1,mpi_integer, 0, mpi_comm_world,ierror)
        call mpi_gather(nnx,1,mpi_integer, &
                        pnx,1,mpi_integer, 0, mpi_comm_world,ierror)
        call mpi_bcast(pny,8,mpi_integer,0, mpi_comm_world,ierror)
        call mpi_bcast(pnx,8,mpi_integer,0, mpi_comm_world,ierror)

        call mpi_barrier(MPI_COMM_WORLD,ierror)
        do i=1,npe
!         if(mpe==0)write(6,*)'pny=, i=',i, pny(i),pnx(i)
        end do

        nx_domain(5)=1; nx_domain1(5)=2001; ny_domain(5)=1; ny_domain1(5)=2001
        nx_domain(6)=1; nx_domain1(6)=2001; ny_domain(6)=2001; ny_domain1(6)=4001
        nx_domain(7)=1; nx_domain1(7)=2001; ny_domain(7)=4001; ny_domain1(7)=5001
        nx_domain(8)=1; nx_domain1(8)=2001; ny_domain(8)=5001; ny_domain1(8)=7001
        nx_domain(1)=2001; nx_domain1(1)=3501; ny_domain(1)=1; ny_domain1(1)=2001
        nx_domain(2)=2001; nx_domain1(2)=3501; ny_domain(2)=2001; ny_domain1(2)=4001
        nx_domain(3)=2001; nx_domain1(3)=3501; ny_domain(3)=4001; ny_domain1(3)=5001
        nx_domain(4)=2001; nx_domain1(4)=3501; ny_domain(4)=5001; ny_domain1(4)=7001

        allocate(ref1(2001,2001),ref2(2001,2001),ref3(2001,2001),ref4(2001,2001))
        allocate(ref5(2001,2001),ref6(2001,2001),ref7(2001,2001),ref8(2001,2001))
        allocate(reftmp(2001,2001))
        allocate(ref_tmp6(701,1401))
        allocate(REFall(1:3501,1:7001))
!mp
        allocate(CREFall(1401,701))
        allocate(REFall3d(1401,701,nnz))
        allocate(array(1401,701))
        gnx=1401; gny=701
!mp

        open(1,file='ref.grd',form='unformatted')

         do k=1,nnz
!        do k=1,1
          reftmp(1:nny,1:nnx)=ref(k,1:nny,1:nnx)
!           reftmp(1:nny,1:nnx)=cref(1:nny,1:nnx)

           if(mpe==0)ref1(1:nny,1:nnx)=reftmp(1:nny,1:nnx)
           if(mpe==1)ref2(1:nny,1:nnx)=reftmp(1:nny,1:nnx)
           if(mpe==2)ref3(1:nny,1:nnx)=reftmp(1:nny,1:nnx)
           if(mpe==3)ref4(1:nny,1:nnx)=reftmp(1:nny,1:nnx)
           if(mpe==4)ref5(1:nny,1:nnx)=reftmp(1:nny,1:nnx)
           if(mpe==5)ref6(1:nny,1:nnx)=reftmp(1:nny,1:nnx)
           if(mpe==6)ref7(1:nny,1:nnx)=reftmp(1:nny,1:nnx)
           if(mpe==7)ref8(1:nny,1:nnx)=reftmp(1:nny,1:nnx)

           kk=2001*2001
           call mpi_bcast(ref1,kk,mpi_real,0, mpi_comm_world,ierror)
           call mpi_bcast(ref2,kk,mpi_real,1, mpi_comm_world,ierror)
           call mpi_bcast(ref3,kk,mpi_real,2, mpi_comm_world,ierror)
           call mpi_bcast(ref4,kk,mpi_real,3, mpi_comm_world,ierror)
           call mpi_bcast(ref5,kk,mpi_real,4, mpi_comm_world,ierror)
           call mpi_bcast(ref6,kk,mpi_real,5, mpi_comm_world,ierror)
           call mpi_bcast(ref7,kk,mpi_real,6, mpi_comm_world,ierror)
           call mpi_bcast(ref8,kk,mpi_real,7, mpi_comm_world,ierror)

           do kk=1,npe
              nx1=nx_domain(kk); nx2=nx_domain1(kk); ny1=ny_domain(kk); ny2=ny_domain1(kk); 
              if(kk==1)REFall(nx1:nx2,ny1:ny2)=ref1(1:pny(kk),1:pnx(kk))
              if(kk==2)REFall(nx1:nx2,ny1:ny2)=ref2(1:pny(kk),1:pnx(kk))
              if(kk==3)REFall(nx1:nx2,ny1:ny2)=ref3(1:pny(kk),1:pnx(kk))
              if(kk==4)REFall(nx1:nx2,ny1:ny2)=ref4(1:pny(kk),1:pnx(kk))
              if(kk==5)REFall(nx1:nx2,ny1:ny2)=ref5(1:pny(kk),1:pnx(kk))
              if(kk==6)REFall(nx1:nx2,ny1:ny2)=ref6(1:pny(kk),1:pnx(kk))
              if(kk==7)REFall(nx1:nx2,ny1:ny2)=ref7(1:pny(kk),1:pnx(kk))
              if(kk==8)REFall(nx1:nx2,ny1:ny2)=ref8(1:pny(kk),1:pnx(kk))
           end do

           call mpi_barrier(MPI_COMM_WORLD,ierror)
           ref_tmp6=refall(1:3501:5,1:7001:5)


	if (mpe == 0) then

	do J=1,7001,5
	if (J .eq. 1) Jloc=0
	Jloc=Jloc+1
        do I=1,3501,5
        if (I .eq. 1) Iloc=0
	Iloc=Iloc+1
	REFall3D(Jloc,Iloc,k)=REFall(I,J)
	enddo
	enddo

!write(2) REFall3D(:,:,k)
        array=REFall3D(:,:,k)
        call deal_with_grib(array,gnx,gny,k,yr,mo,dy,hr)

	endif

! SW corner (1,1) is 130W, 20N

  637	format(40(f4.1,1x))

        end do

!!! bonus part to handle composite reflectivity

!           reftmp(1:nny,1:nnx)=ref(k,1:nny,1:nnx)
           reftmp(1:nny,1:nnx)=cref(1:nny,1:nnx)

           if(mpe==0)ref1(1:nny,1:nnx)=reftmp(1:nny,1:nnx)
           if(mpe==1)ref2(1:nny,1:nnx)=reftmp(1:nny,1:nnx)
           if(mpe==2)ref3(1:nny,1:nnx)=reftmp(1:nny,1:nnx)
           if(mpe==3)ref4(1:nny,1:nnx)=reftmp(1:nny,1:nnx)
           if(mpe==4)ref5(1:nny,1:nnx)=reftmp(1:nny,1:nnx)
           if(mpe==5)ref6(1:nny,1:nnx)=reftmp(1:nny,1:nnx)
           if(mpe==6)ref7(1:nny,1:nnx)=reftmp(1:nny,1:nnx)
           if(mpe==7)ref8(1:nny,1:nnx)=reftmp(1:nny,1:nnx)

           kk=2001*2001
           call mpi_bcast(ref1,kk,mpi_real,0, mpi_comm_world,ierror)
           call mpi_bcast(ref2,kk,mpi_real,1, mpi_comm_world,ierror)
           call mpi_bcast(ref3,kk,mpi_real,2, mpi_comm_world,ierror)
           call mpi_bcast(ref4,kk,mpi_real,3, mpi_comm_world,ierror)
           call mpi_bcast(ref5,kk,mpi_real,4, mpi_comm_world,ierror)
           call mpi_bcast(ref6,kk,mpi_real,5, mpi_comm_world,ierror)
           call mpi_bcast(ref7,kk,mpi_real,6, mpi_comm_world,ierror)
           call mpi_bcast(ref8,kk,mpi_real,7, mpi_comm_world,ierror)

           do kk=1,npe
              nx1=nx_domain(kk); nx2=nx_domain1(kk); ny1=ny_domain(kk); ny2=ny_domain1(kk); 
              if(kk==1)REFall(nx1:nx2,ny1:ny2)=ref1(1:pny(kk),1:pnx(kk))
              if(kk==2)REFall(nx1:nx2,ny1:ny2)=ref2(1:pny(kk),1:pnx(kk))
              if(kk==3)REFall(nx1:nx2,ny1:ny2)=ref3(1:pny(kk),1:pnx(kk))
              if(kk==4)REFall(nx1:nx2,ny1:ny2)=ref4(1:pny(kk),1:pnx(kk))
              if(kk==5)REFall(nx1:nx2,ny1:ny2)=ref5(1:pny(kk),1:pnx(kk))
              if(kk==6)REFall(nx1:nx2,ny1:ny2)=ref6(1:pny(kk),1:pnx(kk))
              if(kk==7)REFall(nx1:nx2,ny1:ny2)=ref7(1:pny(kk),1:pnx(kk))
              if(kk==8)REFall(nx1:nx2,ny1:ny2)=ref8(1:pny(kk),1:pnx(kk))
           end do

           call mpi_barrier(MPI_COMM_WORLD,ierror)

	if (mpe == 0) then

        REFall3D(Jloc,Iloc,1)=-1.

	do J=1,7001,5
	if (J .eq. 1) Jloc=0
	Jloc=Jloc+1
        do I=1,3501,5
        if (I .eq. 1) Iloc=0
	Iloc=Iloc+1
	CREFall(Jloc,Iloc)=REFall(I,J)
	enddo
	enddo

!write(2) CREFall(:,:)
        array=CREFall
        call deal_with_grib(array,gnx,gny,99,yr,mo,dy,hr)
        where(abs(CREFall)<80.0)CREFall=10**(CREFall/10.0)
!write(2) CREFall(:,:)
!       array=CREFall
!       call deal_with_grib(array,gnx,gny,98,yr,mo,dy,hr)
!write(0,*) 'maxval CREFall: ', maxval(CREFall)
        endif


!* output echo top
           reftmp(1:nny,1:nnx)=ETP(1:nny,1:nnx)

           if(mpe==0)ref1(1:nny,1:nnx)=reftmp(1:nny,1:nnx)
           if(mpe==1)ref2(1:nny,1:nnx)=reftmp(1:nny,1:nnx)
           if(mpe==2)ref3(1:nny,1:nnx)=reftmp(1:nny,1:nnx)
           if(mpe==3)ref4(1:nny,1:nnx)=reftmp(1:nny,1:nnx)
           if(mpe==4)ref5(1:nny,1:nnx)=reftmp(1:nny,1:nnx)
           if(mpe==5)ref6(1:nny,1:nnx)=reftmp(1:nny,1:nnx)
           if(mpe==6)ref7(1:nny,1:nnx)=reftmp(1:nny,1:nnx)
           if(mpe==7)ref8(1:nny,1:nnx)=reftmp(1:nny,1:nnx)

           kk=2001*2001
           call mpi_bcast(ref1,kk,mpi_real,0, mpi_comm_world,ierror)
           call mpi_bcast(ref2,kk,mpi_real,1, mpi_comm_world,ierror)
           call mpi_bcast(ref3,kk,mpi_real,2, mpi_comm_world,ierror)
           call mpi_bcast(ref4,kk,mpi_real,3, mpi_comm_world,ierror)
           call mpi_bcast(ref5,kk,mpi_real,4, mpi_comm_world,ierror)
           call mpi_bcast(ref6,kk,mpi_real,5, mpi_comm_world,ierror)
           call mpi_bcast(ref7,kk,mpi_real,6, mpi_comm_world,ierror)
           call mpi_bcast(ref8,kk,mpi_real,7, mpi_comm_world,ierror)

           do kk=1,npe
              nx1=nx_domain(kk); nx2=nx_domain1(kk); ny1=ny_domain(kk); ny2=ny_domain1(kk); 
              if(kk==1)REFall(nx1:nx2,ny1:ny2)=ref1(1:pny(kk),1:pnx(kk))
              if(kk==2)REFall(nx1:nx2,ny1:ny2)=ref2(1:pny(kk),1:pnx(kk))
              if(kk==3)REFall(nx1:nx2,ny1:ny2)=ref3(1:pny(kk),1:pnx(kk))
              if(kk==4)REFall(nx1:nx2,ny1:ny2)=ref4(1:pny(kk),1:pnx(kk))
              if(kk==5)REFall(nx1:nx2,ny1:ny2)=ref5(1:pny(kk),1:pnx(kk))
              if(kk==6)REFall(nx1:nx2,ny1:ny2)=ref6(1:pny(kk),1:pnx(kk))
              if(kk==7)REFall(nx1:nx2,ny1:ny2)=ref7(1:pny(kk),1:pnx(kk))
              if(kk==8)REFall(nx1:nx2,ny1:ny2)=ref8(1:pny(kk),1:pnx(kk))
           end do

           call mpi_barrier(MPI_COMM_WORLD,ierror)

	if (mpe == 0) then

        REFall3D(Jloc,Iloc,1)=-1.

	do J=1,7001,5
	if (J .eq. 1) Jloc=0
	Jloc=Jloc+1
        do I=1,3501,5
        if (I .eq. 1) Iloc=0
	Iloc=Iloc+1
	CREFall(Jloc,Iloc)=REFall(I,J)
	enddo
	enddo

!write(2) CREFall(:,:)
	write(0,*) 'maxval ETP: ', maxval(CREFall)
        array=CREFall
        call deal_with_grib(array,gnx,gny,98,yr,mo,dy,hr)
!* end output echo top

        endif

        close(1)

       call MPI_FINALIZE(ierror)

	end program read

        subroutine deal_with_grib(array,nx,ny,kz,yr,mo,dy,hr)

        real:: array(nx,ny)
        integer:: lev_val(31)
        character(len=2):: yr,mo,dy,hr
        integer:: iyr,imo,idy,ihr

       INTEGER KPDS(200),KGDS(200),JPDS(200),JGDS(200),KF,KNUM, J,IERR, kz
       LOGICAL BITMAP(nx,ny)
       character(len=80):: gribin,gribout

        DATA lev_val/500, 750, 1000, 1250, 1500, 1750, 2000, 2250, 2500, &
                     2750, 3000, 3500, 4000, 4500, 5000, 5500, 6000, 6500, &
                     7000, 7500, 8000, 8500, 9000, 10000, 11000, 12000, &
                     13000, 14000, 15000, 16000, 18000/



        gribout='refd3d.t'//hr//'z.grbf00'

        if (kz .eq. 1) then
       CALL BAOPEN(39,trim(gribout),IERR)
        endif
       KPDS = -1
       KGDS = -1

        KGDS(1)=0 ! latlon
        KGDS(2)=nx
        KGDS(3)=ny
        KGDS(4)=20000
        KGDS(5)=-130000
        KGDS(6)=128 !!?
        KGDS(7)=55000
        KGDS(8)=-60000
        KGDS(9)=50
        KGDS(10)=50
        KGDS(11)=64
        KGDS(12:16)=0

        read(yr,87) iyr
        read(mo,87) imo
        read(dy,87) idy
        read(hr,87) ihr
  87    format(I2)

!       KPDS=-1

        KPDS(1)=7
        KPDS(2)=99   !!! dont have a true generating process number
        KPDS(3)=255
        KPDS(4)=128


        KPDS(8)=iyr
        KPDS(9)=imo
        KPDS(10)=idy
        KPDS(11)=ihr
!!!
        KPDS(12)=0
        KPDS(13)=1  ! time is in hours
        KPDS(14)=0
        KPDS(15)=0
        KPDS(16)=0
        KPDS(17)=0
        KPDS(18)=1  ! GRIB1
        KPDS(19)=129
        KPDS(20)=0
        KPDS(21)=21
        KPDS(22)=5
        KPDS(23)=0

        if (kz < 90) then

        KPDS(5)=211
        KPDS(6)=103
        KPDS(7)=lev_val(kz)
        write(0,*) 'KPDS(7): ',  KPDS(7)
        BITMAP=.true.
        write(0,*) 'min, max array: ', minval(array),maxval(array)
        write(0,*) 'nx*ny: ', nx*ny

        if (kz .eq. 1) then
        do J=1,29
        write(0,*) 'J, PDS, GDS:: ', J, KPDS(J),KGDS(J)
        enddo
        endif

       CALL PUTGB(39,nx*ny,KPDS,KGDS,BITMAP,array,ierr)
        write(0,*) 'ierr from putgb: ', ierr

        else if(kz==99)then

        KPDS(5)=212
        KPDS(6)=200
        KPDS(7)=0

        write(0,*) 'KPDS(7): ',  KPDS(7)
        BITMAP=.true.
        CALL PUTGB(39,nx*ny,KPDS,KGDS,BITMAP,array,ierr)
        write(0,*) 'ierr from putgb(2): ', ierr
        else if(kz==98) then

        KPDS(5)=240
        KPDS(6)=200
        KPDS(7)=0

        write(0,*) 'KPDS(7): ',  KPDS(7)
        BITMAP=.true.
        CALL PUTGB(39,nx*ny,KPDS,KGDS,BITMAP,array,ierr)
        write(0,*) 'ierr from putgb(2): ', ierr



!       call baclose(39,ierr)

        endif


        end subroutine deal_with_grib
