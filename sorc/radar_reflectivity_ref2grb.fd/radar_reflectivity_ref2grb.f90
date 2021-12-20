!################################################################
!     Author: Shun Liu
!     Purpose: read in SRC product using 8 CPUs

!     Record of revisions:
!     Date            Programmer         Descrition of Change
!     ======================================================
!     09/12/2007      Shun Liu           Create

!     01/17/2013      Shun Liu           modify
!     modify for WCOSS I/O

!     11/10/2013      Shun Liu           modify
!     use grib2 format 

!     12/30/2013      Shun Liu           modify
!     add new variable hybrid scan reflectivty and height 
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
        character(200) filenm1,filenm,filetmp
        character(13) mtime

        !* parameter for grib format
        character(len=2):: cyr,cmo,cdy,chr,cmn
        integer:: yr,mo,dy,hr,mn
        integer:: gnx,gny

!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!   START REF2GRB read
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

        if(mpe==0) write(6,*)'start radar_reflectivity_ref2grb.f90',mpe

        open(1,file='tmp')
        read(1,*)yr,mo,dy,hr,mn
        close(1)
!       read(cyr,87) yr
!       read(cmo,87) mo
!       read(cdy,87) dy
!       read(chr,87) hr
!       read(cmn,87) mn
!87      format(I2)
        yr=yr+2000
     
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
100     format(a,i1)
        filenm=trim(filenm1)//'/'//mtime//'.mosaic'
        filenm=trim(filenm)
        write(8,*)trim(filenm)

        this_flsize=flsize(mpe+1)
        open(1,file=trim(filenm),access='stream',status='old')
        read(1) mhead
        close(1)

        nnx=mhead%nx
        nny=mhead%ny
        nnz=mhead%nz

        call mpi_barrier(MPI_COMM_WORLD,ierror)

        allocate(ref(nnz,nny,nnx))
        allocate(CREF(nny,nnx), CREFH(nny,nnx), ETP(nny,nnx), SHI(nny,nnx))
        allocate(POSH(nny,nnx), MEHS(nny,nnx), HSR(nny,nnx), HSRH(nny,nnx))
        allocate(LCREF(nny,nnx), LCREFH(nny,nnx), VIL(nny,nnx), VILD(nny,nnx))
        allocate(PCPFLAG(nny,nnx))

        open(21,file=trim(filenm),access='stream',status='old')
           read(21) mhead, (((ref(k,j,i),i=1,nnx),j=1,nny),k=1,nnz), &
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
          if(mpe==0)write(6,*)'pny=, i=',i, pny(i),pnx(i)
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
          reftmp(1:nny,1:nnx)=ref(k,1:nny,1:nnx)

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
        write(6,*) 'after read data finish'

!write(2) REFall3D(:,:,k)
        array=REFall3D(:,:,k)
        call deal_with_grib(array,gnx,gny,k,yr,mo,dy,hr,mn)

	endif

! SW corner (1,1) is 130W, 20N

  637	format(40(f4.1,1x))

        end do

!!! bonus part to handle composite reflectivity

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

        CREFall=-999.0
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
        call deal_with_grib(array,gnx,gny,99,yr,mo,dy,hr,mn)
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

        CREFall=-999.0
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
	write(6,*) 'maxval ETP: ', maxval(CREFall)
        array=CREFall  !/1000.0
        call deal_with_grib(array,gnx,gny,98,yr,mo,dy,hr,mn)
!* end output echo top

        endif

          if(1==1) then
!* output hybrid scan reflectivity
           reftmp(1:nny,1:nnx)=HSR(1:nny,1:nnx)

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

        CREFall=-999.0
	do J=1,7001,5
	if (J .eq. 1) Jloc=0
	Jloc=Jloc+1
        do I=1,3501,5
        if (I .eq. 1) Iloc=0
	Iloc=Iloc+1
	CREFall(Jloc,Iloc)=REFall(I,J)
	enddo
	enddo

	write(6,*) 'maxval hybird scan reflectivity: ', maxval(CREFall)
        array=CREFall
        call deal_with_grib(array,gnx,gny,97,yr,mo,dy,hr,mn)
        endif
!* end output hybrid scan reflectivity

!* output hybrid scan reflectivity height
           reftmp(1:nny,1:nnx)=HSRH(1:nny,1:nnx)

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

        CREFall=-999.0
	do J=1,7001,5
	if (J .eq. 1) Jloc=0
	Jloc=Jloc+1
        do I=1,3501,5
        if (I .eq. 1) Iloc=0
	Iloc=Iloc+1
	CREFall(Jloc,Iloc)=REFall(I,J)
	enddo
	enddo

	write(6,*) 'maxval hybrid scan height: ', maxval(CREFall)
        array=CREFall
        call deal_with_grib(array,gnx,gny,96,yr,mo,dy,hr,mn)
        endif
!* end output hybrid scan reflectivity height
        end if

        close(1)

       call MPI_FINALIZE(ierror)

	end program read

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

      subroutine deal_with_grib(fld,nx,ny,kz,yr,mo,dy,hr,mn)
!
!-----------------------------------------------------------------
! ABSTRACT: This routine is to write out a new grib2 file
!   Mar 2013     J.Wang   
!   Oct 2013     S.Liu !*  for ref mosaic
!-----------------------------------------------------------------
!
      implicit none
!
      integer, parameter   :: max_bytes=20000000
!
      integer nx,ny,kz 
      integer listsec0(2)
      integer listsec1(13)
      integer igds(5)
      integer igdstmpllen
      integer ipdstmpllen
      integer idrstmpllen
      integer idrsnum,ibmap,numcoord,ipdsnum,idefnum
      
      integer,dimension(100) :: igdstmpl
      integer,dimension(100) :: ipdstmpl
      integer,dimension(100) :: idrstmpl
!
      integer ideflist(1)
      real(4) coordlist(1)
!
      character(255) :: cout1
      character*1 cgrib(max_bytes)
!
      logical*1 bmap(nx,ny)
!
      real(4),dimension(nx*ny) :: fld
      integer ifilw,i,j,lengrib,lonstt,lonlst,latstt,latlst,ierr
      integer :: yr,mo,dy,hr,mn,sc
      character(len=2) :: chr,cmn
      real(4) :: dxval
      integer:: lev_val(31)

        DATA lev_val/500, 750, 1000, 1250, 1500, 1750, 2000, 2250, 2500, &
                     2750, 3000, 3500, 4000, 4500, 5000, 5500, 6000, 6500, &
                     7000, 7500, 8000, 8500, 9000, 10000, 11000, 12000, &
                     13000, 14000, 15000, 16000, 18000/
!
! code start
!-----------------------------------------------------------------
!
!-- set file unit
      ifilw=52
!
!
!-- get file name
!     call getarg(1,cout)
       write(chr,18) hr
!      read(cmn,18) mn
18      format(i2.2)
!     cout1='refd3d.t'//chr//cmn//'zgrb2f00'
      cout1='refd3d.t'//chr//'z.grb2f00'
!     cout1='refd3d.grib2'
!     write(99,*)chr,hr,cout1
!     stop
!
!-----------------------------------------------------------------
!
!-- Open GRIB2 file
      if(kz==1)then
      call baopen(ifilw,trim(cout1),ierr)
      print *,'cout=',trim(cout1),'ierr=',ierr
!
!-- section 0: indicator section 
      listsec0(1)=0         ! Discipline: table 0.0 (0:Meteorological field)
      listsec0(2)=2         ! grib edition number (2:grib2)
!
!-- section 1: identification section
      listsec1(1)=7         ! Identification of orginating center (Table 0)  (7:ncep)
      listsec1(2)=4         ! Identification of orginating subcenter (ON388-Table C) (4:emc)
      listsec1(3)=2         ! GRIB master tables version number (Table 1.0)  (2: Nov 2003)
      listsec1(4)=1         ! Version number of GRIB local tables used to augment Master Tables (Table 1.1)
      listsec1(5)=1         ! Significance of reference time (Table 1.2) (0:ana 1:start of fcst 2:vrfy 3:obs)
      listsec1(6)=yr      ! Reference time - Year (4 digits)
      listsec1(7)=mo        ! Reference time - Month
      listsec1(8)=dy        ! Reference time - Day
      listsec1(9)=hr         ! Reference time - Hour
      listsec1(10)=mn        ! Reference time - Minute
      listsec1(11)=0        ! Reference time - Second
      listsec1(12)=0        ! Production status of data (Table 1.3) (0:opn products 1:opn test products)
      listsec1(13)=1        ! Type of processed data (Table 1.4) (0:ana products 1:fcst products 2:ana & fcst 3: cntl fcst)

       end if

       call gribcreate(cgrib,max_bytes,listsec0,listsec1,ierr)
       write(6,*)'gribcreate status=',ierr
!
!-- section 3: grid definition section
      igds(1)=0             ! Source of grid definition (Table 3.0) (0:specified in the code)
      igds(2)=nx*ny         ! Number of grid points in the defined grid
      igds(3)=0             ! Number of octets for optional list of numbers defining number of points 
      igds(4)=0             ! Interpretation of list of numbers defining number of points 
!-- example: Gaussian Lat/lon
      igds(5)=0            ! Grid definition template number (Table 3.1) (0:Lat/lon, 30:Lambert 40:Gaussian)
      igdstmpl=0
      if( igds(5)==0) then
        igdstmpllen=19
!
!-- set up grid definition template 3.40
        igdstmpl=0
        igdstmpl(1)=6       ! Shape of the Earth (Table 3.2) (6:Shape of the Earth = 6,371,229.0 m)
        igdstmpl(8)=nx      ! Ni . number of points along a paralell 
        igdstmpl(9)=ny      ! Nj . number of points along a meridian 
        igdstmpl(10)=0      ! Basic angle of the initial production domain 
        igdstmpl(11)=0      ! Subdivisions of basic angle used to define extreme longitudes and latitudes, and direction increments 
        latstt=20000000
        lonstt=230000000
        latlst=55000000
        lonlst=300000000
        dxval=1875000
        igdstmpl(12)=latstt ! La1 - latitude of first grid point
        igdstmpl(13)=lonstt ! Lo1 - longitude of first grid point 
        igdstmpl(14)=48     ! Resolution and component flags (Table 3.3, bits order reversed)
        igdstmpl(15)=latlst ! La2 - latitude of last grid point
        igdstmpl(16)=lonlst ! Lo2 - longitude of last grid point 
        igdstmpl(17)=50000  ! Di - i direction increment
        igdstmpl(18)=50000   ! N - number of paralells between a pole and the equator
        igdstmpl(19)=64      ! Scanning mode (Table 3.4) (+i,+j,i consecutive,row scan same direction)
      endif 
!
      idefnum=1             ! Used if igds(3) .ne. 0. The number of entries in array ideflist
      ideflist=0            ! Used if igds(3) .ne. 0. number of grid points contained in each row ( or column ), Dummy array otherwise
      call addgrid(cgrib,max_bytes,igds,igdstmpl,igdstmpllen,ideflist,idefnum,ierr)
      print*,'addgrid status=',ierr
!
!-- section 4: product definition section
      ipdstmpl=0
      ipdsnum=0             ! Product Definition Template Number (Table 4.0) (0: Analysis or forecast at a horizontal level or in a horizontal layer at a point in time) 
      ipdstmpllen=15        ! pdt template length
      ipdstmpl(1)=16         ! catogory
      ipdstmpl(3)=2         ! Type of generating process (Table 4.3) (0:ana, 1:ic, 2:fcst)
      ipdstmpl(4)=0         ! Background generating process identifier 
      ipdstmpl(5)=99        ! Analysis or forecast generating process identified (ON388TableA) 
      ipdstmpl(6)=0         ! Hours of observational data cutoff after reference time
      ipdstmpl(7)=0         ! Minutes of observational data cutoff after reference time
      ipdstmpl(8)=1         ! Indicator of unit of time range (Table 4.4) (0:minute, 1:hour 2:day)
      ipdstmpl(9)=0         ! Forecast time in units defined by ipdstmpl(8)
      ipdstmpl(11)=0        ! Scale factor of first fixed surface
      if(kz < 40)then
      ipdstmpl(2)=195         ! parameter
      ipdstmpl(10)=102      ! Type of first fixed surface (see Code table 4.5) (100:isobaric level)
      ipdstmpl(12)=lev_val(kz)    ! Scaled value of first fixed surface
      else if(kz==99)then
      ipdstmpl(2)=196         ! parameter
      ipdstmpl(10)=200      ! Type of first fixed surface (see Code table 4.5) (100:isobaric level)
      ipdstmpl(12)=0
      else if(kz==98)then
      ipdstmpl(2)=197         ! parameter
      ipdstmpl(10)=200      ! Type of first fixed surface (see Code table 4.5) (100:isobaric level)
      ipdstmpl(12)=0
      else if(kz==97)then
!     ipdstmpl(1)=16         ! catogory
!     ipdstmpl(2)=192       ! hybrid scan:: HSR (http://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_table4-2-0-15.shtml)
      ipdstmpl(1)=15         ! catogory
      ipdstmpl(2)=15       ! hybrid scan:: HSR (http://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_table4-2-0-15.shtml)
      ipdstmpl(10)=200      ! Type of first fixed surface (see Code table 4.2.0-16) (100:isobaric level)
      ipdstmpl(12)=0
      else if(kz==96)then
!     ipdstmpl(1)=16         ! catogory
!     ipdstmpl(2)=193       ! hybrid scan:: HSRHT (http://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_table4-2-0-15.shtml)
      ipdstmpl(1)=15         ! catogory
      ipdstmpl(2)=16       ! hybrid scan:: HSRHT (http://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_table4-2-0-15.shtml)
      ipdstmpl(10)=200      ! Type of first fixed surface (see Code table 4.2.0-16) (100:isobaric level)
      ipdstmpl(12)=0
      end if
      ipdstmpl(13)=255      ! Type of first second surface (see Code table 4.5) (100:isobaric level)
      ipdstmpl(14)=0        ! Scale factor of second fixed surface
      ipdstmpl(15)=0        ! Scaled value of second fixed surface
!
      numcoord=0            ! Number of coordinate values after template 
      coordlist=0.          ! Optional list of coordinate values
!
!-- section 5: Data Representation Section
!     if(kz==98) then  
!     idrstmpl=0
!     idrsnum=0   
!     idrstmpllen=18        ! Length of Data representation section
!     else
      idrstmpl=0
      idrsnum=3             ! Data representation section template number (Table 5.0) (3:Grid Point Data - Complex Packing and Spatial Differencing)

      idrstmpllen=18        ! Length of Data representation section
      idrstmpl(2)=2         ! Binary scale factor
      idrstmpl(3)=5         ! Decimal scale factor
!     idrstmpl(4)=28        ! Decimal scale factor
      idrstmpl(4)=2        ! Decimal scale factor
      idrstmpl(5)=0        ! Decimal scale factor
      idrstmpl(6)=1        ! Decimal scale factor
      idrstmpl(7)=0         ! Missing value management used (see Code Table 5.5)
      idrstmpl(8)=0         ! Primary missing value substitute
      idrstmpl(9)=0         ! Secondary missing value substitute 
!     idrstmpl(10)=6656     ! Secondary missing value substitute 
      idrstmpl(11)=0         ! Secondary missing value substitute 
      idrstmpl(12)=5        ! Order of spatial difference (see Code Table 5.6) 
      idrstmpl(13)=1        ! Order of spatial difference (see Code Table 5.6) 
      idrstmpl(14)=1        ! Order of spatial difference (see Code Table 5.6) 
      idrstmpl(15)=2048        ! Order of spatial difference (see Code Table 5.6) 
      idrstmpl(16)=11        ! Order of spatial difference (see Code Table 5.6) 
      idrstmpl(17)=1        ! Order of spatial difference (see Code Table 5.6) 
      idrstmpl(18)=4        ! Order of spatial difference (see Code Table 5.6) 

!     end if
 
!-- section 6:       
      ibmap=255             ! Bit-map indicator (Table 6.0) (0:A bit map applies, 255:A bit map doesn't apply)
!
      call addfield(cgrib,max_bytes,ipdsnum,ipdstmpl,ipdstmpllen, &
                          coordlist,numcoord,idrsnum,idrstmpl, &
                          idrstmpllen,fld,nx*ny,ibmap,bmap,ierr)
      print*,'addfield status=',ierr

!-- finalize  GRIB message after all section
!-- adds the End Section ( "7777" )

      call gribend(cgrib,max_bytes,lengrib,ierr)
      print*,'gribend status=',ierr
      print*,'length of the final GRIB2 message in octets =',lengrib
!
      call wryte(ifilw, lengrib, cgrib)
!
      print*,'after wrt cgrib2, lengrib=',lengrib
!
      end
