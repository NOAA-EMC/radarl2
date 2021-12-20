! S. Guan  share data module =====Start
!* date: 07/23/2008

!#ifdef UNDERSCORE
!   #define SRC_IO SRC_IO_ 
!#endif

!module radar_data
!use sdata1
!implicit none
!save
!logical:: dual_index
!type(ccout_strct_ncep) :: strct_refvel(6,18)
!end module radar_data
!* S. Guan  share data module =====End

subroutine get_radar_data(temp1, temp2, temp3, temp4, temp5, temp6, num_elevation, dualindex, sailsindex)
use sdata1
use radar_data

integer ::  num_elevation, dualindex, sailsindex
type(ccout_strct_ncep) :: temp1, temp2, temp3, temp4, temp5, temp6
! Save radar data
    strct_refvel(1,num_elevation+1) = temp1
    strct_refvel(2,num_elevation+1) = temp2
    strct_refvel(3,num_elevation+1) = temp3
    strct_refvel(4,num_elevation+1) = temp4   !* rho    cc
    strct_refvel(5,num_elevation+1) = temp5   !* phi
    strct_refvel(6,num_elevation+1) = temp6   !* zdr
    dual_index=.false.
    if(dualindex==1) dual_index=.true.
    write(*,*)'sails::',sailsindex

end subroutine get_radar_data

subroutine build10_recomb(num_elevation,lunout,srcgrid_nx,srcgrid_ny,volumeid,radar_len,tablepath,fixlut)
!
! Codes are from tranrad_lev2_sub and modified by S. Guan
!
use sdata1
use radar_data
use CONFIG_PARS
implicit none
integer ::  num_elevation

integer:: nb,srcgrid_nx,srcgrid_ny, radar_len
integer ::  lunout, lundx
real(8) ::  volumeid
character(8) :: radar_name
character(len=*) :: tablepath
character(len=*) :: fixlut
!
real,allocatable::reflectivity(:,:,:),dvelocity(:,:,:),spectralwidth(:,:,:)
real,allocatable::refl_gate_dis(:,:,:),dvel_gate_dis(:,:,:),spec_gate_dis(:,:,:)
real,allocatable::elevation_angle_refl(:,:),elevation_angle_dvel(:,:),elevation_angle_spec(:,:)
real,allocatable::azimuth_angle_refl(:,:),azimuth_angle_dvel(:,:),azimuth_angle_spec(:,:)
real,allocatable::flag_qcrf_refl(:,:),flag_qcrw_dvel(:,:)
real,allocatable::vcp_dvel(:,:),nyq_dvel(:,:)

real,allocatable::year_refl(:,:),year_dvel(:,:),year_spec(:,:)
real,allocatable::month_refl(:,:),month_dvel(:,:),month_spec(:,:)
real,allocatable::day_refl(:,:),day_dvel(:,:),day_spec(:,:)
real,allocatable::hour_refl(:,:),hour_dvel(:,:),hour_spec(:,:)
real,allocatable::minute_refl(:,:),minute_dvel(:,:),minute_spec(:,:)
real,allocatable::second_refl(:,:),second_dvel(:,:),second_spec(:,:)
!
integer(4) nazm_ref,nazm_vel,nazm_spe,numrefgate,numvelgate,numspegate
integer(4) ngate_dis_indv,ngate_dis_inds
integer(4) nazm_ref_max,nazm_vel_max,nazm_spe_max,numrefgate_max,numvelgate_max,numspegate_max
integer,dimension(:,:),allocatable:: markflag
character (len=8) :: radar_id
integer numbeam(6)

!* for connecting QC package======Start
type(ccout_strct_ncep_tmp) :: strct_tmp
type(ccout_strct_ncep_tmp),allocatable :: strct_ref_tmp(:),strct_vel_tmp(:),strct_sw_tmp(:)
type(ccout_strct_ncep_tmp),allocatable :: strct_cc_tmp(:),strct_kdp_tmp(:),strct_zdr_tmp(:)
integer :: length_tmp,numid
integer :: nelv_ref,nelv_vel,nelv_sw
!!! shun liu for connecting QC package======End

!character(1),allocatable::buf(:,:)
!character(len=256) tablepath
character(len=256) tfix
character(1) header(24)
integer(2) jday,kday
integer(4) jtime,kday4,ktime,lrec
real(4) rtime
character(1) cbuf2(2),ckbuf2(2),cbuf4(4)
integer(1) ibuf2(2),kbuf2(2)
integer(1) ibuf4(4)
equivalence (ibuf2(1),cbuf2(1))
equivalence (kbuf2(1),ckbuf2(1))
equivalence (ibuf4(1),cbuf4(1))
equivalence (jday,ibuf2(1))
equivalence (kday,kbuf2(1))
equivalence (jtime,ibuf4(1))
real(4) timemax,timemin,timethis
integer(4) juldate,juldatemax,juldatemin
integer(4) radial_num,radial_status
integer(4) iday,imonth,iyear,ihour,iminute,isecond
real (4)                     rhour,rminute,rsecond
real(4) azimuth,elev_angle,vadd,vmult,previous_elev_angle
!
integer(4) numref,numvel,irefptr,ivelptr
integer(4) numspe,ispeptr                                               !kumar
integer(4) nbytes,nwords
!character(120) radar_name
real(8) radar_lat,radar_lon,radar_height
character(400) filename
character(8) stid 
character(1) underscore,dot
real(4) slat,slon,selv,sanht
integer(4) iost1,iost2
                                                                    !!!!!kumar 
character(80) infile,config_file,cvar 
!namelist/namlev2/nbytes
                                                                    !!!!!kumar
character(400) fileout,l2_out
integer(4) nrecs
integer(4) ifield5
character(400) field
integer(4) icountr,icountv
integer(4) icounts                                                     !kumar
integer(4),allocatable::istartr(:),iendr(:)
integer(4),allocatable::istartv(:),iendv(:)
integer(4),allocatable::istarts(:),iends(:)                            !kumar
real(4),allocatable::sumr(:),angler(:),anglermax(:),anglermin(:)
real(4),allocatable::sumv(:),anglev(:),anglevmax(:),anglevmin(:)
real(4),allocatable::sums(:),angles(:),anglesmax(:),anglesmin(:)       !kumar
real(4),allocatable::anglev0(:)
integer(4) nbytes_vol,j,k,msgtype,nrecs_actual,irecl,i,nbytes_in,ibl,j_of_i
character(1) nors,eorw
integer(4) iradstat,icountrv,icountrvmax,idegrees,iminutes,iseconds,iazm
real(8) station_elev
integer(4) radar_elev_above_station
integer(4) length,ioutrecl
real(4) vel_max,vel_min,ref_max,ref_min
real(4) spe_max,spe_min                                                 !kumar
real(4) ref_gate_min,ref_gate_max,vel_gate_min,vel_gate_max,spe_gate_min,spe_gate_max
real(4) azimtemp
character(10) cdate
character(3) plus_minutes,minus_minutes
character(8) rid
integer(4) yy4,mm2,dd2,hh2,min2,ss2, na
integer(8) nmin8,nmin_begin8,nmin_end8,min_m8,min_p8
integer(4) idate(5)
integer(4) num_beam_max
integer :: ii, ivar, na_ref, ng_ref, SRCrun
integer,dimension(:),allocatable:: refspval
common/radard/stid,radar_lat,radar_lon,radar_height,station_elev,radar_elev_above_station

real(8) xmem,get_memory
real(8) rtc,t1,t2
real(8) start_t,end_t
!                                                                     !!!!!kumar

!!! X.Y.X opens array
integer flag_nssl_cplusplus 
type(ccout_strct_nssl),allocatable :: strct_nssl_tmp(:)
type(ccout_strct_nssl),allocatable :: strct_nssl_ref(:),strct_nssl_vel(:),strct_nssl_sw(:)
INTEGER(4)  NPPI                                         

type (CCOUT_CONFIG_PARS)          pp
type (ccout_strct_src_header)     src_data_header
real, allocatable ::              src_data(:,:,:)

data lundx/40/ 

    write(*,*) "num_elevation:",num_elevation,'dual variable:',dual_index
    icountr = num_elevation
    icountrv = num_elevation
    radar_name = strct_refvel(1,1)%radar_name(1:radar_len)
    write(*,*)'fixlut:',radar_len,radar_name, len(trim(radar_name))

    do k = 1, num_elevation
      do ivar=1,6
      strct_refvel(ivar,k)%radar_name = radar_name
      end do
      do i=1, nazim1
         do ivar=1,6
            strct_refvel(ivar,k)%radlat(i)=radar_lat
            strct_refvel(ivar,k)%radlon(i)=radar_lon
            strct_refvel(ivar,k)%radhgt(i)=radar_height
            end do
      end do 
    end do

!!!!shun liu insert or output data here=====Start
!
      nelv_ref=icountrv;nelv_vel=icountrv;nelv_sw=icountrv
      allocate(strct_ref_tmp(nelv_ref),strct_vel_tmp(nelv_vel),strct_sw_tmp(nelv_sw))
      allocate(strct_cc_tmp(nelv_ref),strct_kdp_tmp(nelv_vel),strct_zdr_tmp(nelv_sw))
      allocate(markflag(nazim1,nelv_vel))
!
!* markflag=0 is 'reject data'
!* markflag=1 is 'accept data'
!
      do i=1,nelv_ref
      call zero_tmp(strct_tmp)
      call copy_strct_in(strct_tmp,strct_refvel(1,i))
      strct_ref_tmp(i)=strct_tmp
!              write(*,*) "ref::",i,strct_refvel(1,i)%elev_angle(1),strct_refvel(1,i)%radhgt(1)
      end do

      do i=1,nelv_vel
      call zero_tmp(strct_tmp)
      call copy_strct_in(strct_tmp,strct_refvel(2,i))
      strct_vel_tmp(i)=strct_tmp
!              write(*,*) "vel::",strct_tmp%elev_angle,strct_tmp%radhgt 
      end do

      do i=1,nelv_sw
      call zero_tmp(strct_tmp)
      call copy_strct_in(strct_tmp,strct_refvel(3,i))
      strct_sw_tmp(i)=strct_tmp
!              write(*,*) "swg::",strct_tmp%elev_angle,strct_tmp%radhgt 
      end do

      do i=1,nelv_ref
      call zero_tmp(strct_tmp)
      if(maxval(strct_refvel(4,i)%num_beam)==0)then
      dual_index=.false.
      end if
      if(dual_index) then
      call copy_strct_in(strct_tmp,strct_refvel(4,i))
      strct_cc_tmp(i)=strct_tmp
!     write(*,*) "cc::",i,strct_refvel(4,i)%elev_angle(1),strct_refvel(4,i)%radhgt(1)
      else
      strct_cc_tmp(i)=strct_tmp
      write(*,*) "cc::",i, "no dual-pol observation"
      end if
      end do

      do i=1,nelv_ref
      call zero_tmp(strct_tmp)
      if(dual_index) then
      call copy_strct_in(strct_tmp,strct_refvel(5,i))
      strct_kdp_tmp(i)=strct_tmp
!     write(*,*) "kdp::",i,strct_refvel(5,i)%elev_angle(1),strct_refvel(5,i)%radhgt(1)
      else
      strct_kdp_tmp(i)=strct_tmp
      write(*,*) "kdp::",i,"no dual-pol observation"
      end if
      end do

      do i=1,nelv_ref
      call zero_tmp(strct_tmp)
      if(dual_index) then
      call copy_strct_in(strct_tmp,strct_refvel(6,i))
      strct_zdr_tmp(i)=strct_tmp
!     write(*,*) "zdr::",i,strct_refvel(6,i)%elev_angle(1),strct_refvel(6,i)%radhgt(1)
      else
      strct_zdr_tmp(i)=strct_tmp
      write(*,*) "zdr::",i,"no dual_pol observation"
      end if
      end do

!** raw data check
        write(*,*)'check raw data: number of beam'
        write(*,*)'elv     ref       cc        zdr       kdp       vel       sw'
      do i=1,nelv_ref 
        numbeam(1)=strct_ref_tmp(i)%num_beam
        numbeam(2)=strct_cc_tmp(i)%num_beam
        numbeam(3)=strct_zdr_tmp(i)%num_beam
        numbeam(4)=strct_kdp_tmp(i)%num_beam
        numbeam(5)=strct_vel_tmp(i)%num_beam
        numbeam(6)=strct_sw_tmp(i)%num_beam
        write(*,'(i2,6i10)')i,(numbeam(j),j=1,6)
      end do

      if(dual_index) then
      do i=1,nelv_ref 
        numbeam(1)=strct_ref_tmp(i)%num_beam
        numbeam(2)=strct_cc_tmp(i)%num_beam
        numbeam(3)=strct_zdr_tmp(i)%num_beam
        numbeam(4)=strct_kdp_tmp(i)%num_beam
        numbeam(5)=strct_vel_tmp(i)%num_beam
        numbeam(6)=strct_sw_tmp(i)%num_beam
        do j=1,6
          if(numbeam(j)<340.or.numbeam(j)>380) then
             open(88,file='qcinfor')
             write(88,*)'STOP:: radar data s incomplete at level::',i  
             close(88)
             stop
          end if
        end do

        if(abs(numbeam(1)-numbeam(2))>5) then
             open(88,file='qcinfor')
             write(*,*)'STOP:: reflectivity do not match dual-pol vars at level::',i 
             close(88)
             stop
        end if

        if(abs(numbeam(2)-numbeam(3))>1 .or.           &
           abs(numbeam(2)-numbeam(4))>1 .or.           & 
           abs(numbeam(3)-numbeam(4))>1             ) then
             open(88,file='qcinfor')
             write(*,*)'STOP:: dual-pol vars do not match each other at level::',i 
             close(88)
             stop
        end if
      end do
      end if

!** end raw data check

!     print*,'sliu print before Vr QC:'
!     call output_for_NetCDF_convert(nelv_ref,strct_ref_tmp,strct_vel_tmp)  
!     print*,'sliu end print before Vr QC:'

!     print*,'sliu begin Vr QC:'
      t1=0
      write(*,*) 'this rtc::',t1
      t1 = rtc()
!     xmem=get_memory()
!     write(*,*) 'this mem1::',xmem
!     xmem=get_memory()
!     write(*,*) 'this mem2::',xmem
      rid=strct_refvel(1,icountrv)%radar_name 
      yy4=strct_refvel(1,1)%year(1)
      mm2=strct_refvel(1,1)%month(1)
      dd2=volumeid
      write(infile,221)trim(rid),yy4,mm2,dd2
221   format(a,'_',i4.4,i2.2,i6.6)
      write(*,*)'infile is::', infile
      write(*,*)'volume ID is::', volumeid

      if(1==1)then
      do i=1,1
!     do i=1,nelv_vel
         ii=int(strct_ref_tmp(i)%elev_angle*100.0)
         cvar='ref'
         call dumpvar(i,ii,infile,nelv_vel,strct_ref_tmp,cvar)
         cvar='vel'
         call dumpvar(i,ii,infile,nelv_vel,strct_vel_tmp,cvar)
         if(dual_index)then
         cvar='cc'
         call dumpvar(i,ii,infile,nelv_vel,strct_cc_tmp,cvar)
         cvar='zdr'
         call dumpvar(i,ii,infile,nelv_vel,strct_zdr_tmp,cvar)
         cvar='kdp'
         call dumpvar(i,ii,infile,nelv_vel,strct_kdp_tmp,cvar)
         end if
      end do
      end if
!     do i=1,nelv_ref
!        ii=int(strct_ref_tmp(i)%elev_angle*100.0)
!        call dumpref(i,ii,infile,nelv_ref,strct_ref_tmp)
!     end do

      call qcmain(nelv_ref,nelv_vel,nelv_sw,strct_ref_tmp,strct_vel_tmp,strct_sw_tmp, &
                  strct_cc_tmp,strct_zdr_tmp,strct_kdp_tmp,infile,markflag,dual_index)
     t2 = rtc()
     write(*,500)t2-t1,strct_refvel(2,1)%vcpnum(10)
500   format('cpu time after qcmain ',f15.4,'  vcp',i3)
     print*,'sliu End QC:'

     print*,'start REF QC'
     t1 = rtc()

      call nssl_qcmain(nelv_ref,strct_ref_tmp,strct_vel_tmp)         !!!! Call pengfei QC 

      if(1==1)then
      do i=1,1
!     do i=1,nelv_vel
         ii=int(strct_ref_tmp(i)%elev_angle*100.0)
         cvar='ref_af'
         call dumpvar(i,ii,infile,nelv_vel,strct_ref_tmp,cvar)
         cvar='vel_af'
         call dumpvar(i,ii,infile,nelv_vel,strct_vel_tmp,cvar)
         if(dual_index)then
         cvar='cc_af'
         call dumpvar(i,ii,infile,nelv_vel,strct_cc_tmp,cvar)
         cvar='zdr_af'
         call dumpvar(i,ii,infile,nelv_vel,strct_zdr_tmp,cvar)
         cvar='kdp_af'
         call dumpvar(i,ii,infile,nelv_vel,strct_kdp_tmp,cvar)
         end if
      end do
      end if

      do i=1,icountr
      call copy_strct_out(strct_ref_tmp(i),strct_refvel(1,i))
      call copy_strct_out(strct_vel_tmp(i),strct_refvel(2,i))
      call copy_strct_out(strct_sw_tmp(i),strct_refvel(3,i))
      end do

      deallocate(strct_ref_tmp,strct_vel_tmp,strct_sw_tmp)   !  X.Y.X adds

!!!!shun liu insert or output data here=====End

! NSSL C++ QC BEGIN
     flag_nssl_cplusplus = 0

     IF(flag_nssl_cplusplus .ne. 0 ) THEN   

     allocate(strct_nssl_tmp(1),strct_nssl_ref(icountrv),strct_nssl_vel(icountrv),strct_nssl_sw(icountrv))
    
     if(1==1)then
     do i=1,icountrv
     call zero_nssl_tmp(strct_nssl_tmp(1))
     call copy_strct_in_nssl(strct_refvel(1,i),strct_nssl_tmp(1))
     strct_nssl_ref(i)=strct_nssl_tmp(1)

     call zero_nssl_tmp(strct_nssl_tmp(1))
     call copy_strct_in_nssl(strct_refvel(2,i),strct_nssl_tmp(1))
     strct_nssl_vel(i)=strct_nssl_tmp(1)

     call zero_nssl_tmp(strct_nssl_tmp(1))
     call copy_strct_in_nssl(strct_refvel(3,i),strct_nssl_tmp(1))
     strct_nssl_sw(i)=strct_nssl_tmp(1)
     end do
     end if

      config_file="nexrad_level2.config"

      NPPI=icountrv 
     write(0,*)trim(radar_name),'  ',trim(infile),' ',trim(config_file)

     
      print*,'Before NSSL',NPPI

    print*,'After NSSL'

     do i=1,icountrv
     call copy_strct_out_nssl(strct_nssl_ref(i),strct_refvel(1,i))
     call copy_strct_out_nssl(strct_nssl_vel(i),strct_refvel(2,i))
     call copy_strct_out_nssl(strct_nssl_sw(i),strct_refvel(3,i))
     end do


    deallocate(strct_nssl_tmp,strct_nssl_ref,strct_nssl_vel,strct_nssl_sw)

  ENDIF      ! for if flag_nssl_cplusplus = 1
 
!!! NSSL C++ QC END
     t2 = rtc()
     write(*,600)t2-t1
600   format('cpu time after refQC ',f15.4)
!    print*,'END REF QC'

!!!! Start to call SRC

    print*, 'Start SRC'

     t1 = rtc()
      allocate(strct_ref_tmp(icountrv),refspval(icountrv))
      
      refspval=0
      do k=1,icountrv
        call zero_tmp(strct_tmp)
        call copy_strct_in(strct_tmp,strct_refvel(1,k))
        strct_ref_tmp(k)=strct_tmp

        na_ref=strct_tmp%num_beam
        ng_ref=strct_tmp%num_gate
        do j=1,na_ref
        do i=1,ng_ref
          if(abs(strct_tmp%field(i,j))<80.0) refspval(k)=1
        end do
        end do
      enddo

     SRCrun=sum(refspval)

     IF(SRCrun==0) write(*,*)'Warning: no qualified reflectivity for SRC file' 
!    IF(SRCrun>0)then
     print*,'srcgrid_nx= ',srcgrid_nx,srcgrid_ny
     print*,'fixlut = ',trim(fixlut)
     call get_config(pp,fixlut)

     pp.nx=srcgrid_nx
     pp.ny=srcgrid_ny
     print*,'pp.nx,pp.ny = ',pp.nx,pp.ny
     print*,'fixlut = ',fixlut

    allocate(src_data(pp.nz,pp.ny,pp.nx))

    call   SRC_IO(strct_ref_tmp, icountrv, pp, &
                  pp.nx, pp.ny, pp.nz, src_data, src_data_header) 

!* output SRC data
!    srcfilenm1=srcfilenm(1:20)//'.src'
!    print*,'srcfilenm(1:20)',srcfilenm(1:20)

                 rid=strct_refvel(1,icountrv)%radar_name 
                 yy4=strct_refvel(1,1)%year(1)
                 mm2=strct_refvel(1,1)%month(1)
                 dd2=strct_refvel(1,1)%day(1)
                 hh2=strct_refvel(1,1)%hour(1)
                 min2=strct_refvel(1,1)%minute(1)
                 ss2=strct_refvel(1,1)%second(1)
                 write(srcfilenm1,222)trim(rid),yy4,mm2,dd2, &
                                      hh2,min2,ss2 
 222            format(a,'_',i4.4,i2.2,i2.2,'_',i2.2,i2.2,i2.2,'.','src')
                 print*,'rid,yy,mm,dd,hh,min2,ss ',rid,yy4,mm2,dd2,hh2,min2,ss2
    open(10, file=trim(srcfilenm1), form='unformatted')
    write(10)src_data_header
    write(10)src_data
    close(10)

!    if(1==0)then
!    srcfilenm1=srcfilenm(1:20)//'.txt'
!    open(10, file=srcfilenm1)
!    do k=1,pp.nz
!    do j=1,pp.ny
!    do i=1,pp.nx
!    write(10,*)src_data(k,j,i)
!    end do
!    end do
!    end do
!    close(10)
!    end if

!    print*,'start test SRC output'
!    if(1==0)then
!    do i=1,601
!    do j=1,601
!      if(abs(src_data(2,i,j))<50.0) print*,'SRC output:', i,j, src_data(2,i,j)
!    end do
!    end do
!    end if
!    print*,'end test SRC output'

    print*,'SRC output test, pp.nx', pp.nx, pp.ny, pp.nz
    print*,'SRC output test, src_data_header nx', src_data_header%nx
    print*,'SRC output test, src_data_header ny', src_data_header%ny
    print*,'SRC output test, src_data_header nz', src_data_header%nz
    print*,'SRC output test, src_data_header nz', src_data_header%nw_lat
    print*,'SRC output test, src_data_header nz', src_data_header%nw_lon

!    open(11,file='src.grd',form='unformatted')
!    where(abs(src_data)>90.0) src_data=-999.0
!    do k=1,pp.nz
!      write(11)src_data(k,:,:)
!    end do
!    close(11)
     deallocate(src_data)
!   END IF 
    deallocate(strct_ref_tmp,refspval)

     t2 = rtc()
     write(*,700)t2-t1
700   format('cpu time after SRC ',f15.4)
    print*, 'End SRC'
 
!!!! End call SRC
!
! radar_id for bufrout
!
             radar_id=strct_refvel(1,icountrv)%radar_name
!
            print '("radar_id = ",a8)',radar_id
!
             nazm_ref_max=maxval((strct_refvel(1,1)%num_beam),nazm_ref)
             nazm_vel_max=maxval((strct_refvel(2,1)%num_beam),nazm_vel)
             nazm_spe_max=maxval((strct_refvel(3,1)%num_beam),nazm_spe)
             numrefgate_max=maxval((strct_refvel(1,1)%num_gate),numrefgate)
             numvelgate_max=maxval((strct_refvel(2,1)%num_gate),numvelgate)
             numspegate_max=maxval((strct_refvel(3,1)%num_gate),numspegate)
          do i=1,icountrv
             nazm_ref=maxval((strct_refvel(1,i)%num_beam),nazm_ref)
             nazm_vel=maxval((strct_refvel(2,i)%num_beam),nazm_vel)
             nazm_spe=maxval((strct_refvel(3,i)%num_beam),nazm_spe)
             numrefgate=maxval((strct_refvel(1,i)%num_gate),numrefgate)
             numvelgate=maxval((strct_refvel(2,i)%num_gate),numvelgate)
             numspegate=maxval((strct_refvel(3,i)%num_gate),numspegate)
!
             if(nazm_ref.gt.nazm_ref_max)then
               nazm_ref_max=nazm_ref
             endif
!
             if(nazm_vel.gt.nazm_vel_max)then
               nazm_vel_max=nazm_vel
             endif
!
             if(nazm_spe.gt.nazm_spe_max)then
               nazm_spe_max=nazm_spe
             endif
!
             if(numrefgate.gt.numrefgate_max)then
               numrefgate_max=numrefgate
             endif
!
             if(numvelgate.gt.numvelgate_max)then
               numvelgate_max=numvelgate
             endif
!
             if(numspegate.gt.numspegate_max)then
               numspegate_max=numspegate
             endif
!             print*,'nazm_ref,nazm_vel,nazm_spe,numrefgate,numvelgate,numspegate  ',  &
!             nazm_ref,nazm_vel,nazm_spe,numrefgate,numvelgate,numspegate
          enddo
         print*,'nazm_ref_max,nazm_vel_max,nazm_spe_max,numrefgate_max,numvelgate_max,numspegate_max  ',  &
         nazm_ref_max,nazm_vel_max,nazm_spe_max,numrefgate_max,numvelgate_max,numspegate_max
!
allocate(reflectivity(icountrv,nazm_ref_max,numrefgate_max))
allocate(dvelocity(icountrv,nazm_vel_max,numvelgate_max))
allocate(spectralwidth(icountrv,nazm_vel_max,numspegate_max))
!
allocate(refl_gate_dis(icountrv,nazm_ref_max,numrefgate_max))
allocate(dvel_gate_dis(icountrv,nazm_vel_max,numvelgate_max))
allocate(spec_gate_dis(icountrv,nazm_vel_max,numspegate_max))
!
allocate(elevation_angle_refl(icountrv,nazm_ref_max))
allocate(elevation_angle_dvel(icountrv,nazm_vel_max))
allocate(elevation_angle_spec(icountrv,nazm_spe_max))
!
allocate(azimuth_angle_refl(icountrv,nazm_ref_max))
allocate(azimuth_angle_dvel(icountrv,nazm_vel_max))
allocate(vcp_dvel(icountrv,nazm_vel_max))
allocate(nyq_dvel(icountrv,nazm_vel_max))
allocate(azimuth_angle_spec(icountrv,nazm_spe_max))
!
allocate(flag_qcrf_refl(icountrv,nazm_ref_max))   
allocate(flag_qcrw_dvel(icountrv,nazm_vel_max))
!
allocate(year_refl(icountrv,nazm_ref_max))
allocate(year_dvel(icountrv,nazm_vel_max))
allocate(year_spec(icountrv,nazm_spe_max))
!
allocate(month_refl(icountrv,nazm_ref_max))
allocate(month_dvel(icountrv,nazm_vel_max))
allocate(month_spec(icountrv,nazm_spe_max))
!
allocate(day_refl(icountrv,nazm_ref_max))
allocate(day_dvel(icountrv,nazm_vel_max))
allocate(day_spec(icountrv,nazm_spe_max))
!
allocate(hour_refl(icountrv,nazm_ref_max))
allocate(hour_dvel(icountrv,nazm_vel_max))
allocate(hour_spec(icountrv,nazm_spe_max))
!
allocate(minute_refl(icountrv,nazm_ref_max))
allocate(minute_dvel(icountrv,nazm_vel_max))
allocate(minute_spec(icountrv,nazm_spe_max))
!
allocate(second_refl(icountrv,nazm_ref_max))
allocate(second_dvel(icountrv,nazm_vel_max))
allocate(second_spec(icountrv,nazm_spe_max))
!!!
          do i=1,icountrv
             do j=1,nazm_ref_max
                do k=1,numrefgate_max
                   reflectivity(i,j,k)=strct_refvel(1,i)%field(k,j)
                   refl_gate_dis(i,j,k)=strct_refvel(1,i)%gate_dis(k,j)
                enddo
                azimuth_angle_refl(i,j)=strct_refvel(1,i)%azim(j) 
                elevation_angle_refl(i,j)=strct_refvel(1,i)%elev_angle(j)
!
! store the year,month,day,hour,minute,second for all the radial lines for reflectiviy
!
                 year_refl(i,j)=strct_refvel(1,i)%year(j)
                 month_refl(i,j)=strct_refvel(1,i)%month(j)
                 day_refl(i,j)=strct_refvel(1,i)%day(j)
                 hour_refl(i,j)=strct_refvel(1,i)%hour(j)
                 minute_refl(i,j)=strct_refvel(1,i)%minute(j)
                 second_refl(i,j)=strct_refvel(1,i)%second(j)
             enddo
!
             do j=1,nazm_vel_max
                do k=1,numvelgate_max
                   dvelocity(i,j,k)=strct_refvel(2,i)%field(k,j)
                   dvel_gate_dis(i,j,k)=strct_refvel(2,i)%gate_dis(k,j)
                enddo
                azimuth_angle_dvel(i,j)=strct_refvel(2,i)%azim(j) 
                vcp_dvel(i,j)=strct_refvel(2,i)%vcpnum(j)
                nyq_dvel(i,j)=strct_refvel(2,i)%nyq_vel(j)
                elevation_angle_dvel(i,j)=strct_refvel(2,i)%elev_angle(j)
!
! assign the flag_qcrw_dvel values from Shun's QC package (markflag array)
! markflag = 0 (reject data) and markflag = 1 (accept data)
!
                flag_qcrw_dvel(i,j)=float(markflag(j,i))


!                print*,'flag_qcrw_dvel  ',flag_qcrw_dvel(i,j)
!
! store the year,month,day,hour,minute,second for all the radial lines for doppler velocity 
!
                 year_dvel(i,j)=strct_refvel(2,i)%year(j)
                 month_dvel(i,j)=strct_refvel(2,i)%month(j)
                 day_dvel(i,j)=strct_refvel(2,i)%day(j)
                 hour_dvel(i,j)=strct_refvel(2,i)%hour(j)
                 minute_dvel(i,j)=strct_refvel(2,i)%minute(j)
                 second_dvel(i,j)=strct_refvel(2,i)%second(j)
             enddo
!
             do j=1,nazm_spe_max
                do k=1,numspegate_max
                   spectralwidth(i,j,k)=strct_refvel(3,i)%field(k,j)
                   spec_gate_dis(i,j,k)=strct_refvel(3,i)%gate_dis(k,j)
                enddo
                azimuth_angle_spec(i,j)=strct_refvel(3,i)%azim(j) 
                elevation_angle_spec(i,j)=strct_refvel(3,i)%elev_angle(j)
!
! store the year,month,day,hour,minute,second for all the radial lines for spectral width 
!
                 year_spec(i,j)=strct_refvel(3,i)%year(j)
                 month_spec(i,j)=strct_refvel(3,i)%month(j)
                 day_spec(i,j)=strct_refvel(3,i)%day(j)
                 hour_spec(i,j)=strct_refvel(3,i)%hour(j)
                 minute_spec(i,j)=strct_refvel(3,i)%minute(j)
                 second_spec(i,j)=strct_refvel(3,i)%second(j)
             enddo
!
          enddo
!
          tfix = tablepath(1:len(tablepath)) // '/bufrtab.006'
!         tfix = '/nco/sib/wx12gs/temp/v_final/bufrtab.006'
          OPEN (unit=lundx, file=tfix)

         t1 = rtc()
          call bufrout(lunout,lundx,radar_id,volumeid,nyq_dvel,vcp_dvel,radar_lat,radar_lon, &
               radar_elev_above_station,station_elev, &
               year_refl,year_dvel,year_spec, &
               month_refl,month_dvel,month_spec, &
               day_refl,day_dvel,day_spec, &
               hour_refl,hour_dvel,hour_spec, &
               minute_refl,minute_dvel,minute_spec, &
               second_refl,second_dvel,second_spec, &
               icountrv,nazm_ref_max,nazm_vel_max,nazm_spe_max, &
               numrefgate_max,numvelgate_max,numspegate_max, & 
               refl_gate_dis,reflectivity,dvel_gate_dis,dvelocity,spec_gate_dis,spectralwidth, &
               azimuth_angle_refl,azimuth_angle_dvel,azimuth_angle_spec, &
               elevation_angle_refl,elevation_angle_dvel,elevation_angle_spec,flag_qcrw_dvel)
          t2 = rtc()
          print*,'cpu time after bufrout  ',t2-t1

!!!!!!if(flag_nssl_cplusplus .ne. 0 ) deallocate(strct_nssl_tmp,strct_nssl_ref,strct_nssl_vel,strct_nssl_sw)
deallocate(markflag)                                                     !X.Y.X adds

deallocate(reflectivity)
deallocate(dvelocity)
deallocate(spectralwidth)
deallocate(refl_gate_dis)
deallocate(dvel_gate_dis)
deallocate(spec_gate_dis)
deallocate(elevation_angle_refl)
deallocate(elevation_angle_dvel)
deallocate(elevation_angle_spec)
deallocate(azimuth_angle_refl)
deallocate(azimuth_angle_dvel)
deallocate(azimuth_angle_spec)
deallocate(flag_qcrw_dvel)
deallocate(year_refl)
deallocate(year_dvel)
deallocate(year_spec)
deallocate(month_refl)
deallocate(month_dvel)
deallocate(month_spec)
deallocate(day_refl)
deallocate(day_dvel)
deallocate(day_spec)
deallocate(hour_refl)
deallocate(hour_dvel)
deallocate(hour_spec)
deallocate(minute_refl)
deallocate(minute_dvel)
deallocate(minute_spec)
deallocate(second_refl)
deallocate(second_dvel)
deallocate(second_spec)

!        print '(" ref max,min=",2f10.1)',ref_max,ref_min
!        print '(" vel max,min=",2f10.1)',vel_max,vel_min
!        print '(" spe max,min=",2f10.1)',spe_max,spe_min
!        num_beam_max=maxval(strct_refvel(1,1)%num_beam)
!        print '(" reflectivity num_beam_max=",i10)',num_beam_max
!         do i=1,num_beam_max,200
!          print '(" i,strct_ref(1)%vcpnum(i)=",i4,i10)',i,strct_refvel(1,1)%vcpnum(i)
!          print '(" i,strct_ref(1)%year(i)=",i4,f15.5)',i,strct_refvel(1,1)%year(i)
!          print '(" i,strct_ref(1)%month(i)=",i4,f15.5)',i,strct_refvel(1,1)%month(i)
!          print '(" i,strct_ref(1)%day(i)=",i4,f15.5)',i,strct_refvel(1,1)%day(i)
!          print '(" i,strct_ref(1)%hour(i)=",i4,f15.5)',i,strct_refvel(1,1)%hour(i)
!          print '(" i,strct_ref(1)%minute(i)=",i4,f15.5)',i,strct_refvel(1,1)%minute(i)
!          print '(" i,strct_ref(1)%second(i)=",i4,f15.5)',i,strct_refvel(1,1)%second(i)
!          print '(" i,strct_ref(1)%radlat(i)=",i4,f10.3)',i,strct_refvel(1,1)%radlat(i)
!          print '(" i,strct_ref(1)%radlon(i)=",i4,f10.3)',i,strct_refvel(1,1)%radlon(i)
!          print '(" i,strct_ref(1)%radhgt(i)=",i4,f10.3)',i,strct_refvel(1,1)%radhgt(i)
!          print '(" i,strct_ref(1)%elev_angle(i)=",i4,f10.3)',i,strct_refvel(1,1)%elev_angle(i)
!          print '(" i,strct_ref(1)%fstgatdis(i)=",i4,f10.3)',i,strct_refvel(1,1)%fstgatdis(i)
!          print '(" i,strct_ref(1)%nyq_vel(i)=",i4,f10.3)',i,strct_refvel(1,1)%nyq_vel(i)
!          print '(" i,strct_ref(1)%num_beam(i)=",i4,i10)',i,strct_refvel(1,1)%num_beam(i)
!          print '(" i,strct_ref(1)%num_gate(i)=",i4,i10)',i,strct_refvel(1,1)%num_gate(i)
!          print '(" i,strct_ref(1)%gateWidth(i)=",i4,f10.3)',i,strct_refvel(1,1)%gateWidth(i)
!          print '(" i,strct_ref(1)%azim(i)=",i4,f10.3)',i,strct_refvel(1,1)%azim(i)
!          do j=1,strct_refvel(1,1)%num_gate(i),30
!           print '(1h ,15f7.2)',(strct_refvel(1,1)%field(k,i),k=j,min(j+14,strct_refvel(1,1)%num_gate(i)))
!          end do
!         end do
 
!         num_beam_max=maxval(strct_refvel(2,1)%num_beam)
!         print '(" velocity num_beam_max=",i10)',num_beam_max
   
!         do i=1,num_beam_max,200
!          print '(" i,strct_vel(1)%vcpnum(i)=",i4,i10)',i,strct_refvel(2,1)%vcpnum(i)
!          print '(" i,strct_vel(1)%year(i)=",i4,f15.5)',i,strct_refvel(2,1)%year(i)
!          print '(" i,strct_vel(1)%month(i)=",i4,f15.5)',i,strct_refvel(2,1)%month(i)
!          print '(" i,strct_vel(1)%day(i)=",i4,f15.5)',i,strct_refvel(2,1)%day(i)
!          print '(" i,strct_vel(1)%hour(i)=",i4,f15.5)',i,strct_refvel(2,1)%hour(i)
!          print '(" i,strct_vel(1)%minute(i)=",i4,f15.5)',i,strct_refvel(2,1)%minute(i)
!          print '(" i,strct_vel(1)%second(i)=",i4,f15.5)',i,strct_refvel(2,1)%second(i)
!          print '(" i,strct_vel(1)%radlat(i)=",i4,f10.3)',i,strct_refvel(2,1)%radlat(i)
!          print '(" i,strct_vel(1)%radlon(i)=",i4,f10.3)',i,strct_refvel(2,1)%radlon(i)
!          print '(" i,strct_vel(1)%radhgt(i)=",i4,f10.3)',i,strct_refvel(2,1)%radhgt(i)
!          print '(" i,strct_vel(1)%elev_angle(i)=",i4,f10.3)',i,strct_refvel(2,1)%elev_angle(i)
!          print '(" i,strct_vel(1)%fstgatdis(i)=",i4,f10.3)',i,strct_refvel(2,1)%fstgatdis(i)
!          print '(" i,strct_vel(1)%nyq_vel(i)=",i4,f10.3)',i,strct_refvel(2,1)%nyq_vel(i)
!          print '(" i,strct_vel(1)%num_beam(i)=",i4,i10)',i,strct_refvel(2,1)%num_beam(i)
!          print '(" i,strct_vel(1)%num_gate(i)=",i4,i10)',i,strct_refvel(2,1)%num_gate(i)
!          print '(" i,strct_vel(1)%gateWidth(i)=",i4,f10.3)',i,strct_refvel(2,1)%gateWidth(i)
!          print '(" i,strct_vel(1)%azim(i)=",i4,f10.3)',i,strct_refvel(2,1)%azim(i)
!          do j=1,strct_refvel(2,1)%num_gate(i),30
!           print '(1h ,15f7.2)',(strct_refvel(2,1)%field(k,i),k=j,min(j+14,strct_refvel(2,1)%num_gate(i)))
!          end do
!         end do
   
!         num_beam_max=maxval(strct_refvel(3,1)%num_beam)
!         print '(" spectral width num_beam_max=",i10)',num_beam_max
!         do i=1,num_beam_max,200
!          print '(" i,strct_spe(1)%vcpnum(i)=",i4,i10)',i,strct_refvel(3,1)%vcpnum(i)
!          print '(" i,strct_spe(1)%year(i)=",i4,f15.5)',i,strct_refvel(3,1)%year(i)
!          print '(" i,strct_spe(1)%month(i)=",i4,f15.5)',i,strct_refvel(3,1)%month(i)
!          print '(" i,strct_spe(1)%day(i)=",i4,f15.5)',i,strct_refvel(3,1)%day(i)
!          print '(" i,strct_spe(1)%hour(i)=",i4,f15.5)',i,strct_refvel(3,1)%hour(i)
!          print '(" i,strct_spe(1)%minute(i)=",i4,f15.5)',i,strct_refvel(3,1)%minute(i)
!          print '(" i,strct_spe(1)%second(i)=",i4,f15.5)',i,strct_refvel(3,1)%second(i)
!          print '(" i,strct_spe(1)%radlat(i)=",i4,f10.3)',i,strct_refvel(3,1)%radlat(i)
!          print '(" i,strct_spe(1)%radlon(i)=",i4,f10.3)',i,strct_refvel(3,1)%radlon(i)
!          print '(" i,strct_spe(1)%radhgt(i)=",i4,f10.3)',i,strct_refvel(3,1)%radhgt(i)
!          print '(" i,strct_spe(1)%elev_angle(i)=",i4,f10.3)',i,strct_refvel(3,1)%elev_angle(i)
!          print '(" i,strct_spe(1)%fstgatdis(i)=",i4,f10.3)',i,strct_refvel(3,1)%fstgatdis(i)
!          print '(" i,strct_spe(1)%nyq_vel(i)=",i4,f10.3)',i,strct_refvel(3,1)%nyq_vel(i)
!          print '(" i,strct_spe(1)%num_beam(i)=",i4,i10)',i,strct_refvel(3,1)%num_beam(i)
!          print '(" i,strct_spe(1)%num_gate(i)=",i4,i10)',i,strct_refvel(3,1)%num_gate(i)
!          print '(" i,strct_spe(1)%gateWidth(i)=",i4,f10.3)',i,strct_refvel(3,1)%gateWidth(i)
!          print '(" i,strct_spe(1)%azim(i)=",i4,f10.3)',i,strct_refvel(3,1)%azim(i)
!          do j=1,strct_refvel(3,1)%num_gate(i),30
!           print '(1h ,15f7.2)',(strct_refvel(3,1)%field(k,i),k=j,min(j+14,strct_refvel(3,1)%num_gate(i)))
!          end do
!         end do

 inquire(iolength=length) strct_refvel(1,1)
!print *,' length of structure ccout_strct_ncep = ',length
!print *,' number of reflectivity/wind scan pairs processed = ',icountrv
 ioutrecl=length*3*icountrv
!print *,' full size of output file = ',ioutrecl
 goto 2234                           ! X.Y.X 
   do j=1,icountrv
    do i=1,3
     write(0,'(" i,j,icountrvmax,min,max vcpnum=",3i4,2i12)')i,j,icountrvmax, &
                     minval(strct_refvel(i,j)%vcpnum), &
                     maxval(strct_refvel(i,j)%vcpnum)
     write(0,'(" min,max year=",2f15.5)') &
                     minval(strct_refvel(i,j)%year), &
                     maxval(strct_refvel(i,j)%year)
     write(0,'(" min,max month=",2f15.5)') &
                     minval(strct_refvel(i,j)%month), &
                     maxval(strct_refvel(i,j)%month)
     write(0,'(" min,max day=",2f15.5)') &
                     minval(strct_refvel(i,j)%day), &
                     maxval(strct_refvel(i,j)%day)
     write(0,'(" min,max hour=",2f15.5)') &
                     minval(strct_refvel(i,j)%hour), &
                     maxval(strct_refvel(i,j)%hour)
     write(0,'(" min,max hour=",2f15.5)') &
                     minval(strct_refvel(i,j)%minute), &
                     maxval(strct_refvel(i,j)%minute)
     write(0,'(" min,max hour=",2f15.5)') &
                     minval(strct_refvel(i,j)%second), &
                     maxval(strct_refvel(i,j)%second)
     write(0,'(" min,max num_beam=",2i15)') &
                     minval(strct_refvel(i,j)%num_beam), &
                     maxval(strct_refvel(i,j)%num_beam)
     write(0,'(" min,max num_gate=",2i15)') &
                     minval(strct_refvel(i,j)%num_gate), &
                     maxval(strct_refvel(i,j)%num_gate)
    end do
   end do

2234 CONTINUE 

! deallocate(istartr,iendr,istartv,iendv,sumr,angler,anglermax,anglermin)
! deallocate(istarts,iends,sums,angles,anglesmax,anglesmin)              ! kumar
! deallocate(sumv,anglev,anglevmax,anglevmin,anglev0)
!#############end do bigloop
go to 9030
9000 continue
!  print *,' reached EOF on file "file_info"'
   go to 9030
9020 continue
!  print *,' encountered read error on file "file_info"'
9025 continue
!  print *,' could not obtain radar lat, lon, elevation--name in table'
9026 continue
!  print *,'End of search for that station - Go to the next station '
9030 continue
!  print *,' processed all available level 2 88D radar data'
!close(10)
!end_t = rtc()
 
!print*,'total time used in QC is', end_t-start_t

   
end subroutine build10_recomb

