!     ##################################################################
!     ######            SINGLE RADAR REMAP                        ######
!     ##################################################################
!#######################################################################
!  PURPOSE: remap single radar data in polar coordinat system to 
!           cartesion grid coordinat system
!##########################################################################
!   Author: Shunxin Wang (CIMMS/NSSL)
!   April 30, 2005
!
!   Modification History:
!      Sep 10, 2006   Xiaoyong Xu
!      Convert from C++ to Fortran
!##########################################################################

!#include<iostream>
!#include<stdlib.h>
!#include<string>
!#include<vector>
!#include<fstream>
!#include<iterator>
!#include<iomanip>
!#include<time.h>
!#include <map>
!#include <cmath>
!#include <ctime>
!#include <cstdlib>
!#include <cstdio>

!#include "mosaic_adapt_pars.h"
!#include "func_prototype.h"
!#include "grid_vars.h"
!#include "radar_sum.h"
!#include "maplib3d.h"
!#include "NETCDF_HANDLER.h"
!#include "phycst.h"
!#include "setgrd.h"
!#include "nids_output.h"
!#include "BRIGHTBAND.h"
!#include "CONFIG_PARS.h"
!#include "VOLUME_BASE_REFLECT.h"
!#include "RADAR.h"

      SUBROUTINE SRC_IO(qced_ref, actul_tilt, pp,   &
                        nx, ny, nz, src_data, src_data_header )

      use reflectivity
      use BRIGHTBAND
      use RADAR2
      use CONFIG_PARS
      use MAPLIB3D
      use MOSAIC_ADAPT_PARS
      implicit none

      integer                           actul_tilt 
      type (ccout_strct)                qced_ref(actul_tilt)
      type (ccout_QCed_ALIGNed_ref)     qced_aligned_ref(actul_tilt) 
      type (ccout_strct_src)            vbr(actul_tilt)
      type (CCOUT_BRIGHTBAND)           bb_info
      type (CCOUT_RADAR)                radar_info
      type (CCOUT_CONFIG_PARS)          pp
      type (ccout_strct_src_header)     src_data_header
      integer                           nz,ny,nx
      real(4)                           src_data(nz,ny,nx)   
      
      integer i,j,k
      integer NUM_RAYS,ncolumn,nrow  
      real, allocatable:: azimuth_angle(:)
      real, allocatable:: fieldin(:,:), fieldout(:,:)      

      
!!!!!!!   ALIGHMENT  and put them into qced_aligned_ref *****************

       do i= 1, actul_tilt

       NUM_RAYS = 360
       ncolumn  = qced_ref(i).num_beam
       nrow     = qced_ref(i).num_gate

       allocate (azimuth_angle(ncolumn), fieldin(nrow,ncolumn),fieldout(nrow, NUM_RAYS))

       do j= 1, ncolumn
          azimuth_angle(j)=qced_ref(i).azim(j)
       do k= 1, nrow
          fieldin(k,j) = qced_ref(i).field(k,j)
       enddo
       enddo     

       call  alignment( 999.0                              &
                       ,NUM_RAYS, ncolumn,nrow             &
                       ,azimuth_angle                      &
                       ,fieldin, fieldout                  &
                       ,1.0)
          
       qced_aligned_ref(i).radar_name       =  qced_ref(i).radar_name
       qced_aligned_ref(i).vcpnum           =  qced_ref(i).vcpnum
       qced_aligned_ref(i).year             =  qced_ref(i).year
       qced_aligned_ref(i).month            =  qced_ref(i).month
       qced_aligned_ref(i).day              =  qced_ref(i).day
       qced_aligned_ref(i).hour             =  qced_ref(i).hour
       qced_aligned_ref(i).minute           =  qced_ref(i).minute
       qced_aligned_ref(i).second           =  qced_ref(i).second
       qced_aligned_ref(i).radlat           =  qced_ref(i).radlat
       qced_aligned_ref(i).radlon           =  qced_ref(i).radlon  
       qced_aligned_ref(i).radhgt           =  qced_ref(i).radhgt
       qced_aligned_ref(i).elev_angle       =  qced_ref(i).elev_angle
       qced_aligned_ref(i).fstgatdis        =  0.
       qced_aligned_ref(i).num_beam         =  NUM_RAYS      
       qced_aligned_ref(i).num_gate         =  nrow
       qced_aligned_ref(i).gatewidth        =  qced_ref(i).gateWidth(1)
       do j = 1, NUM_RAYS
          qced_aligned_ref(i).azim(j)       =  real(j-1) 
       do k = 1, nrow
          qced_aligned_ref(i).field(k,j)    =  fieldout(k,j)
       enddo
       enddo

!      print*,'radar height:', qced_ref(i).radhgt
      deallocate(azimuth_angle, fieldin,fieldout)

      enddo 

 
     
      do i=1,actul_tilt 
      print*,'SRC',i, qced_aligned_ref(i).year,            &
                      qced_aligned_ref(i).hour,            &
                      qced_aligned_ref(i).radlat,          &
                      qced_aligned_ref(i).elev_angle,      &
                      qced_aligned_ref(i).num_gate,        &
                      qced_aligned_ref(i).gatewidth
      enddo      

!!!!  Read in pp
   
!!!!     call get_config(pp)
    
!!!!  Read in vbr

      do i=1,actul_tilt

        vbr(i).radar_name    = qced_aligned_ref(i).radar_name
        vbr(i).vcpnum        = qced_aligned_ref(i).vcpnum
        vbr(i).year          = qced_aligned_ref(i).year
        vbr(i).month         = qced_aligned_ref(i).month
        vbr(i).day           = qced_aligned_ref(i).day
        vbr(i).hour          = qced_aligned_ref(i).hour
        vbr(i).minute        = qced_aligned_ref(i).minute
        vbr(i).second        = qced_aligned_ref(i).second   
        vbr(i).radlat        = qced_aligned_ref(i).radlat
        vbr(i).radlon        = qced_aligned_ref(i).radlon
        vbr(i).radhgt        = qced_aligned_ref(i).radhgt
        vbr(i).num_beam      = qced_aligned_ref(i).num_beam
        vbr(i).num_gate      = qced_aligned_ref(i).num_gate
        vbr(i).elev          = qced_aligned_ref(i).elev_angle
        vbr(i).fstgatdis     = qced_aligned_ref(i).fstgatdis
        vbr(i).azim_spc      = 1.
        vbr(i).gate_spc      = qced_aligned_ref(i).gatewidth
        vbr(i).scale_value   = pp.bref_scale 
        vbr(i).missing_data  = pp.bref_scale * 999.0
        do j = 1, vbr(i).num_beam
        vbr(i).azim(j)       = qced_aligned_ref(i).azim(j)
        do k = 1, vbr(i).num_gate
        vbr(i).field_data(k,j)= pp.bref_scale * qced_aligned_ref(i).field(k,j)
        enddo
        enddo

!        print*,  vbr(i).scale_value

      enddo

!!!!  Read in bb_info

        call get_brightband(bb_info,pp)
 
!!!! Call SRC
        call src(vbr,bb_info,radar_info,pp,actul_tilt   &
                 , nx, ny, nz, src_data, src_data_header )

      RETURN
      END SUBROUTINE SRC_IO 



      SUBROUTINE src(vbr,bb_info,radar_info,pp,actul_tilt  & 
                 , nx, ny, nz, sum_wref, src_data_header )


      use PHYCST
      use reflectivity
      use BRIGHTBAND
      use RADAR2
      use CONFIG_PARS
      use MAPLIB3D
      use MOSAIC_ADAPT_PARS
      implicit none

      integer                           actul_tilt
      type (ccout_strct_src)            vbr(actul_tilt)
      type (CCOUT_BRIGHTBAND)           bb_info
      type (CCOUT_RADAR)                radar_info
      type (CCOUT_CONFIG_PARS)          pp
      type (ccout_strct_src_header)     src_data_header
      integer                           nz, ny, nx
      real(4)                           sum_wref(nz,ny,nx)
      

      real(8) rtc,cpu_start,cpu_end

      character*20  timeString       ! time string used in product file names
      character*20  nc_timeString    ! time string used in product file names
      
      type (radar)    rd 
      integer(2) ibb_mode
      integer(2) ibb_radar
      integer(2) bb_top_rad
      integer(2) bb_bot_rad

      type (setmapr)  sm
    
      integer(4) i, j, k,i1,j1,n        ! local variables for Cartesian grid indices
      
      real, allocatable :: gv_x(:), gv_y(:), gv_z(:), gv_zp(:)
      real, allocatable :: gv_hterain(:,:), gv_gridlat(:,:) 
      real, allocatable :: gv_gridlon(:,:)

      real(8) dx_km, dy_km
      
      real(4) xrad,yrad   
  
      real(4) d_lat,d_lon,arg

      real(4),allocatable :: sum_wgt(:,:,:)
      integer(2),allocatable :: grid_init(:,:,:)
     
      real(4) ref_mm6                !reflectivity factor in unit of mm6/m3  
      integer(2) iray,jgate,ktilt,thisktilt    !polar grid indices
      integer(4) nray_lut,ngate_lut  !lookup table info
      integer(8) ngrid_tilt          !total # of grid cells influenced by a tilt
      integer(4) nn
      integer(4) min_ngate,iflag_SRC
      integer istatus
      
      integer(4) radar_ctrx,radar_ctry,ratiox,ratioy,i0,j0,iu,jv
      real(4)    time_diff_x,time_diff_y
      real(4),allocatable :: u_Array(:,:)
      real(4),allocatable :: v_Array(:,:)
 
      integer flag_read_RUCWindField

      integer(4) numrows,numcols      

      type (tilt_data_header)  t_hdr
      integer(4),allocatable :: refl_polar(:)
      integer(4)    ref_miss_flag

      integer(4) vcp
      character(200) mosaiclut
      character(200) mosaiclut1       !kumar
      character(200) dirfix           !kumar
      character(4) radar_dummy

      integer  iflag_looktable

      type (tilt_moslut_header) lut_hdr

      integer(2),allocatable :: ngrid(:,:)

      integer(8),allocatable :: cellid(:)
      real(4), allocatable   :: w_remap(:)
      
      real(4) dbz_polar
      integer(4) index_polar

      integer(1) l_prt
      real(4)   echo_pcnt,echo_count

      type (nids_pars) np
      integer(2) QPEcode
      real(4) r_max_scale

      character(200) lreffname
      real(4),allocatable :: lref(:,:)
      integer(4) icount

      character(200) dirname
      character(20)  netcdf_filen
      character(20)  varname
      character(7)   varunit
      integer(2),allocatable :: i2tem(:)
      integer iindex

      character(200) vfname
      character radarnam(1,5)

!      character(1) ibuf
!      character(1), allocatable :: buf(:)  
!      integer nbytes_in

       integer(2) ibuf_ngate, ibuf_nray 
       character(1) ibuf(10) 

      print*,''
      print*,"*************************************************"
      print*,"*         WELCOME TO SINGLE RADAR CARTESION     *"
      print*,"*            REFLECTIVITY REMAP!                *"
      print*,"*************************************************"
      print*,''


!//#################################################################
!//  Begin timing the code....
!//#################################################################
      cpu_start=rtc()
     
!    time_t start,end, start1 = 0
!    clock_t cpu_start, cpu_end, cpu_start1 = 0

!    double clocks_per_second = (double) CLOCKS_PER_SEC
!    start = time(NULL)
!    cpu_start = clock()

!//######################################################################
!//  Find time string refferences etc from VOLUME_BASE_REFLECT object
!//######################################################################
   
      call get_radattr_realtime(pp,vbr(actul_tilt),           &
                               timeString,nc_timeString)
    
!      print*,timeString,nc_timeString
!//#######################################################################
!//  Get radar & volume scans attributes from VOLUME_BASE_REFLECT object
!//  and BRIGHTBAND object
!//#######################################################################

      call dataSelect(pp,vbr(actul_tilt),bb_info,rd,          &
               ibb_mode,ibb_radar,bb_top_rad,bb_bot_rad)

       print*, pp.ctrlat, pp.ctrlon,ibb_mode,ibb_radar,bb_top_rad,bb_bot_rad

!//#######################################################################
!//  Setup map projection
!//######################################################################

      call setmap_proj(pp,sm)
   
!//######################################################################
!//  Setup Cartesian analysis grid and find lat/lon for each grid point
!//######################################################################

      allocate(gv_x(pp.nx),gv_y(pp.ny),gv_zp(pp.nz))
      allocate(gv_hterain(pp.nx,pp.ny))
      allocate(gv_gridlat(pp.nx,pp.ny),gv_gridlon(pp.nx,pp.ny))

      call ini_grd(pp,sm,gv_x, gv_y, gv_zp, gv_hterain, gv_gridlat, gv_gridlon) 

      do k=1,pp.nz,1
       gv_zp(k)=pp.zp(k)
      enddo   

      dx_km = dble(pp.dx) *111.20 * cos (d2rad*pp.ctrlat)
      dy_km = dble(pp.dy) *111.20

!//#######################################################################
!//  Find i- and j-indices of the radar site.
!//#######################################################################

      call lltoxy_func(rd.latrad,rd.lonrad,xrad,yrad,sm)

      rd.ixrad = int((xrad-gv_x(1))/pp.dx)  +  1 
      rd.jyrad = int((yrad-gv_y(1))/pp.dy)  +  1

      print*,rd.ixrad,rd.jyrad,dx_km,dy_km

!//#######################################################################
!//  Calculate physical grid resolution
!//#######################################################################

      if(pp.mapproj.eq.4) then
       
        d_lon=(gv_gridlon(pp.nx,pp.ny/2+1)-gv_gridlon(1,pp.ny/2+1)) /real(pp.nx-1)
        d_lat=(gv_gridlat(pp.nx/2+1,pp.ny)-gv_gridlat(pp.nx/2+1,1)) /real(pp.ny-1)

        if (d_lon.lt.0.0) d_lon = -d_lon
        arg = d2rad*gv_gridlat(pp.nx/2,pp.ny/2)
        pp.dx_meter = er_temp *cos(arg)*d_lon
        pp.dy_meter = er_temp *d_lat

      else

        pp.dx_meter = pp.dx
        pp.dy_meter = pp.dy

      endif

      print*,d_lon,d_lat,pp.dx_meter,pp.dy_meter 

!//#######################################################################
!//  More initialization....
!//  Prepare for the mosaic data arrays......
!//#######################################################################

!      if(pp.debug_flag) {
!      start1 = time(NULL);
!      cpu_start1 = clock(); }
       
      allocate(sum_wgt(pp.nz,pp.ny,pp.nx))
      allocate(grid_init(pp.nz,pp.ny,pp.nx))   
 
    
      do i=1,pp.nx
      do j=1,pp.ny
      do k=1,pp.nz
        sum_wgt(k,j,i)=0.0
        sum_wref(k,j,i)=0.0
        grid_init(k,j,i)=0
      enddo
      enddo
      enddo

      print*,nz,ny,nx, pp.nz, pp.ny, pp.nx 

!//###########################################################################
!//  Temperory variables.
!//###########################################################################

      iray=0
      jgate=0
      ktilt=0                      ! polar grid indices
      thisktilt=0                      ! polar grid indices

      nray_lut = 0
      ngate_lut = 0                 ! lookup table info

      ngrid_tilt=0                  ! total # of grid cells influenced by a tilt
!      nn=0

      min_ngate = 0                !  initialization
      istatus = -1

!//###########################################################################
!//  get RUC Wind field array and remap it to 2d array with same dimmension of 
!//  this radar
!//###########################################################################

      if( pp.sync_opt.eq.1 ) then

        allocate(u_Array(pp.rucwindfield_ny,pp.rucwindfield_nx))
        allocate(v_Array(pp.rucwindfield_ny,pp.rucwindfield_nx))

        do i=1,pp.rucwindfield_nx
        do j=1,pp.rucwindfield_ny
            u_Array(j,i) = 0.0
            v_Array(j,i) = 0.0
        enddo
        enddo

 
       ! if( read_RUCWindField(&pp,u_Array,v_Array) == 0 )
       call read_RUCWindField(pp,u_Array,v_Array,flag_read_RUCWindField)
                
       if(flag_read_RUCWindField.eq.0) then

        pp.sync_opt = 0
        deallocate(u_Array,v_Array)
                     
       else 

        radar_ctrx = int((pp.ctrlon - pp.startlon)/pp.rucwindfield_dx) 
        radar_ctry = int((pp.ctrlat - pp.startlat)/pp.rucwindfield_dy)
        ratiox = int(pp.rucwindfield_dx/pp.dx)
        i0 = pp.nx/(2*ratiox)
        ratioy = int(pp.rucwindfield_dy/pp.dy)
        j0 = pp.ny/(2*ratioy)

       endif   
      endif

!//###########################################################################
!//  A big-loop tranverse through all tilts....
!//###########################################################################
      DO 100 thisktilt=1,actul_tilt,1

       print*,"******* PPI = ",ktilt," ***********" 

        numrows =  vbr(thisktilt)%num_gate
        numcols =  vbr(thisktilt)%num_beam
 
       !!! Read in the reflectivity from each tilt and remap it

        allocate(refl_polar(numrows*numcols))

        call readTiltData(vbr(thisktilt),refl_polar,t_hdr,numrows,numcols)

        ref_miss_flag = t_hdr%data_missing_flag

        !!! Get synchronization parameters 
        time_diff_x = pp%time_diff/(dx_km*1000.0)
        time_diff_y = pp%time_diff/(dy_km*1000.0)

        if(pp.debug_flag.eq.1)then 
           print*,"ngate/nray in data: ",t_hdr%ngates,t_hdr%nrays
           print*," elv:  ",thisktilt,t_hdr%elv_angle,t_hdr%hgtrad,vbr(1)%radhgt
        endif

        !!! Open up the mosaic lookup table file and get the header info

        vcp = rd%vcpmod
        print*,'vcp= ', vcp
        if(vcp==121)then
                if(thisktilt<=3)ktilt=1
                if(thisktilt>3.and.thisktilt<=6)ktilt=2
                if(thisktilt>6.and.thisktilt<=9)ktilt=3
                if(thisktilt>9.and.thisktilt<=12)ktilt=4
                if(thisktilt>12.and.thisktilt<=14)ktilt=5
                if(thisktilt==15)ktilt=6
                if(thisktilt==16)ktilt=7
                if(thisktilt==17)ktilt=8
                if(thisktilt==18)ktilt=9
        else
                ktilt=thisktilt
        end if

        if(vcp==121) vcp=21
        if(vcp==211) vcp=11
        if(vcp==212) vcp=12
        if(vcp==215) vcp=12
        if(vcp==35) vcp=12
        if(vcp==221) vcp=21

        if( vcp .ne. 11 .and. vcp .ne. 12 .and. vcp .ne. 21            &
                        .and. vcp .ne. 31 .and. vcp .ne. 32 ) then
         print*,' There is a problem with the vcp mode. '
         stop 

        endif

        print*,'sliu output', trim(pp%lut_dir)
        write(dirfix,9)trim(pp%lut_dir)
 9      format(a)
        print*,'trim(dirfix) = ',trim(dirfix)
        print*,'length of dirfix  ',len_trim(dirfix)
!
!       Initialize the mosaiclut array with blanks
!
        do k=1,200
           mosaiclut(k:k)=' '
        enddo

        radar_dummy = rd%namrad
         rd%namrad = radar_dummy
        if (pp%remap_opt.eq.0) then          ! nearest neighbor
        
	 if(pp.nx.ge.1000.and.pp.ny.ge.1000)                                  &  
           write(mosaiclut,10)trim(pp%lut_dir),pp%nx,pp%ny,trim(rd%namrad),vcp,ktilt,'.NNlut'
         if(pp.nx.ge.1000.and.pp.ny.lt.1000)                                  &  
           write(mosaiclut,20)trim(pp%lut_dir),pp%nx,pp%ny,trim(rd%namrad),vcp,ktilt,'.NNlut'
	 if(pp.nx.lt.1000.and.pp.ny.ge.1000)                                  &  
           write(mosaiclut,30)trim(pp%lut_dir),pp%nx,pp%ny,trim(rd%namrad),vcp,ktilt,'.NNlut'
         if(pp.nx.lt.1000.and.pp.ny.lt.1000)                                  &  
           write(mosaiclut,40)trim(pp%lut_dir),pp%nx,pp%ny,trim(rd%namrad),vcp,ktilt,'.NNlut'

        else if (ibb_mode.eq.1 .and.                                      & 
                 pp%radar_ctr_tsfc .lt. pp%radar_ctr_tsfc_threshold .and. &
                 pp%radar_ctr_tsfc .gt. -999.0 )  then    ! horizontal and vertical interp 
				 
	 if(pp.nx.ge.1000.and.pp.ny.ge.1000)                                  &  
           write(mosaiclut,10)trim(pp%lut_dir),pp%nx,pp%ny,trim(rd%namrad),vcp,ktilt,'.BBlut'
         if(pp.nx.ge.1000.and.pp.ny.lt.1000)                                  &  
           write(mosaiclut,20)trim(pp%lut_dir),pp%nx,pp%ny,trim(rd%namrad),vcp,ktilt,'.BBlut'
         if(pp.nx.lt.1000.and.pp.ny.ge.1000)                                  &  
           write(mosaiclut,30)trim(pp%lut_dir),pp%nx,pp%ny,trim(rd%namrad),vcp,ktilt,'.BBlut'
         if(pp.nx.lt.1000.and.pp.ny.lt.1000)                                  &  
           write(mosaiclut,40)trim(pp%lut_dir),pp%nx,pp%ny,trim(rd%namrad),vcp,ktilt,'.BBlut'	 	

        else               !vertical interp only

         if(pp.nx.ge.1000.and.pp.ny.ge.1000)                                  &  
           write(mosaiclut,10)trim(pp%lut_dir),pp%nx,pp%ny,trim(rd%namrad),vcp,ktilt,'.lut'
         if(pp.nx.ge.1000.and.pp.ny.lt.1000)                                  &
           write(mosaiclut,20)trim(pp%lut_dir),pp%nx,pp%ny,trim(rd%namrad),vcp,ktilt,'.lut'
	 if(pp.nx.lt.1000.and.pp.ny.ge.1000)                                  &  
           write(mosaiclut,30)trim(pp%lut_dir),pp%nx,pp%ny,trim(rd%namrad),vcp,ktilt,'.lut'
         if(pp.nx.lt.1000.and.pp.ny.lt.1000)                                  &  
           write(mosaiclut,40)trim(pp%lut_dir),pp%nx,pp%ny,trim(rd%namrad),vcp,ktilt,'.lut'	     
                      
        endif

10      format(a,'SRC_',i4.4,'x',i4.4,'.',a,'_vcp',i2.2,'_',i2.2,a)
20      format(a,'SRC_',i4.4,'x',i3.3,'.',a,'_vcp',i2.2,'_',i2.2,a)
30      format(a,'SRC_',i3.3,'x',i4.4,'.',a,'_vcp',i2.2,'_',i2.2,a)
40      format(a,'SRC_',i3.3,'x',i3.3,'.',a,'_vcp',i2.2,'_',i2.2,a)

        iflag_looktable = -1

        print*,'sliu output:', trim(mosaiclut)
        mosaiclut = trim(mosaiclut)
!        mosaiclut='/nwprod/fix/SRC_LUT/SRC_601x601.KABR_vcp12_01.lut'
        print*,'sliu output:',trim(rd%namrad)
        mosaiclut(1:1) = '/'
        write(*,*)'index::',index(mosaiclut,'/nwprod/fix/SRC_LUT/SRC_601x601.KABR_')
        radar_dummy = rd%namrad
        write(*,*)'index::',index('KABR',radar_dummy)
        open(13,file=trim(mosaiclut), form='unformatted',access='stream',status='old',   &
                iostat=iflag_looktable)

         if(iflag_looktable.ne.0) then
          print*," ++WARNING : can not open file: ", trim(mosaiclut)
          print*,"Skipping this tilt:  ", ktilt
          goto 105
         endif

      !!! Check if the lookup table is consistent with current mosaic domain and radar VCP.

        !  read(13) lut_hdr,arg,ibuf_ngate,ibuf_nray,(ibuf(i),i=1,2),ngrid_tilt
         read(13) lut_hdr,arg,ibuf_ngate,ibuf_nray,ngrid_tilt

        close(13)

        print*,"lut_hdr = ",lut_hdr.elv_angle,lut_hdr.hgtrad
        print*,"sliu output ngrid_tilt = ", ngrid_tilt

        call compare_tilt_lut (pp, lut_hdr, t_hdr, istatus)  

        if (istatus.ne.1) then
         print*,"++WARNING!  Inconsistant lut and data for this tilt."
         print*,"tilt #",ktilt,"  elevation angle: ",t_hdr.elv_angle
         print*,"Inconsistant tilt and Skipping this tilt"
         goto 105
        endif

       !!! Check if the nominal ele_angle is close enough to the actual ele_angle in the data.

        print*,"arg = ", arg

        if(abs(arg-t_hdr%elv_angle).gt.pp%delv_vcp_margin2)       then
         print*,"ERROR:elv_cur=",t_hdr%elv_angle," elv_lut=",arg       &
              ," tilt:",ktilt," Radar: ",rd%namrad," VCP=",rd%vcpmod
         print*,"Skipping this tilt "
         goto 105
        
        else if(abs(arg-t_hdr.elv_angle).gt.pp%delv_vcp_margin1)  then

         print*,"Warning:  elv_obs=",t_hdr%elv_angle," elv_lut=",arg   &
              ," tilt:",ktilt," Radar: ",rd%namrad," VCP=",rd%vcpmod
        endif
        
          ngate_lut = ibuf_ngate 
          nray_lut  = ibuf_nray

        if(pp.debug_flag.eq.1) print*,"ngate_lut/nray/ngrid_tilt= ",ngate_lut,nray_lut,ngrid_tilt

        if(nray_lut.ne.t_hdr.nrays)then
         print*,"ERROR moslut:  nray_data=",t_hdr.nrays
         print*," nray_lut=",nray_lut
         print*,"Set flag_SRC=0 and Exiting "
         iflag_SRC=0
         goto 9999
        endif

      !!! Read in # of grid cells influence by the current tilt.

        nn=1          ! a counter to keep track the data position in lut file
    

 
        allocate(ngrid(ngate_lut,nray_lut))
        allocate(cellid(ngrid_tilt),w_remap(ngrid_tilt)) 

         open(14,file=trim(mosaiclut), form='unformatted',access='stream',status='old')

         read(14) lut_hdr,arg,ibuf_ngate,ibuf_nray,(ibuf(i),i=1,8),     &
                  ((ngrid(i,j),j=1,nray_lut),i=1,ngate_lut),            &                
                  (cellid(i),i=1,ngrid_tilt),                           &
                  (w_remap(i),i=1,ngrid_tilt)                           
            
         close(14)

         print*,ibuf_ngate,ibuf_nray,ngrid(34,1)
         print*,cellid(1),cellid(2),w_remap(1),w_remap(2)  
                
      !!! Compute the number of gates
        
        min_ngate = min(t_hdr.ngates, ngate_lut)

        if(pp.debug_flag.eq.1) then
         print*,"Remapping: tilt ",ktilt
         print*,"  min_ngate=",min_ngate
         print*,"  nray_lut=",nray_lut
        endif

         print*,'Test: ', sum_wgt(1, 300, 294), sum_wref(1, 300, 294)

      !!! Remap the reflectivity in the current tilt ...

        DO jgate=1,min_ngate,1
        DO iray=1,t_hdr.nrays,1
        
          
         index_polar = (jgate-1) * t_hdr.nrays + iray
   
!         if(index_polar.eq.10000) print*,jgate,iray
!         if(index_polar.eq.20000) print*,jgate,iray

         if(index_polar.gt.numrows*numcols) then
           print*,'Error: index_polar exceeds numrows*numcols'
           stop
         endif 
         
          if (refl_polar(index_polar).ne.ref_miss_flag ) then
            
        dbz_polar=real(refl_polar(index_polar))/real(t_hdr%data_scale)
!!!          dbz_polar=refl_polar(index_polar)
          if (pp%remap_opt.eq.1) then
           ref_mm6 = 10.0**(0.1*dbz_polar)
          else 
           ref_mm6 = dbz_polar
          endif

          endif   !!!!!if (ref.ne.missing)

          if (ngrid(jgate,iray).gt.0) then

!!!!! Regular 3D remapping
          
          do n=1,ngrid(jgate,iray),1
           
            k = cellid(nn)/(lut_hdr.nx*lut_hdr.ny)                          +1
            j = mod(int(cellid(nn)),lut_hdr.nx*lut_hdr.ny)/lut_hdr.nx       +1
            i = mod(mod(int(cellid(nn)),lut_hdr.nx*lut_hdr.ny),lut_hdr.nx)  +1

            if(i.lt.1.or.i.gt.pp.nx.or.j.lt.1.or.          &
               j.gt.pp.ny.or.k.lt.1.or.k.gt.pp.nz) then
             
              print*,"++ERROR outbounds.  i/j/k: ",i,j,k,jgate,iray
              print*,"  nx/ny/nz: ",pp.nx,pp.ny,pp.nz
              print*,"Set flag_SRC = 0 and Exiting"
              iflag_SRC = 0
              goto 9999
            
             endif
           
             IF( pp.sync_opt .eq. 1 )  THEN   !sync
             
              iu = i/ratiox -i0 + radar_ctrx
              jv = j/ratioy -j0 + radar_ctry
              if( iu .ge.1 .and.iu .le. pp.rucwindfield_nx .and.   &
                  jv .ge.1 .and.jv .le. pp.rucwindfield_ny ) then
              
               if(u_Array(jv,iu).gt. pp%rucwindfield_missing + 1.0 ) then
               
                i1 = i + int(u_Array(jv,iu)*time_diff_x)
                if(i1.ge.1.and.i1.le.pp%nx) i = i1
               
               endif
         
               if(v_Array(jv,iu).gt. pp%rucwindfield_missing + 1.0 ) then

                j1 = j + int(v_Array(jv,iu)*time_diff_y)
                if(j1.ge.1.and.j1.le.pp%ny )j = j1
               
               endif
              
              endif

             ENDIF
             
             if (refl_polar(index_polar).ne.ref_miss_flag ) then       
             
               if(grid_init(k,j,i).eq.-1) then
                  sum_wgt(k,j,i) = 0.
                  sum_wref(k,j,i) = 0.
               endif
                                
               grid_init (k,j,i) = 1
               sum_wgt   (k,j,i) = w_remap(nn) +  sum_wgt(k,j,i)
               sum_wref  (k,j,i) = w_remap(nn)*ref_mm6 + sum_wref(k,j,i)

             else  !!!!! Flag the affected grid cells as having coverage but with no echo

                 if (grid_init(k,j,i).eq.0 ) then
                    grid_init(k,j,i) = -1 
                    sum_wgt (k,j,i) = radar_cover_flag
                    sum_wref(k,j,i) = radar_cover_flag
                 endif
              
              endif 


             if(k.eq.1.and.j.eq.300.and.i.eq.294) & 
               print*,'NO: ', w_remap(nn), ref_mm6, sum_wgt(k,j,i), sum_wref(k,j,i)

             nn = nn + 1 
             
          enddo    

         endif   !!!!!if ngrid > 0      

        ENDDO              ! iray-loop
        ENDDO              ! jgate-loop


        print*,"nn= ",nn-1
        if(nn-1.gt.ngrid_tilt) then
        print*,"Error: nn-1 gt ngrid_tilt"
        stop
        endif

        deallocate(ngrid,cellid,w_remap)

105    deallocate(refl_polar) 
  
100   CONTINUE             ! ktilt-loop

      deallocate(grid_init)

      if( pp.sync_opt .eq. 1) deallocate( u_Array,v_Array)
    
!//#######################################################################
!//  Compute the final weighted mean
!//#######################################################################

      print*,"Sum...."
      
      l_prt = 0
      echo_pcnt = 0

      print*,'befor_src..  ', sum_wref(1,300,294),sum_wgt(1,300,294)
      print*,'befor_src..  ', sum_wref(1,300,304),sum_wgt(1,300,304)

      DO k = 1, pp%nz, 1

       echo_count = 0

       do j = 1, pp%ny, 1
       do i = 1, pp%nx, 1
      
        if ( abs (sum_wgt(k,j,i)- radar_cover_flag) .lt. 0.1) then
        
         sum_wref(k,j,i) = radar_cover_flag
        
        else if(sum_wgt(k,j,i) .gt. 0.50)  then  ! to avoid extrapolation beyond beamwidth
        
         if(pp%remap_opt.eq.1) then 
          sum_wref(k,j,i) = 10.0*log10 (sum_wref(k,j,i)/sum_wgt(k,j,i))
         else 
          sum_wref(k,j,i) = sum_wref(k,j,i)/sum_wgt(k,j,i)
         endif

           if(sum_wref(k,j,i).gt.60.0) print*, sum_wref(k,j,i), k,j,i 
        
        else
        
         sum_wref(k,j,i) = real(ref_miss)
      
        endif

 !       write(0,*) sum_wref(k,j,i) 
        
        if (sum_wref(k,j,i).gt. real(dbz_clear_air) + 1.0e-5)  & 
            echo_count = echo_count + 1.0

       enddo
       enddo    ! end of j- and i-loops

       if (echo_count .gt. echo_pcnt) echo_pcnt = echo_count
  
      ENDDO    ! end of k-loops
   

      echo_pcnt = echo_pcnt / real(pp.nx*pp.ny) * 100.0


      deallocate(sum_wgt)

      print*,'src..  ', sum_wref(1,300,294)
      print*,'src..  ', sum_wref(1,300,304)


!!!!!!!!!!!!!!!!!!!!!!! construct src_data_header !!!!!!!!!!!!!!!!!!!!
      
      src_data_header.nx   =  pp.nx
      src_data_header.ny   =  pp.ny
      src_data_header.nz   =  pp.nz
      src_data_header.dx   =  pp.dx
      src_data_header.dy   =  pp.dy
      src_data_header.nw_lat = pp.ctrlat + pp.ny/2*pp.dy
      src_data_header.nw_lon = pp.ctrlon - pp.nx/2*pp.dx
      do i = 1, pp.nz
      src_data_header.zp(i) = pp.zp(i)
      enddo

      src_data_header.missing_value = radar_cover_flag 
         
!//######################################################################
!//  Re-project the mosaic grid into a 1 km x 1 km NIDS raster grid.
!//######################################################################

      np%nx_n = int ( dble(pp.nx) * dx_km/dx_nids_km)
      np%ny_n = int ( dble(pp.ny) * dy_km/dx_nids_km)
      np%dlon_n = dble(pp.dx) * dx_nids_km/dx_km
      np%dlat_n = dble(pp.dy) * dx_nids_km/dy_km

      QPEcode=0
      r_max_scale = 1.0

!//#######################################################################
!//  Deriving pre-specified height level relectivity products.
!//#######################################################################

      IF( pp%nids_lref_flag.eq.1 ) THEN
    
       allocate(lref(pp%nx,pp%ny))        

       DO  k=1, pp%nz, 1
        
!        write(lreffname,40)pp%nids_lref_dir,rd%namrad,k,rd.namrad,
!     &                     timeString,pp%nids_lref_name_tag,k
!40      format(a,'/',a,'/','src',i4.4,'_rad','/',a,
!     &         '.',a,'.',a,'.',i4.4,'.nids')

        QPEcode=2000+k+1
        r_max_scale = 1.0

        icount = 0
         
        do i=1,pp%nx,1
        do j=1,pp%ny,1      
          lref(i,j) = sum_wref(k,j,i)
          if(lref(i,j) .gt. 5.0) icount = icount + 1    
        enddo
        enddo
        
        if(pp.debug_flag.eq.1)  &
          print*,"On layer: ",k," the cell # with ref >5.0:",icount

!        call nids_output(pp%config_file_name,lreffname,pp,lref,
!     &                   np.dlon_n, np.dlat_n, np.nx_n, np.ny_n,QPEcode,
!     &                   REF_VALUES_TAG, REF_COLORS_TAG,
!     &                   real(ref_miss), r_max_scale)

        if(pp.debug_flag.eq.1) print*,"Finished writing file:",lreffname

       ENDDO  !k-loop 

       deallocate(lref)
  
      ENDIF    !  end if(pp.nids_lref_flag)

!//#######################################################################
!//  Output 3D mosaic netcdf file.
!//#######################################################################

      varname = "mosaicked refl"
      varunit = "dbz   "

      allocate(i2tem(pp%nx*pp%ny*pp%nz))
      
      do k=1,pp%nz,1
      do j=1,pp%ny,1
      do i=1,pp%nx,1

       iindex = k*pp%nx*pp%ny + j*pp%nx + i
       if( sum_wref(k,j,i) .lt. real(ref_miss) +1.0)  then
!           i2tem(iindex) = get_int(ref_miss*ref_scale)
       else
!           i2tem(iindex) = get_int(ref_scale*sum_wref(k,j,i))
       endif

      enddo
      enddo
      enddo 

      if( pp.mosaic3d_netcdf_flag.eq.1) then

       netcdf_filen = "mrefl_mosaic"
       write(dirname,50)pp%mosaic3d_netcdf_dir,rd%namrad
50     format(a,'/',a)
    
!      NETCDF_HANDLER mosaic3d(1,pp,"mosaic3d",
!                 dirname,nc_timeString,
!                 timeString,gv.gridlon[0][pp.ny-1],
!                 gv.gridlat[0][pp.ny-1])

!      mosaic3d.ith_netcdf_wrt(varname,varunit,netcdf_filen,
!                 i2tem, ref_scale, ref_miss,
!                 pp, gv, 1, pp.nz, min_ref_rang,max_ref_rang,1)


      endif   ! end of if(pp.mosaic3d_netcdf_flag)

!//#######################################################################
!//  Output 3D mosaic Binary files.
!//#######################################################################

      if( pp.mosaic3d_binary_flag.eq.1 ) then
       
       radarnam = rd%namrad
       dirname = pp%mosaic3d_binary_dir
       
!       write(vfname,60)rd%namrad,timeString,pp.mosaic3d_binary_name_tag
!60     format(a,'/',a,'.',a,'.bin')

!       i2wrtvar_cart3d(dirname,vfname,latlon_scale,
!                gv.gridlon[0][pp.ny-1],gv.gridlat[0][pp.ny-1],
!                latlon_scale,dlatdlon_scale,meter_scale,
!                varname,varunit,1,radarnam,
!                i2tem,ref_scale, ref_miss, pp,
!                gv,1, pp.nz)
           
      endif
  
      deallocate(gv_x,gv_y,gv_zp,gv_hterain,gv_gridlat,gv_gridlon)     
      deallocate(i2tem)    


!  End Timing the code and output mosaic infomation like cpu time etc
      
      iflag_SRC = 1

      
      cpu_end = rtc() 
   
      print*,"Total clock time: ",cpu_end - cpu_start

9999  RETURN

      END  SUBROUTINE src 

