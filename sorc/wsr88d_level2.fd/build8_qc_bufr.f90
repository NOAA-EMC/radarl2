!
!  The raw data format is shown below:
!
! Volume Record Header
! ********************
! A 24-byte header that consists of
! Tape Filename Extension    Date     Time:Millisecs  ICAO
!               Number       Julian   past midnight   ID
! 9bytes        3bytes       4bytes   4bytes          4bytes
! 'AR2V0001.'   '001'  (Message type 01 legacy format)
! Size Record Header
! ******************
! A 4-byte integer field. This value specifies the total size
! of the next LDM Compressed Record that immediately follows.
! LDM Compressed Record
! *********************
! A record that is bzip2 compressed. It consists of Metadata
! message types 15, 13, 18, 3, 5, and 2. Metadata is only in
! Compressed Record #1.
! Size Record Header
! ******************
! A 4-byte integer field. This value specifies the total size of
! the next LDM Compressed Record that immediately follows.
! LDM Compressed Record
! *********************
! A variable size record that is bzip2 compressed. it consists of
! 100 Archive II message types, or up to 100 Archive II messages
! with the last message having a radial status signaling "end of
! elevation" or "end of volume"
!---------------------------------------------------------------
! Repeat (Size Record Header with preceding LDM Compressed Record)
!                   Or
!           End of File (for end of volume data)
!
!******************************************************************
! LDM Compressed Record
! *********************
! The bzip2 library will be used for data compression with a block
! size of 300K.
! Fixed Buffers in the LDM Compressed Record
! ******************************************
! The first LDM Compressed Record is the metadata. It will be a fixed
! size uncompressed, but will vary in size once compressed (depending
! on the data). Any user not interested in metadata can skip the first
! compressed record.
!
! Message type 15 will be 14 blocks of 2432 bytes
! Message type 13 will be 14 blocks of 2432 bytes
! Message type 18 will be 6 blocks of 2432 bytes
! Message type 3 will be 1 block of 2432 bytes
! Message type 5 will be 1 block of 2432 bytes
! Message type 2 will be 1 block of 2432 bytes
! Each of the messages of type 1 or 2 in the preceding LDM Compressed
! Records will be stored into a fixed buffer size of 2432 bytes. The
! unused portion of the buffer is zero filled.
!
! Therefore, an LDM compressed record after the first metadata record
! is defined as a group of Archive II messages that consist of 100
! messages/radials (2432 bytes each), or up to 100 messages/radials
! with the last message having a radial status signaling "end of elevation"
! or "end of volume".
!
! Modified by Krishna Kumar on 05/01/2006
!
! Previously the bytes 15-16 were taken together to represent for
! digital radar data (should be 1 if radar data present) without considering
! the significance of the upper byte 15. The upper byte value corresponds to   
! the Radar Data Acquisition (RDA) channel information which is used to denote
! Channel 1 or Channel 2 on a FAA Redundant RDA and also whether the data is
! Open RDA (ORDA) data. The RDA channel values for "Legacy" are 0 (single channel)
! 1 (Redundant Channel 1) and 2 (Redundant Channel 2) and for ORDA are: 8 (single channel),
! 9 (Redundant Channel 1) and 10 (Redundant Channel 10). About ~2/3 rd of the
! stations reported a change in the channel number and now only a ~1/3 rd of 
! the stations are reporting a value "0" for byte 15 ("Legacy" (single channel)) and 
! the remaining 2/3 rd of the stations a value "8" ("ORDA" single channel) 
! and together the byte 15-16 value was "0x801" or 2049 for the 2/3 rd of stations 
! thereby reporting missing radar data with the message "No radar data found". 
! Since we are interested only in type 1 messages regardless of whether it is legacy RDA or
! ORDA, based on Steve Smith's (Radar Operations Center) recommendation, the upper byte
! was masked and now only the byte 16 was read to check for the presence of digital radar
! data (reflectivity, radial velocity/spectral width).
!========================================================
!* Shun Liu share data module =====Start
!* date: 05/22/2005
module sdata1
   implicit none
   integer, parameter :: nazim1=760,ngate1=1000,nvol1=30
   type ccout_strct
     character(len=8) :: radar_name
     integer ::  vcpnum
     integer ::  year
     integer ::  month
     integer ::  day
     integer ::  hour
     integer ::  minute
     integer ::  second
     real ::  radlat
     real ::  radlon
     real ::  radhgt
     real ::  elev_angle
     real ::  fstgatdis
     real ::  nyq_vel
     integer ::   num_beam
     integer ::   num_gate
     real ::  gateWidth(nazim1)
     real ::  elev(nazim1)
     real ::  azim(nazim1)
     real ::  field(ngate1,nazim1)
   end type ccout_strct

   type qcpara
     integer :: intmax_azim    !*azimuth control
     integer :: con_jump_azim  !*azimuth control: continuity of jump along beam
     integer :: intmax_gate    !*azimuth control
     integer :: con_jump_gate  !*azimuth control: continuity of jump along beam
     integer :: iref      !*reflectivity average
     integer :: iref0
     integer :: iref5
     integer :: iref35
     integer :: imaxv     !* the max number of valid velocity along a beam
     real    :: sn_minus_9pt      !*sign change

     real    :: avg_sd   !*sign change
     real    :: sn        !*sign change
     real    :: refavg    !* averager reflictivity
     real    :: sn_minus_avg      !*sign change
     real    :: covrate   !* the coverage rate of the velocity
     integer :: vcp
     character(len=20) :: mark
     integer :: vad_fail
     integer :: value
   end type qcpara

   type mem_para_ap
     real:: MDREF(3)
     real:: MDVE(3)
     real:: MDSW(3)
     real:: SDVE(3)
     real:: SDSW(3)
     real:: TDBZ(3)
     real:: SPIN(3)
     real:: SIGN(3)
     real:: GDZ(3)
   end type mem_para_ap

type ccout_strct_ncep
     sequence
     character(len=8) :: radar_name
     integer ::  vcpnum(nazim1)
     real(4) ::  year(nazim1)
     real(4) ::  month(nazim1)
     real(4) ::  day(nazim1)

     real(4) ::  hour(nazim1)
     real(4) ::  minute(nazim1)
     real(4) ::  second(nazim1)     ! in millisecond precision for passing into bufr
                                    ! passing into bufr for every radial line
     real ::  radlat(nazim1)
     real ::  radlon(nazim1)
     real ::  radhgt(nazim1)
     real ::  elev_angle(nazim1)
     real ::  fstgatdis(nazim1)
     real ::  nyq_vel(nazim1)
     integer ::   num_beam(nazim1)
     integer ::   num_gate(nazim1)
     real ::  gateWidth(nazim1)
     real ::  azim(nazim1)
     real ::  field(ngate1,nazim1)
     real ::  gate_dis(ngate1,nazim1)
end type ccout_strct_ncep

type ccout_strct_ncep_tmp
     sequence
     character(len=8) :: radar_name
     integer ::  vcpnum
     integer ::  year
     integer ::  month
     integer ::  day
     integer ::  hour
     integer ::  minute
     integer ::  second

     real ::  radlat
     real ::  radlon
     real ::  radhgt
     real ::  elev_angle
     real ::  fstgatdis
     real ::  nyq_vel
     integer ::   num_beam
     integer ::   num_gate
     real ::  gateWidth(nazim1)
     real ::  elev(nazim1)
     real ::  azim(nazim1)
     real ::  field(ngate1,nazim1)
end type ccout_strct_ncep_tmp

type ccout_strct_nssl               !X.Y.X adds
     sequence
     integer(4) ::  vcpnum
     integer(4) ::  year
     integer(4) ::  month
     integer(4) ::  day
     integer(4) ::  hour
     integer(4) ::  minute
     integer(4) ::  second

     real(4) ::  radlat
     real(4) ::  radlon
     real(4) ::  radhgt
     real(4) ::  elev_angle
     real(4) ::  fstgatdis
     real(4) ::  nyq_vel
     integer(4) ::   num_beam
     integer(4) ::   num_gate
     real(4) ::  gateWidth(nazim1)
     real(4) ::  elev(nazim1)
     real(4) ::  azim(nazim1)
     real(4) ::  field(ngate1,nazim1)
     real(4) ::  qc_flag(ngate1,nazim1)
end type ccout_strct_nssl

type ccout_strct_src_header
            integer*4        :: nx
            integer*4        :: ny
            integer*4        :: nz
            real *4          :: dx
            real *4          :: dy
            real *4          :: nw_lat
            real *4          :: nw_lon
            real *4          :: zp(31)
            real *4          :: missing_value
!missing_value for the 2nd status: no echo but covered by radar
end type ccout_strct_src_header

character(len=24):: srcfilenm
!character(len=6) :: volumeid_char
character(len=28):: srcfilenm1
character(len=28):: superobnm
end module sdata1
!* Shun LIU share data module =====End

 module radar_data
 use sdata1
 implicit none
 save
 logical:: dual_index
 type(ccout_strct_ncep) :: strct_refvel(6,18)
 end module radar_data
!* S. Guan  share data module =====End

!========================================================
!
!********************************************************************************
!
! Main program for level 2
!
!********************************************************************************

subroutine bufrdtab(nexrad,radar,ifnd)
  character(256) nexrad 
  character(8) radar 
  integer ::  ifnd
  character(8) stid 
  real(8) radar_lat,radar_lon,radar_height
  integer(4) radar_elev_above_station
  real(8) station_elev,height
  character(132) buffer
  logical found
  common/radard/stid,radar_lat,radar_lon,radar_height,station_elev,radar_elev_above_station
!
!  check now to see if radar name is in table of locations
  ifnd=0
  open(unit=12,file=nexrad,form='formatted')

!print '(" looking for radar_name = ",a4)',radar_name  

  found = .false.
  iost1 = 0
  do while ( ( .not. found ) .and. ( iost1 .eq. 0 ) )
   buffer = ' '
   read(12,fmt='(a)', iostat=iost1) buffer
   if ( iost1 .eq. 0 ) then
      if ( buffer(1:1) .ne. '!' ) then   ! exclude comment lines
          read ( buffer, fmt='(a8, 47x, f5.2, 1x, f6.2, 1x, f6.1 , 2x, i2)', iostat=iost2) &
             stid, radar_lat, radar_lon, radar_height, radar_elev_above_station

!         print '(" stid,lat,lon,height,elev_above,iost2  = ",a3,1x,f7.2,1x,f7.2,1x,f7.1,1x,i2,i2)', &
!           stid, radar_lat, radar_lon, radar_height, radar_elev_above_station,iost2 

          if ( iost2 .eq. 0 ) then
             if ( radar(2:4) .eq. stid(1:3) ) then

                ifnd=1

                found = .true.
                height = radar_elev_above_station
                station_elev = radar_height - height
!                 station_elev is the height of the station ground above MSL
!                 radar_height is the height of the antenna above MSL
!                 radar_elevation_above_station is the height of the antenna above station ground
             end if
          end if
      end if
   end if
 end do
 close(12)

end subroutine bufrdtab

subroutine jday2dmy(jday,rtime,iday,imonth,iyear,rhour,rminute,rsecond)

  integer(2) jday
  real (4)  rtime,rhour,rminute,rsecond
  real (4) rhourmod,rminutemod

   l      = jday + 68569 + 2440587
   n      = 4 * l / 146097
   l      = l - (146097 * n + 3) / 4
   i      = 4000 * (l + 1) / 1461001
   l      = l - 1461 * i / 4 + 31
   j      = 80 * l / 2447
   iday    = l - 2447 * j / 80
   l      = j / 11
   imonth  = j + 2 - 12 * l
   iyear   = 100 * (n - 49) + i + l

   rhour   = int(rtime / 3600.0)
   if(rhour .gt. 0.0)then
      rhourmod = mod( (rtime / 3600.0),rhour)
   else
      rhourmod = rtime/3600.0
   endif

   rminute = int(rhourmod*60.0)
   if(rminute .gt. 0.0)then
      rminutemod = mod( (rhourmod*60.0),rminute)
   else
      rminutemod = rhourmod*60.0
   endif
   rsecond = rminutemod*60.0
end subroutine jday2dmy

!========================================================
!!!shun liu set defaut value for tmp strct=====Start

Subroutine zero_tmp(strct_tmp)
   use sdata1
   implicit none

   type(ccout_strct_ncep_tmp) :: strct_tmp

   strct_tmp%vcpnum=0
   strct_tmp%year=0
   strct_tmp%month=0
   strct_tmp%day=0
   strct_tmp%hour=0
   strct_tmp%minute=0
   strct_tmp%second=0

   strct_tmp%radlat=0.0
   strct_tmp%radlon=0.0
   strct_tmp%radhgt=0.0
   strct_tmp%elev_angle=0.0
   strct_tmp%fstgatdis=0.0
   strct_tmp%nyq_vel=0.0
   strct_tmp%num_beam=0
   strct_tmp%num_gate=0
   strct_tmp%gateWidth=0.0
   strct_tmp%elev=0.0
   strct_tmp%azim=0.0
   strct_tmp%field=999.0

End subroutine zero_tmp
!!!shun liu set defaut value for tmp strct=====End
!========================================================

!========================================================
!!!shun liu write out a strct for plot in linux====Start

Subroutine write_strct(numid,strct_tmp)
   use sdata1
   implicit none
   integer :: numid,i,j

   type(ccout_strct_ncep_tmp) :: strct_tmp

!   write(numid,'(i5)')strct_tmp%vcpnum
!   write(numid,'(i5)')strct_tmp%year
!   write(numid,'(i5)')strct_tmp%month
!   write(numid,'(i5)')strct_tmp%day
!   write(numid,'(i5)')strct_tmp%hour
!   write(numid,'(i5)')strct_tmp%minute
!   write(numid,'(i5)')strct_tmp%second

!   write(numid,'(f8.2)')strct_tmp%radlat
!   write(numid,'(f8.2)')strct_tmp%radlon
!   write(numid,'(f8.2)')strct_tmp%radhgt
!   write(numid,'(f8.2)')strct_tmp%elev_angle
!   write(numid,'(f8.2)')strct_tmp%fstgatdis
!   write(numid,'(f8.2)')strct_tmp%nyq_vel
!   write(numid,'(i5)')strct_tmp%num_beam
!   write(numid,'(i5)')strct_tmp%num_gate
!   write(numid,'(20f8.2)')strct_tmp%gateWidth
!   write(numid,'(20f8.2)')strct_tmp%elev
!   write(numid,'(20f8.2)')strct_tmp%azim
!   write(numid,'(20f8.2)')strct_tmp%field

    open(5,file='ncar.vr')
!   write(5,'(15f5.1)')(strct_tmp%azim(i),i=1,367)
!   do j=1,367
!     write(5,'(20f6.1)')(strct_tmp%field(i,j),i=1,920)
!   end do
    close(5)

End subroutine write_strct
!!!shun liu write out a strct for plot in linux====End
!========================================================

!========================================================
!!!shun liu copy strct to a simple strct=====Start

Subroutine copy_strct_in(strct_tmp,strct)
   use sdata1
   implicit none
   integer :: i,na,maxnaz,ii,thisyear,thisbeamnum

   type(ccout_strct_ncep_tmp) :: strct_tmp

   type(ccout_strct_ncep) :: strct
   real    :: field(ngate1,nazim1), field_sort(ngate1,nazim1)
   real    :: azim(nazim1), azim_sort(nazim1)
   integer,dimension(nazim1):: indx
   real, parameter:: spval=999.0

   strct_tmp%radar_name=strct%radar_name

   maxnaz=maxval(strct%num_beam)
   if(maxnaz<=0.or.maxnaz>380) then
     write(*,*),'azimuth number is not in normal range'
     stop
   end if

   ii=0
   do i=1,maxnaz
   thisyear=strct%year(i)
   thisbeamnum=strct%num_beam(i)
   if(thisyear>0 .and. thisbeamnum>0)then
     ii=ii+1 
     strct_tmp%elev_angle=strct_tmp%elev_angle+strct%elev_angle(i)
     strct_tmp%azim(ii)=strct%azim(i)
     strct_tmp%field(:,ii)=strct%field(:,i)
   end if
!  write(*,'(i6, f8.2,i8,2f8.2,i8)')i,strct%year(i),strct%num_beam(i),strct%elev_angle(i),strct%azim(i),strct%num_gate(i)
   end do

   maxnaz=ii
   strct_tmp%elev_angle=strct_tmp%elev_angle/maxnaz
   strct_tmp%elev=strct%elev_angle

   strct_tmp%num_beam=maxnaz
   strct_tmp%vcpnum=strct%vcpnum(1)
   strct_tmp%year=strct%year(1)
   strct_tmp%month=strct%month(1)
   strct_tmp%day=strct%day(1)
   strct_tmp%hour=strct%hour(1)
   strct_tmp%minute=strct%minute(1)
   strct_tmp%second=strct%second(1)

   strct_tmp%radlat=strct%radlat(1)
   strct_tmp%radlon=strct%radlon(1)
   strct_tmp%radhgt=strct%radhgt(1)
   strct_tmp%fstgatdis=strct%fstgatdis(1)
   strct_tmp%nyq_vel=strct%nyq_vel(1)


   if(maxval(strct%num_gate)<=0) then
     print*,'gate number is negative'
     stop
   end if
   if(maxval(strct%num_gate)>=1900) then
     print*,'gate number is larger than 1900'
      stop
   end if

   strct_tmp%num_gate=maxval(strct%num_gate)
   strct_tmp%gateWidth=strct%gateWidth(1)

   if(1==1)then
   field_sort=spval; azim_sort=spval
   field_sort=strct_tmp%field
   azim_sort=strct_tmp%azim

   azim=spval;field=spval
   field=strct_tmp%field
   azim=strct_tmp%azim
   na=strct_tmp%num_beam
!  write(*,*)'na=', na, nazim1,ngate1
   call datasort(nazim1,ngate1,na,ngate1,azim,azim_sort,field, &
                         field_sort,indx)

   strct_tmp%azim=azim_sort
   strct_tmp%field=field_sort
   end if
      

   if(1==0)then
   strct_tmp%vcpnum=strct%vcpnum(1)
   strct_tmp%year=strct%year(1)
   strct_tmp%month=strct%month(1)
   strct_tmp%day=strct%day(1)
   strct_tmp%hour=strct%hour(1)
   strct_tmp%minute=strct%minute(1)
   strct_tmp%second=strct%second(1)
   end if

End subroutine copy_strct_in
!!!shun liu copy strct to simple strct=====End
!========================================================

!========================================================
!!!shun liu copy simple strct back to strct=====Start

Subroutine copy_strct_out(strct_tmp,strct)
   use sdata1
   implicit none
   integer :: i

   type(ccout_strct_ncep_tmp) :: strct_tmp
   type(ccout_strct_ncep) :: strct

   strct%azim=strct_tmp%azim
   strct%field=strct_tmp%field

End subroutine copy_strct_out
!!!shun liu copy strct to simple strct=====End
!========================================================


!************X.Y.X adds**********************

Subroutine zero_nssl_tmp(strct_tmp)
   use sdata1
   implicit none
   type(ccout_strct_nssl) :: strct_tmp

   strct_tmp%vcpnum=0
   strct_tmp%year=0
   strct_tmp%month=0
   strct_tmp%day=0
   strct_tmp%hour=0
   strct_tmp%minute=0
   strct_tmp%second=0
   strct_tmp%radlat=0.0
   strct_tmp%radlon=0.0
   strct_tmp%radhgt=0.0
   strct_tmp%elev_angle=0.0
   strct_tmp%fstgatdis=0.0
   strct_tmp%nyq_vel=0.0
   strct_tmp%num_beam=0
   strct_tmp%num_gate=0
   strct_tmp%gateWidth=0.0
   strct_tmp%elev=0.0
   strct_tmp%azim=0.0
   strct_tmp%field=999.0
   strct_tmp%qc_flag=0.0
End subroutine zero_nssl_tmp

Subroutine copy_strct_in_nssl(strct,strct_tmp)
   use sdata1
   implicit none
   integer :: i
   type(ccout_strct_nssl) :: strct_tmp
   type(ccout_strct_ncep) :: strct

   strct_tmp%vcpnum=strct%vcpnum(1)
   strct_tmp%year=strct%year(1)
   strct_tmp%month=strct%month(1)
   strct_tmp%day=strct%day(1)
   strct_tmp%hour=strct%hour(1)
   strct_tmp%minute=strct%minute(1)
   strct_tmp%second=strct%second(1)
   strct_tmp%radlat=strct%radlat(1)
   strct_tmp%radlon=strct%radlon(1)
   strct_tmp%radhgt=strct%radhgt(1)
   strct_tmp%elev_angle=strct%elev_angle(1)
   strct_tmp%fstgatdis=strct%fstgatdis(1)
   strct_tmp%nyq_vel=strct%nyq_vel(1)
   strct_tmp%num_beam=maxval(strct%num_beam)
   strct_tmp%num_gate=strct%num_gate(1)
   strct_tmp%gateWidth=strct%gateWidth(1)
   strct_tmp%elev=strct%elev_angle
   strct_tmp%azim=strct%azim
   strct_tmp%field=strct%field

   if(1==0)then
   strct_tmp%vcpnum=strct%vcpnum(1)
   strct_tmp%year=strct%year(1)
   strct_tmp%month=strct%month(1)
   strct_tmp%day=strct%day(1)
   strct_tmp%hour=strct%hour(1)
   strct_tmp%minute=strct%minute(1)
   strct_tmp%second=strct%second(1)
   end if
End subroutine copy_strct_in_nssl

Subroutine copy_strct_out_nssl(strct_tmp,strct)
   use sdata1
   implicit none
   integer :: i
   type(ccout_strct_nssl) :: strct_tmp
   type(ccout_strct_ncep) :: strct

   strct%field=strct_tmp%field
End subroutine copy_strct_out_nssl

!*********X.Y.X struct subs End *******

subroutine zero(strct)

use sdata1
implicit none

type(ccout_strct_ncep) :: strct

integer(4) i,j

strct%radar_name='        '
 strct%vcpnum=0
 strct%year  =0.
 strct%month =0.
 strct%day   =0.
 strct%hour  =0.
 strct%minute=0.
 strct%second=0.
 strct%radlat=0.
 strct%radlon=0.
 strct%radhgt=0.
 strct%elev_angle=0.
 strct%fstgatdis =0.
 strct%nyq_vel   =0.
 strct%num_beam=0
 strct%num_gate=0
 strct%gateWidth=0.
 strct%azim=0.
 strct%field=999.0
 strct%gate_dis=0.

end subroutine zero
!!!!
   subroutine bufrout(lunout,lundx,radar_id,volumeid,nyq_dvel,vcp_dvel,radar_lat,radar_lon, &
     radar_elev_above_station,station_elev, &
     year_refl,year_dvel,year_spec, & 
     month_refl,month_dvel,month_spec, &
     day_refl,day_dvel,day_spec, & 
     hour_refl,hour_dvel,hour_spec, &
     minute_refl,minute_dvel,minute_spec, &
     second_refl,second_dvel,second_spec, &
     nvol,nazm_ref_max,nazm_vel_max,nazm_spe_max, &
     numrefgate_max,numvelgate_max,numspegate_max, & 
     refl_gate_dis,reflectivity,dvel_gate_dis,dvelocity,spec_gate_dis,spectralwidth, & 
     azimuth_angle_refl,azimuth_angle_dvel,azimuth_angle_spec, &
     elevation_angle_refl,elevation_angle_dvel,elevation_angle_spec,flag_qcrw_dvel)
!!!
use sdata1
use radar_data

integer, parameter :: nmsgsize=10000
integer, parameter :: maxbyt=50000
integer, parameter :: lmbstr=12, lmbstrm1=lmbstr-1
!!!
  character(len=80) hdrstr1,hdrstr2_1,hdrstr2_2,rptstr1,rptstr2
  character(len=8) radar_id,subset1,subset2
  character(len=8) cdata
  character(len=lmbstr) mbstr  ! needs to be one bigger than the maximum length string it will hold
  equivalence(cdata,xdata)
  integer valid_time
  integer mgbf(maxbyt/4),lmgbf,iret1,iret2,iret3
  real(8) volumeid
!!!
  integer(4) nvol,nazm_ref_max,nazm_vel_max,nazm_spe_max
  integer(4) numrefgate_max,numvelgate_max,numspegate_max
  integer(4) nref,nvel,lunout,lundx,jradial,kgate,nret1,nret2,i,j,k,iercbf,radar_elev_above_station
!
  real(8)  radar_lat,radar_lon,station_elev
! real(8) :: rdata(2,nmsgsize),vdata(6,nmsgsize),xdata(18)
  real(8) :: rdata(2,nmsgsize),vdata(3,nmsgsize),xdata(18)
!
  real :: reflectivity(nvol,nazm_ref_max,numrefgate_max),dvelocity(nvol,nazm_vel_max,numrefgate_max), & 
          spectralwidth(nvol,nazm_ref_max,numrefgate_max)

  real :: refl_gate_dis(nvol,nazm_ref_max,numrefgate_max),dvel_gate_dis(nvol,nazm_vel_max,numrefgate_max), & 
          spec_gate_dis(nvol,nazm_spe_max,numrefgate_max)
!
  real :: elevation_angle_refl(nvol,nazm_ref_max),elevation_angle_dvel(nvol,nazm_vel_max), & 
          elevation_angle_spec(nvol,nazm_spe_max)
!
  real :: azimuth_angle_refl(nvol,nazm_ref_max),azimuth_angle_dvel(nvol,nazm_vel_max), & 
          azimuth_angle_spec(nvol,nazm_spe_max)
!
  real :: flag_qcrw_dvel(nvol,nazm_vel_max)
  real :: vcp_dvel(nvol,nazm_vel_max),nyq_dvel(nvol,nazm_vel_max)
!
  real :: year_refl(nvol,nazm_ref_max),year_dvel(nvol,nazm_vel_max),year_spec(nvol,nazm_spe_max)
  real :: month_refl(nvol,nazm_ref_max),month_dvel(nvol,nazm_vel_max),month_spec(nvol,nazm_spe_max)
  real :: day_refl(nvol,nazm_ref_max),day_dvel(nvol,nazm_vel_max),day_spec(nvol,nazm_spe_max)
  real :: hour_refl(nvol,nazm_ref_max),hour_dvel(nvol,nazm_vel_max),hour_spec(nvol,nazm_spe_max)
  real :: minute_refl(nvol,nazm_ref_max),minute_dvel(nvol,nazm_vel_max),minute_spec(nvol,nazm_spe_max)
  real :: second_refl(nvol,nazm_ref_max),second_dvel(nvol,nazm_vel_max),second_spec(nvol,nazm_spe_max)
! 
  data hdrstr1 /'SSTN YEAR MNTH DAYS HOUR MINU SECO CLAT CLON HSMSL HSALG ANAZ ANEL NUL VOID SCID'/
  data hdrstr2_1 /'SSTN YEAR MNTH DAYS HOUR MINU SECO CLAT CLON HSMSL HSALG ANAZ ANEL QCRW VOID'/ 
  data hdrstr2_2 /'SCID HNQV VOCP'/ 
!
  data rptstr1 /'DIST HREF'/
!
! data rptstr2 /'DIST125M DMVR DVSW DRHO  DPHI  DREF'/
  data rptstr2 /'DIST125M DMVR DVSW'/
!
! print*,'radar_lat,radar_lon,radar_elev_above_station,station_elev ',  &
!     radar_lat,radar_lon,radar_elev_above_station,station_elev
!
! print*,'nvol,nazm_ref_max,nazm_vel_max,nazm_spe_max,numrefgate_max,numvelgate_max,numspegate_max ', &
!         nvol,nazm_ref_max,nazm_vel_max,nazm_spe_max,numrefgate_max,numvelgate_max,numspegate_max
!
!  make BUFR output with no embedded tables, and with no output written directly to lunout
!
   call openbf(lunout,'NODX',lundx)
   close(lundx)
!
!  turn off all BUFRLIB printout except abort messages
!
   call openbf(lunout,'QUIET',-1)
!
!  make BUFR messages up to maxbyt bytes long
!
   call maxout(maxbyt)
!
   mbstr = 'level2_' // radar_id(1:4)
!
   do i=1,3
      do j=1,nmsgsize
         vdata(i,j)=10.0e+10
      enddo
   enddo
!
  do ivol=1,nvol           ! Loop for all volume scans begins
!
     cdata=radar_id
     xdata(8)=radar_lat
     xdata(9)=radar_lon
     xdata(10)=station_elev
     xdata(11)=float(radar_elev_above_station)

     xdata(15)=volumeid
     xdata(16)=ivol
!
!
! writing reflectivity part
!
        do jradial=1,nazm_ref_max     ! Loop for all radial lines for reflectivity begins

           nref=0
!
! Find the nonzero gates and its reflectivity values for storing into bufr
!
          if(numrefgate_max .gt. 0) then
            do kgate=1,numrefgate_max  ! Loop for all gates within reflectivity radial line begins
               if(refl_gate_dis(ivol,jradial,kgate).gt.0.0)then
                 nref=nref+1
                 rdata(1,nref)=refl_gate_dis(ivol,jradial,kgate)
                 rdata(2,nref)=reflectivity(ivol,jradial,kgate)
!                 print*,'nref,rdata -refl_gate & refl_valu  ',nref,rdata(1,nref),rdata(2,nref)
               endif
            enddo                     ! Loop for all gates within reflectivity radial line ends
          endif
!
!         print*,'non zero reflectivity gates stored -# of reports ',nref
!
           if ( (nref.gt.0) .and. &
                (hour_refl(ivol,jradial).ge.0) .and. &
                (hour_refl(ivol,jradial).le.23) ) then
!
              xdata(2)=year_refl(ivol,jradial)
              xdata(3)=month_refl(ivol,jradial)
              xdata(4)=day_refl(ivol,jradial)
              xdata(5)=hour_refl(ivol,jradial)
              xdata(6)=minute_refl(ivol,jradial)
              xdata(7)=second_refl(ivol,jradial)
              xdata(12)=azimuth_angle_refl(ivol,jradial)
              xdata(13)=elevation_angle_refl(ivol,jradial)
!
!             print '("xdata stuff  ",a8,15f10.3)',radar_id,  &
!                  (xdata(j),j=2,16) 
!
              valid_time=(int(xdata(2))*1000000)+ &
                         (int(xdata(3))*10000)+ &
                         (int(xdata(4))*100)+(int(xdata(5)))
!
! set the report subtype based on the report hour - see the bufrtab.006
! for the hour windows
!
             subset1(1:6) = 'NC0060'
             WRITE (UNIT=subset1(7:8),FMT='(I2)') int(xdata(5)) + 40
!
              call openmb(lunout,subset1,valid_time)
              call ufbint(lunout,xdata,16,1,nret1,hdrstr1)
!             print*,'nret1 in reflectivity ufbint =  ',nret1
!
! call ufbint to store the reflectivity gate distance and reflectivity values  
!
              call ufbint(lunout,rdata,2,nref,nret1,rptstr1) 
!             print*,'nret1 in reflectivity ufbint =  ',nret1
!
              call writsb(lunout,mgbf,lmgbf)
!	      if (lmgbf.gt.0) call send2dbn(mbstr,lmbstrm1,mgbf,lmgbf,iret1)
           endif
!
        enddo                        ! Loop for all radial lines for reflectivity ends
!
! writing velocity and spectral width
!
        do jradial=1,nazm_vel_max   ! Loop for all radial lines for doppler velocity & spectral width  begins
            nvel=0
!
! write radial line data only for the accepted data from Shun's QC package
!
         if ( (flag_qcrw_dvel(ivol,jradial)) .eq. 1.0 )  then
!
! Find the nonzero gates and its doppler velocity & spectral width values for storing into bufr
!
         if ( numvelgate_max .gt. 0 ) then
           do kgate=1,numvelgate_max  ! Loop for all gates within dvelocity & spectralwidth  radial line begins 
              if( (dvel_gate_dis(ivol,jradial,kgate).gt.0.0).or.(spec_gate_dis(ivol,jradial,kgate).gt.0.0) )then
                 nvel=nvel+1
                 vdata(1,nvel)=dvel_gate_dis(ivol,jradial,kgate)
                 vdata(2,nvel)=dvelocity(ivol,jradial,kgate)
                 vdata(3,nvel)=spectralwidth(ivol,jradial,kgate)
!                vdata(4,nvel)=strct_refvel(4,ivol)%field(kgate,jradial)
!                vdata(5,nvel)=strct_refvel(5,ivol)%field(kgate,jradial)
!                vdata(6,nvel)=strct_refvel(6,ivol)%field(kgate,jradial)

!
!                 print*,'nvel,vdata -dvel_gate,dvel_valu & spec_val  ',nvel, &
!                 vdata(1,nvel),vdata(2,nvel),vdata(3,nvel)
!
              endif
           enddo  ! Loop for all gates within dvelocity & spectralwidth  radial line ends
         end if 
!
!           print*,'non zero doppler velocity & spectral width gates stored -# of reports ',nvel
!
! call ufbint to store the dvelocity/spectral width gate distance and dvelocity and 
! spectral width values for each radial
!
           if( ( nvel .gt. 0 ) .and. &
               (hour_dvel(ivol,jradial).ge.0) .and. &
               (hour_dvel(ivol,jradial).le.23) ) then
!
              xdata(2)=year_dvel(ivol,jradial)
              xdata(3)=month_dvel(ivol,jradial)
              xdata(4)=day_dvel(ivol,jradial)
              xdata(5)=hour_dvel(ivol,jradial)
              xdata(6)=minute_dvel(ivol,jradial)
              xdata(7)=second_dvel(ivol,jradial)
              xdata(12)=azimuth_angle_dvel(ivol,jradial)
              xdata(13)=elevation_angle_dvel(ivol,jradial)
              xdata(14)=flag_qcrw_dvel(ivol,jradial)
              xdata(17)=nyq_dvel(ivol,jradial)
              xdata(18)=vcp_dvel(ivol,jradial)
!           print '("xdata stuff  ",a8,15f10.3)',radar_id, &
!                   (xdata(j),j=2,18) 
!
              valid_time=(int(xdata(2))*1000000)+ &
                         (int(xdata(3))*10000)+ &
                         (int(xdata(4))*100)+(int(xdata(5)))
!
! set the report subtype based on the report hour - see the bufrtab.006  
! for the hour windows
! 
             subset2(1:6) = 'NC0060'
             WRITE (UNIT=subset2(7:8),FMT='(I2)') int(xdata(5)) + 10
!
             call openmb(lunout,subset2,valid_time)
             call ufbint(lunout,xdata,15,1,nret2,hdrstr2_1)
             call ufbint(lunout,xdata(16:18),3,1,nret2,hdrstr2_2)
!            print*,'nret2 in velocity ufbint =  ',nret2
!            call ufbint(lunout,vdata,6,nvel,nret2,rptstr2) 
             call ufbint(lunout,vdata,3,nvel,nret2,rptstr2) 
!            print*,'nret2 in velocity ufbint =  ',nret2
!
             call writsb(lunout,mgbf,lmgbf)
!	     if (lmgbf.gt.0) call send2dbn(mbstr,lmbstrm1,mgbf,lmgbf,iret1)
           end if 
!
!
        endif         ! if loop for QC flag
!
        enddo         ! Loop for all radial lines for doppler velocity & spectral width  ends
!
      enddo           ! Loop for all volume scans ends
!
!     make sure all BUFR output is flushed to disk
!
	call writsb(lunout,mgbf,lmgbf)
      call closbf(lunout)
	      
      return
      end subroutine bufrout
