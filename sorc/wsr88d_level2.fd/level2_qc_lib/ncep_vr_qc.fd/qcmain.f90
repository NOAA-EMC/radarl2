!===========================================================================
!Purpose:
!  read LevelII radar data and quality control
!
!author:Shun Liu  
!time  :09/15/2003 
!input: total elevation (vr & rf) number in a volum scan   
!       vr and rf observation
!output: vr and rf
!
!revision:
!	the qc initial values is given based on statistical analysis for 
!       2-month radar observation. 
!       author:Shun Liu  
!       time  :12/15/2003 

!	AP detection algorithem developed by Liping Liu is added
!       author:Shun Liu  
!       time  :06/01/2003 

!	Velocity spectral width data is added
!       author:Shun Liu  
!       time  :06/01/2003 

!	Set missing data with respect to the real size of matrix
!       that is read from the radar data structure
!       author:Shun Liu  
!       time  :08/20/2004

!       Add comment on how to set model background in dealiasing code 
!       (./src/src0_new/dealias_volume.f90, line 910 to 916)
!       author:Shun Liu  
!       time  :08/20/2004

!       fix bug when calling AP detection 
!       author:Shun Liu  
!       time  :09/01/2004

!       rearrange the structure of package for NCEP   
!       author:Shun Liu  
!       time  :25/05/2005

!       give qcflag to show why the tilt is rejected   
!       author:Shun Liu  
!       time  :18/08/2005

!       update std_dev.f 
!       author:Shun Liu  
!       time  :23/08/2005

!       let VDC depend on elevation angle 
!       author:Shun Liu  
!       time  :24/08/2005

!       add Simple bird migration control 
!       author:Shun Liu  
!       time  :25/08/2005

!       adjust do-loop structure of QC_STAT part
!       author:Shun Liu  
!       time  :21/09/2005

!       Replace the old dealiasing algorithm with a new one and simplify 
!       the I/O to the new dealiasing subroutine that is compiled as a lib.
!       Author: Pengfei Zhang
!       Time:   3/29/2007

!       update to new dealiasing algorithm from Zhang
!       Author: Shun Liu
!       Time: 04/10/2007 

!       use nelv_vel and nelv_sw
!       Author: Shun Liu
!       Time: 08/25/2007 

!       clean clear-air echo
!       Author: Shun Liu & Pengfei Zhang
!       Time: 09/13/2007 

!       add init_qcpara subroutine 
!       Author: Shun Liu 
!       Time: 03/12/2008 

!       modify qcstandard_sn_minus_9pt for VCP 11 
!       Author: Shun Liu 
!       Time: 03/13/2008 

!       merge new dealiasing codes from Kang Nai to this version
!       Author: Shun Liu 
!       Time: 06/12/2008 

!       add vad header 
!       Author: Shun Liu 
!       Time: 06/12/2008 

!       move clear air check to qc check loop 
!       Author: Shun Liu 
!       Time: 04/12/2012

!       move VAD wind dump to the end of QC main 
!       Author: Shun Liu 
!       Time: 04/22/2012

!       use dual-pol variables in QC 
!       Author: Shun Liu 
!       Time: 07/22/2012

!       adjust ref QC to rely on dual-pol QC 
!       Author: Shun Liu 
!       Time: 04/02/2013
!===========================================================================
module sdata
   integer, parameter :: nazim=760,ngate=1000 
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
     real ::  gateWidth(nazim)
     real ::  elev(nazim)
     real ::  azim(nazim)
     real ::  field(ngate,nazim)   
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
     character(len=20) :: vad
!    character(len=20) :: bird
     integer :: bird
     integer :: vad_fail
     integer :: value
   end type qcpara

   type mem_para
     real :: MDZ(4)
     real :: TDBZ(4)
     real :: TDBPHI(4)
     real :: MDRHO(4)
     real :: MDZDR(4)
     real :: MDPHI(4)
   end type mem_para

   type wt_para
     real :: wt_MDZ
     real :: wt_TDBZ
     real :: wt_TDBPHI
     real :: wt_MDRHO
     real :: wt_MDZDR
     real :: wt_MDPHI
   end type wt_para

    logical :: dual_index_qc

!  type mem_para_ap
!    real:: MDREF(3)
!    real:: MDVE(3)
!    real:: MDSW(3)
!    real:: SDVE(3)
!    real:: SDSW(3)
!    real:: TDBZ(3)
!    real:: SPIN(3)
!    real:: SIGN(3)
!    real:: GDZ(3)
!  end type mem_para_ap
end module sdata

module vadhead
     real(8) ::  vadlat
     real(8) ::  vadlon
     real(8) ::  vadhgt
     character(80) vadstaid
!    integer :: obs_yyyy, obs_mm, obs_dd, obs_hh, obs_mn
     integer :: nvad
     integer, parameter :: VAD_MNEMONICS = 15
     integer, parameter :: VAD_LEVELS=255 !maximum records for a station
     real(8), dimension(VAD_LEVELS):: vad_height, vad_uwind, vad_vwind
     integer, dimension(VAD_LEVELS):: vad_qcflag
end module vadhead

!===========================================================================
      subroutine QCmain(nelv_ref,nelv_vel,nelv_sw,strct_in_ref,strct_in_vel, &
         strct_in_sw,strct_in_cc,strct_in_zdr,strct_in_kdp,infile,markflag,dual_in) 

      use sdata
      use vadhead
      use variable_define

      implicit none
      type(ccout_strct) :: strct_in 
      type(ccout_strct),dimension(nelv_ref) :: strct_in_ref,strct_in_vel,strct_in_sw 
      type(ccout_strct),dimension(nelv_ref) :: strct_in_cc,strct_in_zdr,strct_in_kdp 
      type(qcpara),dimension(:),allocatable :: qccontr 
      type(qcpara),dimension(:),allocatable :: qcstandard 
      logical:: dual_in

      integer :: length
      real    :: pct(nazim,ngate)
      integer :: nelv_ref, nelv_vel,nelv_sw
      integer :: i,j,istat,ipt,jpt
      !real,parameter:: missing_data=-99900.0
      real,parameter:: missing_data=999.0
      real :: elev1, elev2, coffvdc
      integer    :: birdrange_vel,birdrange_ref, clurange_ref

      real mbsh_out(ngate,367)
      integer:: markflag(nazim,nelv_ref)

      integer:: ngate_tmp,nazim_tmp
      real avg
      integer tnum
      integer k,kk,na,nb,ii1
      integer na_ref,ng_ref,na_vel,ng_vel
      integer,dimension(:),allocatable :: vad_fail(:)
      character*80 radfname
      character*20 alphabet
      character*8 dual_pol_mark

!*******variables used in ground and sea clutters
      integer mazim_ref(367),mazim_vr(367),nazim_ref(367)
      real mbsh_ap(ngate,367), mbsh_sc(ngate,367), mbsh_pre(ngate,367)
      real idtype(ngate,367)
      real tdbz(ngate,367),sign(ngate,367),spin(ngate,367),sdevv(ngate,367)
      real m_fldve(ngate,367),gdz(ngate,367),sw(ngate,367),sdsw(ngate,367)
!   real m_fldve(ngate,367),gdz(ngate,367),sw(ngate,367),ref(ngate,367),sdsw(ngate,367)
      integer id,land(ngate,367)
      real wmax,aa,dat(920,360)

!***** decide the radar position
      real:: radar_lat,radar_lon,radar_elv
      integer :: radar_number
      character(len=4),dimension(200) :: radnm
      character(len=79) :: tmpchar1
      character(len=16) :: tmpchar2
      character(len=4)  :: tmpchar3
      integer :: i1lat,i2lat,i3lat,i1lon,i2lon,i3lon,i1elv,kradar
!***** VCP mode=121; 221 index
      logical :: extro_job
      character(len=80) :: name1,name2
      integer :: ii
      character(len=80) infile
!******************************
!***** parameters for reflectivity profile
      integer,parameter:: nprfl_pbl=30
      real:: avgref(nprfl_pbl),avgref1(nprfl_pbl),avgref8(8,nprfl_pbl)
      real:: avgazim(8,nprfl_pbl)
      real:: one_eighth,lastazim(8),pblh(nprfl_pbl)
      real:: mixld    !mixed-layer depths
      real:: diffref,tmpref,tmpmaxref
      integer:: maxnum(1),minnum(1),imaxprfl,iminprfl_pbl,ifirstprfl,ilastprfl
      integer:: tmpnum,catalognum(8),catalog,icata
      logical:: mask(nprfl_pbl)
!***** end parameters for reflectivity profile

!********* VAD wind dump***********
      integer:: vad_dump !>0 dump vad, =0
      real :: vad_speed
!********* end VAD wind dump***********

!********* Extra control for ref***********
      integer:: ppi_j, ppi_i, this_refmark
      integer:: ppi_jmin, ppi_imin
      integer:: ppi_jmax, ppi_imax
!********* END extra control for ref***********
!
! IBM function rtc to check the cpu time

      real(8) rtc,t1,t2,t1_stat, t2_stat,volumeid

      character(len=40) :: str_tmp,str_tmp1
      character(len=8),dimension(12) :: qcflag

      external qc_single
      external convert2
      external convert4
      external dealiase_main

!*********pbl2bufr*******************************
      integer, parameter :: MAX_MNEMONICS = 15
      integer, parameter :: MAX_LEVELS=255 !maximum records for a station
      integer :: lunout, lundx
      character(len=4)   :: yy,mm,dd,hh,minute,radnmpbl
      integer :: obs_yyyy, obs_mm, obs_dd, obs_hh, obs_mn
      real(8), dimension(MAX_LEVELS):: pbl_hgt, angl
      real(8) :: vadlat8,vadlon8,vadhgt8
      character(len=8)   :: radnm8
      integer :: num_records !number of data records for a single station
!*********end pbl2bufr*******************************

!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!    excuting code
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

      dual_index_qc=dual_in

      vadstaid=infile
      nvad=0
      vadlat=0.0;vadlon=0.0;vadhgt=0.0
      vadlat=strct_in_vel(1)%radlat
      vadlon=strct_in_vel(1)%radlon
      vadhgt=strct_in_vel(1)%radhgt

!**********START pbl2bufr and vad2bufr header****************
      radnm8=vadstaid(1:4)
      radid=vadstaid(1:4)
      vadlat8=vadlat
      vadlon8=vadlon
      vadhgt8=vadhgt
      yy=vadstaid(6:9)
      mm=vadstaid(10:11)
      dd=vadstaid(12:13)
      hh=vadstaid(14:15)
      minute=vadstaid(16:17)
      lunout=61
      lundx=62
      read(yy,*)obs_yyyy
      read(mm,*)obs_mm
      read(dd,*)obs_dd
      read(hh,*)obs_hh
      read(minute,*)obs_mn
      pbl_hgt=9999.0
      angl=9999.0
      num_records=0
!**********END pbl2bufr and vad2bufr header****************


      print*,'nelv_ref=',nelv_ref
      allocate(qccontr(nelv_vel),qcstandard(nelv_vel))
      allocate(vad_fail(nelv_vel+2))

      do i=1,nelv_vel
        where(abs(strct_in_ref(i)%field)>100)strct_in_ref(i)%field=missing_data
        where(abs(strct_in_vel(i)%field)>100)strct_in_vel(i)%field=missing_data
        where(abs(strct_in_sw(i)%field)>100)strct_in_sw(i)%field=missing_data
!         strct_in_vel(i)%radlat=radar_lat
!         strct_in_vel(i)%radlon=radar_lon
!         strct_in_vel(i)%radhgt=radar_elv


        ngate_tmp=strct_in_ref(i)%num_gate+1
        nazim_tmp=strct_in_ref(i)%num_beam+1
        strct_in_ref(i)%field(ngate_tmp:ngate,:)=missing_data
        strct_in_ref(i)%field(:,nazim_tmp:nazim)=missing_data

        ngate_tmp=strct_in_vel(i)%num_gate+1
        nazim_tmp=strct_in_vel(i)%num_beam+1
        strct_in_vel(i)%field(ngate_tmp:ngate,:)=missing_data
        strct_in_vel(i)%field(:,nazim_tmp:nazim)=missing_data
        strct_in_sw(i)%field(ngate_tmp:ngate,:)=missing_data
        strct_in_sw(i)%field(:,nazim_tmp:nazim)=missing_data
      end do

!     do j=1,strct_in_vel(2)%num_beam
!        write(*,*)j,strct_in_vel(2)%azim(j),strct_in_vel(2)%field(9,j)
!     end do
!     stop

      do k =1,nelv_vel
            ii=int(strct_in_ref(k)%elev_angle*100.0 )
!           call dumpvel(k,ii,infile,nelv_vel,strct_in_vel)
!           call dumpref(k,ii,infile,nelv_ref,strct_in_ref)

            if(ii>300.and.ii<600.and. &
                (strct_in_ref(k)%vcpnum==31.or. &
                strct_in_ref(k)%vcpnum==32))then
                na=strct_in_ref(k)%num_beam
                nb=strct_in_ref(k)%num_gate
                avgref=0.0
                avgref1=0.0
                avgref8=0.0
                avgazim=0.0

                do i=1,nprfl_pbl
                tmpnum=0
                catalognum=0
                do j=1,na
                one_eighth=real(na)/8
                catalog=real(j)/one_eighth+1
                if(abs(strct_in_ref(k)%field(i,j))<90.0)then
                  tmpnum=tmpnum+1
                  avgref1(i)=avgref1(i)+strct_in_ref(k)%field(i,j)
                  avgref(i)=avgref(i)+10.0**(strct_in_ref(k)%field(i,j)*0.1)

                  catalognum(catalog)=catalognum(catalog)+1
                  if(catalognum(catalog)==1)then
                     lastazim(catalog)=strct_in_ref(k)%azim(j)
!                    print*,lastazim(catalog)
                  end if
                  !if(catalog==1)print*,strct_in_ref(k)%azim(j)+360.0
                  if(lastazim(catalog)-strct_in_ref(k)%azim(j)<-180.0) then
                  avgazim(catalog,i)=avgazim(catalog,i)+strct_in_ref(k)%azim(j)-360.0
                  else if(lastazim(catalog)-strct_in_ref(k)%azim(j)>180.0) then
                  avgazim(catalog,i)=avgazim(catalog,i)+strct_in_ref(k)%azim(j)+360.0
                  else
                  avgazim(catalog,i)=avgazim(catalog,i)+strct_in_ref(k)%azim(j)
                  end if
                  avgref8(catalog,i)=avgref8(catalog,i)+10.0**(strct_in_ref(k)%field(i,j)*0.1)
                end if
                end do
                if(tmpnum>200) then
                        avgref1(i)=avgref1(i)/tmpnum
                        avgref(i)=avgref(i)/tmpnum
                        avgref(i)=10.0*log10(avgref(i))
                else
                        avgref1(i)=999.0
                        avgref(i)=999.0
                end if
                do icata=1,8
                if(catalognum(icata)>30) then
                        avgref8(icata,i)=avgref8(icata,i)/catalognum(icata)
                        avgref8(icata,i)=10.0*log10(avgref8(icata,i))
                        avgazim(icata,i)=avgazim(icata,i)/catalognum(icata)
                else
                        avgref8(icata,i)=999.0
                        avgazim(icata,i)=0.0
                end if
                end do
                end do

                do i=1,nprfl_pbl
                   write(*,'(8f8.2)')(avgazim(icata,i),icata=1,8)
                   pblh(i)=sin(real(ii)/100.0*3.14159/180.0)*real(i)*1000.0
                end do

                mask=.false.
                minnum=0;maxnum=0
                where(avgref<100.0)mask=.true.
                mask(1:4)=.false.
                mask(26:30)=.false.
                do i=1,nprfl_pbl
                  if(avgref(i)/=999.0.and.mask(i))ifirstprfl=i
                  exit
                end do
                maxnum=maxloc(avgref,mask=mask)
                minnum=minloc(avgref,mask=mask)
                imaxprfl=maxnum(1);iminprfl_pbl=minnum(1)
                print*,'avgref::',avgref
                print*,'imax,imin,ifirst::',imaxprfl,iminprfl_pbl,ifirstprfl
                mixld=9999.0
                if(imaxprfl==0.or.iminprfl_pbl==0.or.iminprfl_pbl==imaxprfl)then
                  mixld=9999.0
                  print*,'mixld is set to 9999.0 due to  &
                                 & no max or min value'
                else if(abs(imaxprfl-iminprfl_pbl)<2) then
                  mixld=9999.0
                  print*,'mixld is set to 9999.0 due to &
                             &  max and min value too close in height'
                else
                  diffref=avgref(imaxprfl)-avgref(iminprfl_pbl)
                  if(diffref>8.0)then 
!                   if(pblh(imaxprfl)>pblh(imaxprfl-1).and.pblh(imaxprfl)>pblh(imaxprfl+1))then
                      mixld=pblh(imaxprfl)
!                   end if
                    tmpmaxref=-100.0
                    tmpnum=0
                    tmpref=0.0
                    do icata=1,8
                       if(avgref8(icata,imaxprfl)/=999.0)then
                         tmpnum=tmpnum+1
                         tmpref=tmpref+avgref8(icata,imaxprfl)
                         if(tmpmaxref<avgref8(icata,imaxprfl))tmpmaxref=avgref8(icata,imaxprfl)
                       end if
                    end do
                    tmpref=tmpmaxref-tmpref/7.0
                    if(tmpref>10.0)then
                      mixld=9999.0
                      print*,'max reflectivity in the layer and average::',tmpref,tmpmaxref,avgref8(1:8,imaxprfl)
                      print*,'mixld is set to 9999.0 &
                      & due to anisotropic distribution of avgref in the circle'
                    end if
                  else
                    print*,'mixld is set to 9999.0 &
                      & due to max and min value too close in magnitude'
                  end if
                end if
                print*,'mixed-layer height::',mixld,imaxprfl,maxnum(1),pblh(imaxprfl)

              write(name1,101) ii
              name2=infile(1:17)//'_prfl'//name1(1:8)
              if(1==0)then
              open(31,file=name2,form='formatted')
                write(31,'(a4)') strct_in_ref(k)%radar_name
                write(31,'(i8)') strct_in_ref(k)%vcpnum
                write(31,'(6i8)') strct_in_ref(k)%year                 &
                                 ,strct_in_ref(k)%month                &
                                 ,strct_in_ref(k)%day                  &
                                 ,strct_in_ref(k)%hour                 &
                                 ,strct_in_ref(k)%minute               &
                                 ,strct_in_ref(k)%second
                write(31,'(f8.3)') strct_in_ref(k)%elev_angle
                write(31,'(2f10.3,f10.1)') strct_in_ref(k)%radlat      &
                                          ,strct_in_ref(k)%radlon      &
                                          ,strct_in_ref(k)%radhgt
                 lastazim(1)=360.0
                 do icata=1,8
                   tmpnum=0
                   print*,'avgzim::',icata,avgazim(icata,10)
                 do j=4,nprfl_pbl-10
                   if(avgazim(icata,j)>0)then
                           tmpnum=tmpnum+1
                           avgazim(icata,1)=avgazim(icata,1)+avgazim(icata,j)
                   end if
                 end do

                 avgazim(icata,1)=avgazim(icata,1)/tmpnum
                 end do
                 write(31,'(i8.4,10f8.2)')i,lastazim(1),lastazim(1),(avgazim(icata,1),icata=1,8)
                 do i=1,nprfl_pbl
                   write(31,'(i8,11f8.2)')i,avgref(i),avgref1(i),(avgref8(icata,i),icata=1,8),pblh(i)
                 end do
                write(31,*)'mixed-layer height::'
                write(31,'(2f8.2,3x,a12,3x,a4)')strct_in_ref(k)%elev_angle,mixld,infile(6:17),infile(1:4)
              close(31)
              end if

              if(mixld/=9999.0)then
              num_records=num_records+1
              pbl_hgt(num_records)=mixld
              angl(num_records)=strct_in_ref(k)%elev_angle
              write(*,*)'in pbl k=',k, mixld, pbl_hgt(1),ii
              else
              write(*,*)'in pbl k=',k, mixld, 'no pbl value'
              end if


              name2=infile(1:17)//'_refl'//name1(1:8)


!             call  dumpref(k,ii,infile,nelv_ref,strct_in_vel)
              ii1=int(strct_in_vel(k)%elev_angle*100.0 )
!             if(ii1>3000.and.ii1<6000)then
!                  call  dumpvel(k,ii1,infile,nelv_ref,strct_in_vel)
!             end if

          end if
       enddo

       write(*,*)'lunout::',lunout
       write(*,*)'lundx::',lundx
       write(*,*)'radnm8::',radnm8
       write(*,*)'vadlat8::',vadlat8
       write(*,*)'vadlon8::',vadlon8
       write(*,*)'vadhgt8::',vadhgt8
       write(*,*)'obs_yyyy::',obs_yyyy
       write(*,*)'obs_mm::',obs_mm
       write(*,*)'obs_dd::',obs_dd
       write(*,*)'obs_hh::',obs_hh
       write(*,*)'obs_mn::',obs_mn
       write(*,*)'pbl_hgt::',pbl_hgt(1),mixld,strct_in_ref(5)%vcpnum,k
!      write(*,*)'angl::',angl
       if(num_records>0)then
       num_records=1
       call pbl2_bufr_out(lunout, lundx, radnm8, vadlat8, vadlon8, &
                          vadhgt8, obs_yyyy, obs_mm, obs_dd, obs_hh, &
                          obs_mn, pbl_hgt,angl, num_records)
       end if
101            format(I4.4,'.dat')

      if(1==0)then
!*************Begin the ground and sea clutter  detecting **************************
!land(i,j) is index of land or sea mark for each point in radar coordinate
!and it is input parameter


      land=1

      t1 = rtc()
      do i=1,nelv_vel-2
         !write(*,*)'*****runing ground clutter and sea clutter detecting*****'
!         call var(strct_in_ref(i),strct_in_vel(i),strct_in_sw(i),strct_in_ref(i+1), &
!                  ngate,367,ref,tdbz,sign,spin,gdz,m_fldve,sdevv,sdsw,sw,nazim_ref,mazim_vr)
!         call memfun_ap(ngate,367,tdbz,sign,spin,gdz,m_fldve,sdevv,sw,mbsh_ap)
!         call memfun_sc(ngate,367,tdbz,sign,spin,gdz,m_fldve,sdevv,sw,mbsh_sc)
          !call memfun_pre(ngate,367,tdbz,sign,spin,ref,sdevv,sdsw,mbsh_pre)
          write(*,*)'**************  end of detecting***************'
!       do j=1,367
!       do k=1,800
!         idtype(k,j)=0.0
!         if(land(k,j).eq.0.and.mbsh_sc(k,j).gt.0.5) idtype(k,j)=0.2
!         if(land(k,j).eq.1.and.mbsh_ap(k,j).gt.0.5) idtype(k,j)=0.4
!         if(idtype(k,j).ge.0.2) mbsh_sc(k,j)=1.0
          ! write(60,70)tdbz(k/4,j),sign(k/4,j),spin(k/4,j),gdz(k/4,j),m_fldve(k,j),sdevv(k,j),sw(k,j),ref(k/4,j),sdsw(k,j)
!       enddo
!       enddo

        !   remove the ground and sea clutter from the raw radar data
!       call setapecho(strct_in_ref(i),strct_in_vel(i),strct_in_sw(i),ngate,367, mazim_vr,nazim_ref,mbsh_sc,"sc")
        write(*,*)"after remove"
      enddo
70    format(1x,9(f8.2))
      t2 = rtc()
      write(*,200)t2-t1,strct_in_vel(1)%vcpnum
200   format('cpu time after AP detection in qcmain ',f15.4,'  vcp',i3)
!*************** end of detection ***************************************
      end if


write(*,*)"scan mode number",strct_in_ref(1)%vcpnum

print*,'unfolding radial velocity...'
t1 = rtc()
if(1==1) then
!     ******** put map parameters ********
      radlat=strct_in_vel(1)%radlat
      radlon=strct_in_vel(1)%radlon
      radelv=strct_in_vel(1)%radhgt
      mapproj = 2
      trulat1 = 30.0
      trulat2 = 60.0
      trulon = radlon
      sclfct = 1.0

!----------output ASCII data for netcdf converter to check Vr before dealiasing------
      alphabet = 'abcdefghijklmnopqrst'

!**********************************************
! print out before dealiasing
!**********************************************
      ii=0
      do k =1,nelv_vel
            ii=int(strct_in_vel(k)%elev_angle*100.0 )
!           if(ii>6000)then
!             call  dumpvel_de(k,ii1,infile,nelv_ref,strct_in_vel)
!           end if
      enddo
!**********************************************
! END print out before dealiasing
!**********************************************

!**********************************************
!      START call dealiasing algorithm
!**********************************************
       call get_k121( nelv_vel,strct_in_vel )
       call convert2( nelv_ref,nelv_vel                              &
                    ,strct_in_ref,strct_in_vel,strct_in_sw           &
                    ,strct_in_cc,strct_in_kdp,strct_in_zdr )
       if(dual_index_qc)then
       no_mark=999.0
       call get_mark
!     ------------------------------------------------------------------
       !* need to add bird identification
      do k=1,nthet
        do j=1,np
        do i=1,nr
!         if ( no_mark(i,j,k)==999.8 ) then
          if ( no_mark(i,j,k)==999.7 .or. no_mark(i,j,k)==999.2) then
!         if ( no_mark(i,j,k)==999.7 .or.   & 
!              no_mark(i,j,k)==999.2 .or.   & 
!              no_mark(i,j,k)==999.8 ) then
            vr_mark(i,j,k)=1.0
!           vel(i,j,k)=spval
!           ref(i,j,k)=spval
            swg(i,j,k)=spval
            rho(i,j,k)=spval
            zdr(i,j,k)=spval
            kdp(i,j,k)=spval
          endif
        enddo
        enddo
      enddo
       end if

       call dealiase_main

!     ------------------------------------------------------------------
!     Put the missing value to the data point which was marked as the
!     no atmospheric sign.
!     ------------------------------------------------------------------

       call convert4( nelv_ref,nelv_vel                              &
                    ,strct_in_ref,strct_in_vel,strct_in_sw           &
                    ,strct_in_cc,strct_in_kdp,strct_in_zdr )

!       if ( .not. no_qualify_job ) then
!       ----------------------------------------------------------------
!       Do extro-tilt dealiase if necessary.(VCP=121;221)
!       ----------------------------------------------------------------
!       extro_job=.false.
!       ----------------------------------------------------------------
!       put nodealiased tilt's data into a work array.
!       ----------------------------------------------------------------
!       call convert2_two( nelv_ref,nelv_vel                         &
!                         ,strct_in_ref,strct_in_vel,extro_job )
!       print*,' '
!       print*,'extro_job==  ',extro_job
!       print*,' '

!       if ( extro_job ) then
!         --------------------------------------------------------------
!         do dealiase job.
!         --------------------------------------------------------------
!         call extr_dealiase_main
!         --------------------------------------------------------------
!         convert backward.
!         --------------------------------------------------------------
!         call convert4_two( nelv_ref,nelv_vel                       &
!                           ,strct_in_ref,strct_in_vel )
!       endif
!       endif

!**********************************************
!      END call dealiasing algorithm
!**********************************************
!     deallocate(vel,ref)

end if

      qccontr(:)%vad_fail=1

t1_stat=rtc()
!* QC
print*,'data QC...'

!* Simple bird detection
!* qccontr(i)%bird=1 pass =0 fail

  elev1=sin(strct_in_vel(1)%elev_angle*2.0*3.14159/180)
do i=1,nelv_vel
  call init_qcpara(qccontr(i))
  print*,'elve_angle=',strct_in_vel(i)%elev_angle
  call qc_single(strct_in_ref(i),strct_in_vel(i),qccontr(i),str_tmp1)

  !* vcp control for incomplete volume scan
      if(qccontr(i)%vcp/=qccontr(1)%vcp) then
       qccontr(i)%vcp=qccontr(1)%vcp
      end if
      if(qccontr(i)%vcp==999.or.qccontr(i)%vcp==0) then
       print*,'vcp at first elv:', qccontr(1)%vcp
       print*,'vcp at this elv:', qccontr(i)%vcp
       print*,'no qualified VCP number in the first elev'
       stop
      end if
  !* end vcp control for incomplete volume scan


  if(strct_in_vel(i)%elev_angle < 0.0) then
    print*, 'Elve_angle is zero?'
    stop
  end if

  !* should use coffvdc=sin(elev1)**2/sin(elev_i)**2, but coffvdc is too small.
  elev2=sin(strct_in_vel(i)%elev_angle*2.0*3.14159/180)
  coffvdc=elev1/elev2

  !* set initial values
  if(qccontr(i)%vcp==11 .or. qccontr(i)%vcp==12.or. &
     qccontr(i)%vcp==211.or. qccontr(i)%vcp==212   )then
     qcstandard(i)%sn     = 17.5
     qcstandard(i)%avg_sd = 2.75
     qcstandard(i)%sn_minus_9pt = 38.0
     qcstandard(i)%refavg = 11.0
     qcstandard(i)%covrate = 10.0*coffvdc
     qcstandard(i)%intmax_azim=100
     qcstandard(i)%con_jump_azim=20
     qcstandard(i)%intmax_gate=50
     qcstandard(i)%con_jump_gate=30
  else if(qccontr(i)%vcp==21 .or. qccontr(i)%vcp==121.or. &
          qccontr(i)%vcp==221 )then
     qcstandard(i)%avg_sd = 2.5
     qcstandard(i)%sn     = 20.0
     qcstandard(i)%sn_minus_9pt = 37.5
     qcstandard(i)%refavg = 8.0
     qcstandard(i)%covrate = 10.0*coffvdc
     qcstandard(i)%intmax_azim=100
     qcstandard(i)%con_jump_azim=20
     qcstandard(i)%intmax_gate=50
     qcstandard(i)%con_jump_gate=30
  else if(qccontr(i)%vcp==31.or.qccontr(i)%vcp==32.or. &
          qccontr(i)%vcp==35.or. qccontr(i)%vcp==215 .or. &  
          qccontr(i)%vcp==231.or. qccontr(i)%vcp==232  )then
     qcstandard(i)%avg_sd = 2.5
     qcstandard(i)%sn     = 20.0
     qcstandard(i)%sn_minus_9pt = 36.0
     qcstandard(i)%refavg = 6.5
     qcstandard(i)%covrate = 10.0*coffvdc
     qcstandard(i)%intmax_azim=100
     qcstandard(i)%con_jump_azim=20
     qcstandard(i)%intmax_gate=50
     qcstandard(i)%con_jump_gate=20
  end if

end do

If(1==1)then
      open(88,file='qcinfor')
      write(88,100)

     markflag=0
     DO i=1,nelv_vel

      if(qccontr(i)%value/=999)then

     !* bird control
      if(                                                             &
      qccontr(i)%sn_minus_9pt>qcstandard(i)%sn_minus_9pt  .and.       &
      qccontr(i)%refavg>qcstandard(i)%refavg              .and.       &
      qccontr(i)%covrate>38.5                                         &
      ) then
 
      qccontr(i)%bird=0
      qccontr(i)%value=2
!     print*,'qcmain avg_sd::', qcstandard(i)%avg_sd>qcstandard(i)%avg_sd, &
!                               qcstandard(i)%sn_minus_9pt, &
!                               qcstandard(i)%refavg, &
!                                qcstandard(i)%covrate
!     print*,'qcmain avg_sd::', qccontr(i)%avg_sd>qcstandard(i)%avg_sd, &
!                               qccontr(i)%sn_minus_9pt, &
!                               qccontr(i)%refavg, &
!                               qccontr(i)%covrate
                      
      end if


      !* statistic qc
      if(qccontr(i)%sn>qcstandard(i)%sn                      .or.       &
       qccontr(i)%avg_sd>qcstandard(i)%avg_sd              .or.       &
       qccontr(i)%sn_minus_9pt>qcstandard(i)%sn_minus_9pt  .or.       &
       qccontr(i)%refavg<-10.0              .or.       &
       qccontr(i)%covrate<qcstandard(i)%covrate .or.   &

       qccontr(i)%intmax_azim>qcstandard(i)%intmax_azim    .or.       &
       qccontr(i)%con_jump_azim>qcstandard(i)%con_jump_azim.or.       &
       qccontr(i)%intmax_gate>qcstandard(i)%intmax_gate    .or.       &
       qccontr(i)%con_jump_gate>qcstandard(i)%con_jump_gate) then 

!      strct_in_vel(i)%field=missing_data
       qccontr(i)%mark='reject' 
       qccontr(i)%value=3 
      else
       markflag(1:strct_in_vel(i)%num_beam,i)=1
       do j=1,strct_in_vel(i)%num_beam
        if(minval(abs(strct_in_vel(i)%field))>100.0) then
         markflag(j,i)=0 
        end if
       end do
      end if 

      !* Clean up clear-air echo from reflectivity field
       if(qccontr(i)%vcp==31 .or. qccontr(i)%vcp==32 .or. &
      qccontr(i)%vcp==35 .or. qccontr(i)%vcp==215 .or. &
         qccontr(i)%vcp==231 .or. qccontr(i)%vcp==232) then
         if( qccontr(i)%avg_sd>0.7.and.qccontr(i)%sn_minus_9pt>38.5) then
         qccontr(i)%mark='reject' 
         qccontr(i)%value=4 
         end if
       end if

       !* exclude smooth case
!      if(qccontr(i)%covrate<qcstandard(i)%covrate)then 
        if(qccontr(i)%sn_minus_9pt<5.0) then
          qccontr(i)%value=31
        end if
!      end if

       end if


       dual_pol_mark='  criteria'
       write(88,'(f6.2,i6,5f8.2,4i6,5x,a10)')                      &
                      strct_in_vel(i)%elev_angle,i,qcstandard(i)%avg_sd,       &
                      qcstandard(i)%sn,qcstandard(i)%refavg,                   &
                      qcstandard(i)%sn_minus_9pt,qcstandard(i)%covrate,        &
                      qcstandard(i)%intmax_azim,qcstandard(i)%con_jump_azim,   &
                      qcstandard(i)%intmax_gate,qcstandard(i)%con_jump_gate,   &
                      dual_pol_mark
       dual_pol_mark='  regular'
       if(dual_index_qc)dual_pol_mark='dual-pol'
       write(88,'(f6.2,i6,5f8.2,4i6,i6,3x,i4,i5,3x,a8,i2,a10)')                   &
                      strct_in_vel(i)%elev_angle,i,qccontr(i)%avg_sd,          &
                      qccontr(i)%sn,qccontr(i)%refavg,                         &
                      qccontr(i)%sn_minus_9pt,qccontr(i)%covrate,              &
                      qccontr(i)%intmax_azim,qccontr(i)%con_jump_azim,         &
                      qccontr(i)%intmax_gate,qccontr(i)%con_jump_gate,         &
                      qccontr(i)%vad_fail,qccontr(i)%bird,                     &
                      qccontr(i)%vcp,qccontr(i)%mark,qccontr(i)%value,          &
                      dual_pol_mark
     END DO
       close(88)
100 format('  elev  nlev     std     sn     AVZ     PSC    VDC    jump   cjump  gjp  c_gjp  vad   bird  vcp')
end if

t2_stat=rtc()
write(*,600)t2_stat-t1_stat,strct_in_vel(1)%vcpnum
 600 format('cpu time after qc_stat in qcmain ',f15.4,'  vcp',i3)

!--------------------------------------------------------------------------
!     QC based on dual_pol varialbes
!--------------------------------------------------------------------------
       write(*,*)'Start dual-pol based QC'
!      call dpqc( nelv_ref,nelv_vel                         &
!                     ,strct_in_ref,strct_in_vel                 &
!                     ,strct_in_cc,strct_in_zdr, strct_in_kdp )
       DO k=1,nelv_vel

          na_vel=strct_in_vel(k)%num_beam
          ng_vel=strct_in_vel(k)%num_gate
          elev2=sin(strct_in_vel(k)%elev_angle*3.14159/180.0)
          birdrange_ref=int(5.0/elev2)*0.4
          birdrange_vel=birdrange_ref*4
          clurange_ref=int(0.10/elev2)  ! 0.1km/ele
          write(*,*)'qc_main::', birdrange_ref,birdrange_vel
          do j=1,na_vel
          do i=1,ng_vel
!            strct_in_vel(k)%field(i,j)=vel(i,j,k)
             if(vr_mark(i,j,k)==1.0) strct_in_vel(k)%field(i,j)=spval
             if(qccontr(1)%bird==0.and. i<birdrange_vel) then
                strct_in_vel(k)%field(i,j)=missing_data
             end if
          end do
          end do
          if(qccontr(k)%mark=='reject') strct_in_vel(k)%field=missing_data

          na_ref=strct_in_ref(k)%num_beam
          ng_ref=strct_in_ref(k)%num_gate
          do j=1,na_ref
          do i=1,ng_ref
             if(j<=na_vel)then
             ii=(i-1)*4+1
             if(ii<=ng_vel-5)then
               strct_in_ref(k)%field(i,j)=ref(ii,j,k)
             end if
             end if
             if(qccontr(1)%bird==0.and.i<birdrange_ref) then
 !              write(*,*)'bird height control:: ref',i,j,qccontr(1)%bird
                strct_in_ref(k)%field(i,j)=missing_data
             end if

             ppi_jmin=j-3; ppi_jmax=j+3
             if(ppi_jmin<1) ppi_jmin=1
             if(ppi_jmax>na_ref) ppi_jmax=na_ref
             ppi_imin=i-3; ppi_imax=i+3
             if(ppi_imin<1) ppi_imin=1
             if(ppi_imax>na_ref) ppi_imax=ng_ref

             this_refmark=1
!            do ppi_j=ppi_jmin,ppi_jmax
!            do ppi_i=ppi_imin,ppi_imax
!               if(strct_in_ref(k)%field(ppi_i,ppi_j)>18.0.and.i>clurange_ref)this_refmark=0
!            end do
!            end do
             !this_refmark=0 means lager reflectivity + range larger> range circle corresponding to 0.25 km height
             if(rf_mark(i,j,k)==1.and.this_refmark==1) strct_in_ref(k)%field(i,j)=spval
!            if(rf_mark(i,j,k)==1) strct_in_ref(k)%field(i,j)=spval
             if(qccontr(k)%mark=='reject'.and.qccontr(k)%value/=31.and.this_refmark==1.and.qccontr(k)%value/=999) &
                       strct_in_ref(k)%field(i,j)=missing_data
          end do
          end do
!         if(qccontr(k)%mark=='reject'.and.qccontr(k)%value/=31) strct_in_ref(k)%field=missing_data
!         if(qccontr(k)%bird==0) strct_in_ref(k)%field=missing_data

       END do
       write(*,*)'End dual-pol based QC'

t2_stat=rtc()
write(*,601)t2_stat-t1_stat,strct_in_vel(1)%vcpnum
 601 format('cpu time after dual-pol QC in qcmain ',f15.4,'  vcp',i3)
!--------------------------------------------------------------------------
!     END QC based on dual_pol varialbes
!--------------------------------------------------------------------------

      vad_dump=1
      do i=1,nelv_vel
!*    vad_qcflg=3   bird event in QC package
      if(qccontr(i)%bird==0) vad_qcflag=2
      end do

      num_records=nvad

      if(nvad>0) then
      lundx=52
      lunout=51
      do i=1,nvad
        vad_speed=sqrt(vad_uwind(i)**2+vad_vwind(i)**2)
        if(vad_speed>50.0) then
           vad_dump=0
            exit
        end if
        if(vad_height(i)>7.0) then
            num_records=i
            exit
        end if
      end do
      else
         vad_dump=0
      end if

      if(num_records<=0.or.num_records>250)then
         vad_dump=0
      end if

      if(vad_dump>0) then
      write(*,*)'entering vad2bufr'
      call vad2_bufr_out(lunout, lundx, radnm8, vadlat8, vadlon8, &
                          vadhgt8, obs_yyyy, obs_mm, obs_dd, obs_hh, &
                          obs_mn, vad_height, vad_uwind, vad_vwind, vad_qcflag, num_records)
      write(*,*)'out vad2bufr'
      else
      write(*,*)'no vad2bufr dump'
      end if

deallocate(qccontr,qcstandard,no_mark,rf_mark,vr_mark)
deallocate(ref,vel,zdr,kdp,swg)
deallocate(vad_fail)

print*,'Done QC.'
end subroutine QCmain
!===========================================================================
subroutine init_qcpara(qccontr1)
!Purpose:
!  initialize qcpara structure  
!
!author:Shun Liu  
!time  :03/12/2008 

  use sdata
  implicit none

  type(qcpara):: qccontr1 

     qccontr1%intmax_azim=999
     qccontr1%con_jump_azim=999
     qccontr1%intmax_gate=999
     qccontr1%con_jump_gate=999 
     qccontr1%iref=0     
     qccontr1%iref0=0
     qccontr1%iref5=0
     qccontr1%iref35=0
     qccontr1%imaxv=999

     qccontr1%sn_minus_9pt=999.0    
     qccontr1%avg_sd=999.0
     qccontr1%sn=999.0     
     qccontr1%refavg=0.0 
     qccontr1%sn_minus_avg=999.0  
     qccontr1%covrate=999.0

     qccontr1%vcp=999
     qccontr1%mark='accept'
     qccontr1%vad='true'
     qccontr1%bird=1
     qccontr1%vad_fail=1
     qccontr1%value=1

end subroutine init_qcpara
!===========================================================================
