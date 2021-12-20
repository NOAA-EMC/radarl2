Subroutine qc_single(strct_in_ref,strct_in_vel,qccontr,str_tmp1)
!
!#######################################################################
!     MODIFICATION HISTORY:
!
!     05/25/2002 (Shun Liu)
!     compare azim_rf and azim_vr
!     07/02/2002 (Shun Liu)
!     check space obsered data and record the grid position
!     10/20/2002 (Shun Liu)
!     add the VCP number control
!     11/02/2002 (Shun Liu)
!     add the sign change number stastic
!     11/05/2002 (Shun Liu)
!     add the standard deviation stastic
!     09/05/2003 (Shun Liu)
!#######################################################################
     use sdata
     implicit none

     integer, parameter :: nazim_wk=367
     real,    parameter :: spval=999.0
     integer, parameter :: nx=81,ny=81
     real,    parameter :: dx=1500.0,dy=1500.0

     type(ccout_strct) :: strct_in,strct_in_ref,strct_in_vel
     type(qcpara)      :: qccontr
     real              :: gatelenth

     integer :: length
     real    :: pct(nazim,ngate)
     real    :: field_rf(ngate,nazim)   
     real    :: field_rf_tmp(ngate,nazim_wk)   
     real    :: field_vr(ngate,nazim)
     real    :: field_vr_tmp(ngate,nazim_wk)   
     real    :: field_tmp(ngate,nazim)   
     real    :: azim_rf(nazim), azim_vr(nazim),azim_tmp(nazim)
     real*8  date
     real    :: x,y,th,rd
     real,dimension(nx):: xp
     real,dimension(ny):: yp
     real    :: ath,rdd
     integer :: intath,intrdd
     real,dimension(nx,ny):: xyp

! parameter for quality control
     integer :: vcp_old
     integer :: opt_exit,opt_do,opt
     character(len=40) :: str_tmp,str_tmp1
     character(len=18) :: qcstastic_fn
     character(len=40) :: ncarfn

 !parameter for number sign
     integer :: nrang, numSC
     real    :: prct,prct2,prct3,prct4
     real    :: sn, sn_minus_avg, sn_minus_9pt

 !parameter for standard deviation 
     integer :: npt     !the grid number for medium value
     real    :: med_fld(ngate,nazim)
     real    :: sd_fld(ngate,nazim)
     real    :: fld_prt(ngate,nazim)
     real    :: vel_std,t_sd,max_sd,avg_sd

 !parameter for azimuth control
     integer :: intmax_azim,con_jump_azim
     real    :: nyq_vel

 !parameter for gate control
     integer :: intmax_gate,con_jump_gate

 !parameter for reflectivity average
     real :: refavg
     integer :: iref,iref35,iref5,iref0
     integer  :: imaxv,ivel

 !parameter for w2 dealiasing
     integer,parameter:: num_gates=1600
     real,parameter:: missing_data=-99900.0
     real,dimension(num_gates):: beam_vel
     real:: nnyq,elvation,azimuth(nazim)
     integer:: tilt

 !parameter for tmp
     integer :: i,j,k,im,jm,beam,neff
     logical :: return_flag

!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!    excuting code
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

     inquire(iolength=length) strct_in

!* read reflectivity
        strct_in=strct_in_ref
!* end read reflectivity


!* QC reflectivity average
     iref=0
     iref35=0
     iref5=0
     iref0=0
     refavg=0.0

     do i=1,ngate
     do j=1,nazim_wk
        if( abs(strct_in%field(i,j)) > 100.0 )then
           strct_in%field(i,j)=999.0
        else 
           refavg=refavg+strct_in%field(i,j)
           iref=iref+1
          if(strct_in%field(i,j)>35.0) then
           iref35=iref35+1
          else if(strct_in%field(i,j)>5.0 .and. strct_in%field(i,j)<=35.0)then
           iref5=iref5+1
          end if
        end if
     end do
     end do
     iref0=iref-iref35-iref5
     if(iref>0.0) then
     refavg=refavg/iref
     else
     refavg=999.0
     end if
!* END QC reflectivity average

     if(1==0)then
     open(61,file='ncar.rf')
     write(61,'(15f5.1)')(strct_in%azim(i),i=1,nazim_wk)
     do j=1,nazim_wk
        write(61,'(20f6.1)')(strct_in%field(i,j),i=1,920)
     end do
     close(61)
     end if
!    call system('pltref.x')


!* read radia velocity
     strct_in=strct_in_vel
!* end read radia velocity

!* call w2 dealiasing algorithm
     strct_in%field(921:ngate,:)=missing_data
     strct_in%field(:,368:nazim)=missing_data

     strct_in_vel=strct_in
!* end dealiasing algorithm

     do i=1,100
     if(strct_in%gatewidth(i) > 0.0 .and.                       &
             strct_in%gatewidth(i) < 999.0) then
     gatelenth=strct_in%gatewidth(i)
     end if
     end do
     gatelenth=gatelenth*1000.0

     azim_vr=strct_in%azim
     field_vr=strct_in%field

     return_flag=.true.
     do i=1,ngate
     do j=1,nazim
        if(field_vr(i,j)<-100.0 .or. field_vr(i,j)>500.0)then
           field_vr(i,j)=spval
        else
           return_flag=.false.
        end if
     end do
     end do

     if(return_flag) then  
            qccontr%mark='reject'
            qccontr%value=999
            qccontr%vcp=strct_in%vcpnum
            write(*,*)'in qc_single, return due to no qualified data data ::', qccontr%mark
            return
     end if

     ivel=0
     do i=1,ngate
     do j=1,nazim_wk
        if(field_vr(i,j)<500.0)then
        ivel= ivel+1
        end if
     end do
     end do

!* sort raw radia velocity
     if(1==0)then
     field_tmp=spval
     azim_tmp=spval
     field_vr_tmp(:,1:nazim_wk)=field_vr(:,1:nazim_wk)
     call datasort(nazim_wk,ngate,azim_vr,azim_tmp,field_vr_tmp,field_tmp)
     azim_vr=spval
     azim_vr(1:nazim_wk)=azim_tmp(1:nazim_wk)
     field_vr(:,1:nazim_wk)=field_tmp(:,1:nazim_wk)
     end if
!* sort raw radia velocity

     if(1==0)then
     do i=1,ngate
     do j=1,nazim
        if( abs(field_vr(i,j)) > strct_in%nyq_vel .and. &
                                     field_rf(i,j)<spval )then
           field_vr(i,j)=spval
        end if
     end do
     end do
     end if

!* caculate the data date
!    date=strct_in%year*10000+strct_in%month*100          &
!         +strct_in%day+0.01*real(strct_in%hour)          &
!         +0.0001*real(strct_in%minute)                   &
!         +0.000001*real(strct_in%second)
!    open(61,file='calnum')
!    write(61,'(f14.4)')date
!    close(61)
!* end caculate the data date

   date=0.0
   write(str_tmp1(1:4),'(i4)')strct_in%year
   if(strct_in%month<10)then
   write(str_tmp1(5:5),'(i1)')int(date)
   write(str_tmp1(6:6),'(i1)')strct_in%month
   else
   write(str_tmp1(5:6),'(i2)')strct_in%month
   end if

   if(strct_in%day<10)then
   write(str_tmp1(7:7),'(i1)')int(date)
   write(str_tmp1(8:8),'(i1)')strct_in%day
   else
   write(str_tmp1(7:8),'(i2)')strct_in%day
   end if

   if(strct_in%hour<10)then
   write(str_tmp1(9:9),'(i1)')int(date)
   write(str_tmp1(10:10),'(i1)')strct_in%hour
   else
   write(str_tmp1(9:10),'(i2)')strct_in%hour
   end if

   if(strct_in%minute<10)then
   write(str_tmp1(11:11),'(i1)')int(date)
   write(str_tmp1(12:12),'(i1)')strct_in%minute
   else
   write(str_tmp1(11:12),'(i2)')strct_in%minute
   end if

   if(strct_in%second<10)then
   write(str_tmp1(13:13),'(i1)')int(date)
   write(str_tmp1(14:14),'(i1)')strct_in%second
   else
   write(str_tmp1(13:14),'(i2)')strct_in%second
   end if

   print*,trim(str_tmp1(1:14))

!  write(str_tmp1,'(f15.6)')date
   qcstastic_fn=str_tmp1(1:14)//'.qc'
!     ncarfn='mv wppi.cgm '//str_tmp1(1:14)//'.cgm'
!     call system(ncarfn)

!     ncarfn='mv ppiz.eps '//str_tmp1(1:14)//'.eps'
!     call system(ncarfn)

!     ncarfn='mv ncar.vr '//str_tmp1(1:14)//'.vr'
!     call system(ncarfn)

!     ncarfn='mv ncar.rf '//str_tmp1(1:14)//'.rf'
!     call system(ncarfn)

!  open(66,file=qcstastic_fn,status='unknown')

!* recorder the sign number
   k=1
   nrang=strct_in%num_beam
   beam=1
   call num_sign(field_vr,ngate,nazim_wk,     &
              1,nrang,spval,numSC,prct,prct2)
   !* stastic sign change
   prct=prct*100.0
   sn=prct
   str_tmp=''
   str_tmp(5:40)='sign change stastic:'

   str_tmp=''
   str_tmp(5:40)='bird sign change stastic:'

   call num_signavg(field_vr,ngate,nazim_wk,     &
              1,nrang,spval,numSC,prct,prct2,imaxv)
   prct=prct*100.0
   call num_signavg1(field_vr,ngate,nazim_wk,     &
              1,nrang,spval,numSC,prct3,prct4,imaxv)
   prct3=prct3*100.0
   sn_minus_avg=prct3 
   str_tmp=''
   str_tmp(5:40)='sign change minus avg stastic:'
   str_tmp=''
   str_tmp(5:40)='bird sign change minus avg stastic:'

   str_tmp=''
   str_tmp(5:40)='velocity information imaxv ivel'

   str_tmp=''
   str_tmp(5:40)='reflectivity information'
!* recorder the sign number

!* recorder bad data block
   con_jump_azim=0
   con_jump_gate=0
   nyq_vel= strct_in%nyq_vel
   call azimcontr(field_vr,ngate,nazim_wk,nyq_vel,intmax_azim,con_jump_azim)
   call gatecontr(field_vr,ngate,nazim_wk,nyq_vel,intmax_gate,con_jump_gate)
!     ncarfn='mv jump_location '//str_tmp1(1:15)//'.jump'
!     call system(ncarfn)
   str_tmp=''
   str_tmp(5:40)='azimuth control:'
!* end recorder bad data block

!* recorder the standard deviation of whole field
    npt=9
    nrang=strct_in%num_beam
    call median(npt,field_vr,ngate,nazim_wk, & 
              1,nrang,spval,med_fld)

!   field_vr=med_fld
    call std_dev(npt,field_vr,med_fld,ngate,nazim_wk, &
                1,nrang,spval,sd_fld,fld_prt)
    neff = 0
    t_sd = 0
    max_sd = 0
    do j = 1,nazim_wk
      do i =1, ngate
      if ( sd_fld(i,j) .ne. spval) then
      neff = neff + 1
      t_sd = t_sd + sd_fld(i,j)
        if ( sd_fld(i,j) .gt. max_sd ) then
          max_sd = sd_fld(i,j)
          im= i
          jm = j
         end if
      else
      end if
      end do
    end do
    prct=0.0;prct2=0.0
    call num_sign(fld_prt,ngate,nazim_wk,     &
              1,nrang,spval,numSC,prct,prct2)
    if(prct<100) prct=prct*100.0
    sn_minus_9pt=prct

   if(neff>0)then
    avg_sd = t_sd/float(neff)
   else
    avg_sd=99.0
   end if
   str_tmp=''
   str_tmp(5:40)='standard deviation statistic:'
!* end recorder the standard deviation of whole field

     opt_exit=0
     opt_do=1
     opt=0

   !* stastic vcp number
     str_tmp=''
     str_tmp(5:40)='vcp'
!* vcp number control

!* nyq_velocity control
     if(prct > 15.0)then
       opt=opt_exit
     end if
     
   !* stastic nyq_velocity
     str_tmp=''
     str_tmp(5:40)='nyq_vel'
   !* end nyq_velocity control
   
!    write(1,*)opt
     close(1)

!  close(66)

     qccontr%intmax_azim=intmax_azim
     qccontr%con_jump_azim=con_jump_azim
     qccontr%intmax_gate=intmax_gate
     qccontr%con_jump_gate=con_jump_gate
     qccontr%iref=iref
     qccontr%iref0=iref0
     qccontr%iref35=iref35
     qccontr%imaxv=imaxv
     qccontr%sn_minus_avg=sn_minus_avg

     qccontr%sn=sn
     qccontr%avg_sd=avg_sd
     qccontr%sn_minus_9pt=sn_minus_9pt
     qccontr%refavg=refavg
     qccontr%covrate=real(ivel)/(920*367)*100.0
     print*, qccontr%covrate
     qccontr%vcp=strct_in%vcpnum

End subroutine qc_single
