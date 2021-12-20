!###################################################################################
!PURPOSE
! The subroutine is for generating variables used in ground and sea clutters detection
!#####################################################################################  
!AUTHOR: Liping Liu
!09/10/04
!
!  MODIFICATION HISTORY:
!  Date       Name          Action
!#########################################################
!INPUT
! strct_in_ref,strct_in_vel,strct_in_sw: reflectivity, velocity and width spectrum width in first PPI
! strct_in_ref2: Refelctivity in second PPI
! ng,na: dimension of the input radar data
!OUTPUT
!ref: Reflectivity
!tdbz: texture of reflectivity
!sign: sign change of reflectivity
!spin: spin of reflectivity
!gdz: vertical variation of reflectivity
!m_fldve: median velocity
!sdve: standard diviation of velocity
!sdsw: standard diviation of width spectrum
!sw:   median spectrum
!nazim_ref,mazim_vr:azimuthal transfer for data sort
!#######################################################

Subroutine var(strct_in_ref,strct_in_vel,strct_in_sw,strct_in_ref2,ng,na,ref,tdbz,sign,spin,gdz,m_fldve,sdve,sdsw, sw, &
nazim_ref,mazim_vr)
USE sdata
implicit none
integer, parameter :: nazim_wk=367
integer ng,na,mbsh_num
type(ccout_strct) :: strct_in_ref,strct_in_vel,strct_in_sw,strct_in_ref2
real ref(ng,na),ve(ng,na),sw(ng,na),ref2(ng,na),sdve(ng,na),sdsw(ng,na)
real azim_ref(na),azim_ref2(na),azim_vr(na),azim_tmp(na),dazm
integer mazim_ref(na),mazim_vr(na), mazim_ref2(na),mazim_sw(na)
real  field_vr_tmp(ng,na),tmpmbsh
real m_fldve(ng,na),ddbz,ifact,gdz(ng,na)
real med_fld(ng,na),dbzthresh,spval
integer i,ir,j,jj,jj1,jj2,k,nr,np,mgateref,nrangref,mazimref,numthreshref,iminrgref
integer nptve,mgateve,nrangve,mazimve,iminrgve,numthreshve,title_lev,mbsh_num_ve(ng,na)
integer nazim_ref(na),nazim_ref2(na)
integer num_beamref,num_gateref, num_beamref2,num_gateref2,num_beamvel,num_gatevel
 character(40) char_tmp
 character(1)title_mark
 real tdbz(ng,na),sign(ng,na),spin(ng,na)
!********parameter for variable calculation ******************
!nptve: gate number in  the mediam smooth   
!mgateref and mazimref,mgateve,mazimve: the average number in range and azmith for calculating the TDBZ, SPIN, SIGN and SDEV.
!dbzthresh : the thresh of ref for spin
  
  spval=999.0
  tdbz=spval
  sign=spval
  spin=spval
  gdz=spval
  m_fldve=spval
  sdve=spval
  sw=spval

  nptve=7
  mgateref=3
  mazimref=3
  mgateve=12
  mazimve=3
  dbzthresh=2.0


  numthreshref=(mgateref*mazimref)/2.
  numthreshve=(mgateve*mazimve)/2.

  iminrgref=1+mgateref/2
  nrangref=230
  iminrgve=1+mgateve/2
  nrangve=ng
  
  num_beamref=strct_in_ref%num_beam
  num_gateref=strct_in_ref%num_gate

  num_beamref2=strct_in_ref2%num_beam
  num_gateref2=strct_in_ref2%num_gate

  num_beamvel=strct_in_vel%num_beam
  num_gatevel=strct_in_vel%num_gate
 
  num_beamref=367
!  num_gateref=367

  num_beamref2=367
!  num_gateref2=367

  num_beamvel=367
!  num_gatevel=367



  do j=1,num_beamref
  do i=1,num_gateref
     if(abs(strct_in_ref%field(i,j)).gt.900) strct_in_ref%field(i,j)=spval
  enddo
  enddo
  do j=1,num_beamref2
  do i=1,num_gateref2
     if(abs(strct_in_ref2%field(i,j)).gt.900) strct_in_ref2%field(i,j)=spval
  enddo
  enddo

  do j=1,num_beamvel
  do i=1,num_gatevel
     if(abs(strct_in_vel%field(i,j)).gt.900) strct_in_vel%field(i,j)=spval
     if(abs( strct_in_sw%field(i,j)).gt.900)  strct_in_sw%field(i,j)=spval
  enddo
  enddo
!kumar  write(*,*)"befoe sor"
!***sording the ref data in first and second titles *****************

     azim_tmp=spval
     ref=spval
     azim_vr(1:num_beamref)=strct_in_ref%azim(1:num_beamref)
     field_vr_tmp(1:num_gateref,1:num_beamref)=strct_in_ref%field(1:num_gateref,1:num_beamref)
     call datasort(nazim_wk,ng,num_beamref,num_gateref,azim_vr,azim_tmp,field_vr_tmp,ref,mazim_ref)
     azim_ref(1:num_beamref)=azim_tmp(1:num_beamref)

     azim_tmp=spval
     ref2=spval
     azim_vr(1:num_beamref2)=strct_in_ref2%azim(1:num_beamref2)
     field_vr_tmp(1:num_gateref2,1:num_beamref2)=strct_in_ref2%field(1:num_gateref2,1:num_beamref2)
     call datasort(nazim_wk,ng,num_beamref2,num_gateref2,azim_vr,azim_tmp,field_vr_tmp,ref2,mazim_ref)
     azim_ref2(1:num_beamref2)=azim_tmp(1:num_beamref2)

!     do i=1,num_beamref2
!        write(*,*)azim_ref(i),azim_ref2(i),mazim_ref(i)
!     enddo  

!***sording the vel data*****************
     ve=spval
     azim_tmp=spval
     azim_vr(1:num_beamvel)=strct_in_vel%azim(1:num_beamvel)
     field_vr_tmp(1:num_gatevel,1:num_beamvel)=strct_in_vel%field(1:num_gatevel,1:num_beamvel)
     call datasort(nazim_wk,ng,num_beamvel,num_gatevel,azim_vr,azim_tmp,field_vr_tmp,ve,mazim_vr)
     azim_vr(1:num_beamvel)=azim_tmp(1:num_beamvel)

!kumar    write(*,*)num_beamvel,num_gatevel

!    do i=1,num_beamref2
!       write(16,*)azim_vr(i),mazim_vr(i)
!    enddo



!***sording the sw data*****************
     sw=spval
     azim_tmp=spval
     azim_vr(1:num_beamvel)=strct_in_vel%azim(1:num_beamvel)
     field_vr_tmp(1:num_gatevel,1:num_beamvel)=strct_in_sw%field(1:num_gatevel,1:num_beamvel)
     call datasort(nazim_wk,ng,num_beamvel,num_gatevel,azim_vr,azim_tmp,field_vr_tmp,sw,mazim_vr)
     azim_vr(1:num_beamvel)=azim_tmp(1:num_beamvel)
!kumar     write(*,*)"after sort"

!***match the reflectivity and velocity**********
     do i=1,na
       dazm=999
       do j=i-5,i+5
          jj1=j
          if(jj1.lt.1)jj1=1
          if(jj1.gt.na) jj1=na
          if(abs(azim_vr(i)-azim_ref(jj1)).lt.dazm) then
             dazm=abs(azim_vr(i)-azim_ref(jj1))
             k=jj1
          endif
        enddo
      nazim_ref(i)=k
    enddo
     do i=1,na
       dazm=999
       do j=i-5,i+5
          jj1=j
          if(jj1.lt.1)jj1=1
          if(jj1.gt.na) jj1=na
          if(abs(azim_vr(i)-azim_ref2(jj1)).lt.dazm) then
             dazm=abs(azim_vr(i)-azim_ref2(jj1))
             k=jj1
          endif
        enddo
      nazim_ref2(i)=k
    enddo
!*******processing the data ***********
    k=1
    do i=1,na
      k=i
      if(azim_ref(i).ne.0) goto 5
    enddo
5   continue
    do i=1,k
    do j=1,ng
      ref(j,i)=ref(j,k)
    enddo
    enddo

    k=1
    do i=1,na
      k=i
      if(azim_ref2(i).ne.0) goto 6
    enddo
6   continue
    do i=1,k
    do j=1,ng
      ref2(j,i)=ref2(j,k)
    enddo
    enddo

    k=1
    do i=1,na
      k=i
      if(azim_vr(i).ne.0) goto 15
    enddo
15  continue
    do i=1,k
    do j=1,ng
      ve(j,i)=ve(j,k)
      sw(j,i)=sw(j,k)
    enddo
    enddo
!---calculate the tdbz,spin,sign and its membership--------------------------
     call   dbzpara(ref,m_fldve,ng,na,iminrgref,nrangref,mgateref,mazimref,nazim_ref,spval,dbzthresh,numthreshref,tdbz,sign,spin)
     call   sdt_dev_ve(ve,ng,na,iminrgve,nrangve,mgateve,mazimve,numthreshve,spval,SDVe)
     call   sdt_dev_ve(sw,ng,na,iminrgve,nrangve,mgateve,mazimve,numthreshve,spval,SDsw)
     call   median(nptve,sw,ng,na,iminrgve,nrangve,spval,m_fldve)
     sw=m_fldve
     call   median(nptve,ref,ng,na,iminrgve,nrangve,spval,m_fldve)
     ref=m_fldve
     call   median(nptve,ref2,ng,na,iminrgve,nrangve,spval,m_fldve)
     ref2=m_fldve
     call   median(nptve,ve,ng,na,iminrgve,nrangve,spval,m_fldve)
     do j=1,na
        jj1=nazim_ref(j)
        jj2=nazim_ref2(j)
!kumar        write(*,*)j,jj1,jj2
     do i=2,230
       if(ref(i,jj1).ne.spval) then 
            ddbz=ref2(i,jj2)-ref(i,jj1)
            if(ref2(i,jj2).eq.spval) ddbz=-ref(i,jj1)
            gdz(i,j)=ddbz
!            write(*,*)ddbz
       endif
      enddo
     enddo
!kumar    write(*,*)"end of var"

   if(1==0)then
     write(*,*)"draw the picture"
     open(61,file='input.dat')
     write(61,'(15f5.1)')(azim_vr(i),i=1,nazim_wk)
     do j=1,nazim_wk
        write(61,'(20f6.1)')(ref(i,j),i=1,230)
     end do
     close(61)

!    call system('pltref_c.x')
     char_tmp='convert ppiz.eps  ppiz0_1.jpg'
!    call system(char_tmp)

     open(61,file='ncar.vr')
     write(61,'(15f5.1)')(azim_vr(i),i=1,nazim_wk)
     do j=1,nazim_wk
        write(61,'(20f6.1)')(ve(i,j),i=1,920)
     end do
     close(61)
!    call system('pltv_c.x')
     char_tmp='convert wppi.eps  ppiv0_1.jpg'
!    call system(char_tmp)

     open(61,file='ncar.vr')
     write(61,'(15f5.1)')(azim_vr(i),i=1,nazim_wk)
     do j=1,nazim_wk
        write(61,'(20f6.1)')(sw(i,j),i=1,920)
     end do
     close(61)
!    call system('pltSWPPI_c.x')
     char_tmp='convert wppi.eps  ppiw0_1.jpg'
!    call system(char_tmp)

   end if

  return
  end
  

  subroutine mfun1(para,x,y)
!input para: member function parameter
!input X   : input value 
!output y  : value of member function 

  real  para(3)
  real x,y
  y=0  
  if(x.le.para(1).or.x.ge.para(3)) then 
     y=0
    else if(x.gt.para(1).and.x.le.para(2)) then 
        y=(x-para(1))/(para(2)-para(1))
         else 
            y=(para(3)-x)/(para(3)-para(2))
    endif
   return 
   end 




!c**************************************************************
      subroutine dbzpara(fld,ve,nr,np,iminrg,nrang,mgate,mazim,nazim_ref,spval,dbzthresh,numthresh,tdbz,sign,spin)
!c___________________________________________________
!c
!c    Purpose: calculate the TDBZ,SIGN,SIGN used in ground and sea clutters  detecting
!c    Author: Liping Liu
!c    Date:   April. 8, 2004
!c___________________________________________________

!c___________________________________________________
!c
!c    Input: fld,nr,np  :input the data field and dimension number
!c    Input:iminrg, nrang : Calculating Domain of range
!c    Input: mgate,mazim  : Averaging grid number in radial and azm direction
!c    Input: Spval:    bad value
!c    output: Tdbz(nr,np),spin(nr,np),signn(nr,np): the value at echo grid

!c___________________________________________________

       implicit none 
       integer  nr,np,mgate,mazim,numthresh,mgate_hf,mazim_hf
       real fld(nr,np),ve(nr,np),spval,dbzthresh,mbsh(nr,np),Ptdbz(3),Pspin(3),Psign(3),ttdbz,sspin,diff,mbsh_s
       integer i,j,jj,m,ma,mr,iminrg,nrang,num,nazim_ref(np)
       real nsign,tdbz(nr,np),sign(nr,np),spin(nr,np)
       
!c-------------------------------------------------------
       mgate_hf=(mgate-1)/2
       mazim_hf=(mazim-1)/2
       if(iminrg-mgate_hf.lt.1.or.iminrg+mgate_hf.gt.np) then
         write(*,*)" error iminrg or nrang in dbzpara"
       return
       endif
       do jj=1,np
          j=nazim_ref(jj)
!          j=jj
       do i=iminrg,nrang
!C ------loop for all of data grid---------------
           nsign=0
           ttdbz=0
           sspin=0
           num=0
           mbsh(i,j)=-1.0
!--------------------------------------------------
           if (fld(i,j) .ne. spval) then
              do ma=j-mazim_hf,j+mazim_hf
                m=ma
                if(ma.le.0) m=np+ma
                if(ma.gt.np)m=ma-np
                  do mr=i-mgate_hf,i+mgate_hf
                    if(fld(mr+1,m).ne.spval.and.fld(mr,m).ne.spval) then 
                       num=num+1
                       diff=fld(mr+1,m)-fld(mr,m)
                       ttdbz=ttdbz+(diff*diff)
                       if(abs(diff).gt.dbzthresh) sspin=sspin+1
                        if(diff.gt.0) nsign=nsign+1                       
                        if(diff.lt.0) nsign=nsign-1
                      endif
                   enddo
               enddo
             if(num.gt.numthresh) then 
               nsign=nsign/num
               ttdbz=ttdbz/num
               sspin=100*sspin/num
               tdbz(i,jj)=ttdbz
               spin(i,jj)=sspin
               sign(i,jj)=nsign
             endif
         endif

!-------------------------------------- 
         enddo
         enddo
        return
        end


!c**************************************************************
      subroutine sdt_dev_ve(fld_mdv,nr,np,iminrg,nrang,mgate,mazim,numthresh,spval,SDEVV)
!c___________________________________________________
!c
!c    Purpose: calculate the standard deviation of velocity and SW for AP detecting
!c    Author: Liping Liu
!c    Date:   APril. 8, 2004
!c___________________________________________________

!c___________________________________________________
!c
!c    Input: fldv,fldw,fld_mdv and fld_mdw,nr,np  :input the data field, median value and dimension number
!c    Input:iminrg, nrang : Calculating Domain of range
!c    Input: mgate,mazim  : Averaging grid number in radial and azm direction
!c    Input: Spval:    bad value
!c    output: sdevv,sdevw(nr,np): the value at echo grid

!c___________________________________________________

       implicit none
       integer  nr,np,mgate,mazim,mgate_hf,mazim_hf
       real fld_mdv(nr,np),spval,sdevv(nr,np)
       integer i,j,m,ma,mr,iminrg,nrang,num
       real ssv,ssw,avg
       integer numv,numthresh
!c-------------------------------------------------------
       mgate_hf=(mgate-1)/2
       mazim_hf=(mazim-1)/2
       if(iminrg-mgate_hf.lt.1.or.iminrg+mgate_hf.gt.np) then
         write(*,*)" error iminrg or nrang in sdt_dev_ve"
         return
       endif
        do j=1,np
        do i=iminrg,nrang
          sdevv(i,j)=spval
        enddo
        enddo

       do j=1,np
       do i=iminrg,nrang
!C ------loop for all of data grid---------------
           ssv=0.
           avg=0.0
           numv=0
!--------------------------------------------------
      
       if (abs(fld_mdv(i,j)).le.30) then
           do ma=j-mazim_hf,j+mazim_hf
              m=ma
              if(ma.le.0) m=np+ma
              if(ma.gt.np)m=ma-np
              do mr=i-mgate_hf,i+mgate_hf
                 if(fld_mdv(mr,m).ne.spval) then 
                       numv=numv+1
                       avg=avg+fld_mdv(mr,m)
                 endif    
               enddo
            enddo
          if(numv.gt.numthresh) then 
             avg=avg/numv 
             do ma=j-mazim_hf,j+mazim_hf
                m=ma
                if(ma.le.0) m=np+ma
                if(ma.gt.np)m=ma-np
             do mr=i-mgate_hf,i+mgate_hf
                 if(fld_mdv(mr,m).ne.spval) ssv=ssv+(fld_mdv(mr,m)-avg)*(fld_mdv(mr,m)-avg)
             enddo
             enddo
             sdevv(i,j)=sqrt(ssv/(numv-1))
!          if(numv.gt.numthresh) write(*,*)sdevv(i,j)
          endif

       endif
       enddo
       enddo
       return
       end
