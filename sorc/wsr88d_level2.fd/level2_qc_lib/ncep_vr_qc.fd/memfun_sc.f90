!####################################################################################  
!PURPOSE
! The subroutine is for generating member functions for sea clutters detection
!#####################################################################################
!AUTHOR: Liping Liu
!09/20/04
!
!MODIFICATION HISTORY:
!Date       Name          Action
!#########################################################
!INPUT
!tdbz: texture of reflectivity
!sign: sign change of reflectivity
!spin: spin of reflectivity 
!gdz: vertical variation of reflectivity
!m_fldve: median velocity
!sdve: standard diviation of velocity
!sw:   median spectrum
! ng,na: dimension of the input radar data
!OUTPUT
!mbshout sum of member function 
!#######################################################

Subroutine memfun_sc(ng,na,tdbz,sign,spin,gdz,m_fldve,sdevv,sw,mbshout)
USE sdata
implicit none
integer, parameter :: nazim_wk=367
integer ng,na
real m_fldve(ng,na),gdz(ng,na),tdbz(ng,na),sign(ng,na),spin(ng,na),gdzz,sw(ng,na),sdevv(ng,na)
real mbshout(ng,na),mbsh_s,ifact,spval
integer i,ir,j,jj,jj1,jj2,k,nr,np
integer nptve,mgateve,nrangve,mazimve,iminrgve,numthreshve,title_lev,mbsh_num(ng,na)
 character(40) char_tmp
 character(1)title_mark
  type (mem_para_ap) mem_para

   mem_para%MDSW(1)=-1000.0
   mem_para%MDSW(2)=0.
   mem_para%MDSW(3)=6.0

   mem_para%SDVE(1)=-1000.0
   mem_para%SDVE(2)=0.
   mem_para%SDVE(3)=0.7

   mem_para%TDBZ(1)=0.0
   mem_para%TDBZ(2)=55.0
   mem_para%TDBZ(3)=1000.

   mem_para%SPIN(1)=40.
   mem_para%SPIN(2)=60.
   mem_pAra%SPIN(3)=1000.

   mem_para%gdz(1)=-90000.
   mem_para%gdz(2)=-8.0
   mem_para%gdz(3)=0.0


!********parameter for AP******************
!nptve: gate number in  the mediam smooth   
!mgateref and mazimref,mgateve,mazimve: the average number in range and azmith for calculating the TDBZ, SPIN, SIGN and SDEV.
!dbzthresh : the thresh of ref for spin
   spval=999.0
   do j=1,na
   do i=1,ng
     mbsh_num(i,j)=0
     mbshout(i,j)=0.
     if(sdevv(i,j).ne.spval) then
       call mfun(mem_para%SDVE,SDEVV(i,j),mbsh_s)
       mbshout(i,j)=mbshout(i,j)+mbsh_s
       mbsh_num(i,j)=mbsh_num(i,j)+1
     endif
     if(sw(i,j).ne.spval) then
       call mfun(mem_para%MDSW,sw(i,j),mbsh_s)
       mbshout(i,j)=mbshout(i,j)+mbsh_s
       mbsh_num(i,j)=mbsh_num(i,j)+1
     endif
     ir=(i+2)/4
     if(tdbz(ir,j).ne.spval) then
        call mfun(mem_para%tdbz,tdbz(ir,j),mbsh_s)
        mbshout(i,j)=mbshout(i,j)+mbsh_s
        mbsh_num(i,j)=mbsh_num(i,j)+1   
     endif
     if(spin(ir,j).ne.spval) then 
        call mfun(mem_para%spin,spin(ir,j),mbsh_s)
        mbshout(i,j)=mbshout(i,j)+mbsh_s
        mbsh_num(i,j)=mbsh_num(i,j)+1
     endif

     if(gdz(ir,j).ne.spval) then
        if(i.le.160) then
          ifact=1
        else
          ifact=(300.-i/4.0)/(300.-40.)
        endif
        gdzz=gdz(ir,j)*ifact
        call mfun(mem_para%gdz,gdzz,mbsh_s)
        mbshout(i,j)=mbshout(i,j)+mbsh_s*3.
        mbsh_num(i,j)=mbsh_num(i,j)+3
     endif

     if(mbsh_num(i,j).gt.2) then 
        mbshout(i,j)=mbshout(i,j)/mbsh_num(i,j)
     else 
       mbshout(i,j)=0
     endif
   enddo
   enddo




  return
  end
  
