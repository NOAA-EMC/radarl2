!     ##################################################################
!     ##################################################################
!     ######                                                      ######
!     ######        Radar Data Quality Control System.            ######
!     ######                                                      ######
!     ######              Copyright (c) 2009                      ######
!     ######             Cooperative Institute                    ######
!     ######       for Mesoscale Meteorological Studies           ######
!     ######    University of Oklahoma.  All rights reserved.     ######
!     ######                                                      ######
!     ##################################################################
!     ##################################################################
!=======================================================================
!     PURPOSE: 
!       The program is for analyzing the Bird clutter by using 
!       fuzzy logic algorithms.
!=======================================================================
!
!     AUTHOR : Yuan Jiang
!     Date     : Mar. 9, 2011
!     Action   : Created.
!
!     Modify Histroy
!     --------------
!     name   : Kang Nai
!     date   : Aug. 2, 2012
!     action : change it from structure to common.

!     name   : Shun Liu
!     date   : Sep. 12, 2012
!     action : bug fix and move mem_para_me%MDZDR to rain detection part

!     name   : Shun Liu
!     date   : Sep. 16, 2012
!     action : add bird detection function
!=======================================================================
      Subroutine qc_nonm( np,nr,nazim_reff,ngate_reff,reff           &
                         ,nazim_ve,ngate_ve,ve                       &
                         ,sw,zdr,pho,rho,tmp_mark                    &
                         ,spval )

      USE sdata

      implicit none

      integer            :: np,nr
      integer            :: nazim_reff,ngate_reff
      real               :: reff(nr,np)
      integer            :: nazim_ve,ngate_ve
      real               :: ve(nr,np)
      real               :: sw(nr,np),zdr(nr,np),pho(nr,np),rho(nr,np)
      real               :: tmp_mark(nr,np)
      real               :: spval

      real               :: mdve(nr,np),mdsw(nr,np),mdz1(nr,np)      &
                           ,mdzr(nr,np),mdph(nr,np),mdrh(nr,np)
      real               :: tdbz(nr,np),tdph(nr,np),mve(nr,np)
      real               :: spin(nr,np),sign(nr,np),gdzz(nr,np)
      real               :: mdzr_ap(nr,np),mdph_ap(nr,np)            &
                           ,mdzr_ra(nr,np),mdph_ra(nr,np)            &
                           ,mdzr_bs(nr,np),mdph_bs(nr,np)
      real               :: dbzthresh,ddbz,ddmax,mbsh_s
      real               :: dazim,thresh_ap,thresh_me
      integer            :: iazm,ngnum_ref,ngnum_vel,ngnum_ref2      &
                           ,ngnum_zdr,ngnum_sw,ngnum_phi,ngnum_rho   &
                           ,m0,n0,mm,nn
      type(mem_para)     :: mem_para_ap,mem_para_bs                  &
                           ,mem_para_me,mem_para_bi
      type(wt_para)      :: wt_ap, wt_bs,wt_me,wt_bi
      real               :: clutter,rain,bios,bird
      real               :: sum_v, num

      character*1        :: ip
      character*2        :: ip2
      character*3        :: ip3  
      integer            :: v,u,i11,i22,j11,j22,i,j,datanum,ii,id,jd,jj
      integer            :: count1,count2,count3,count4,count5
      real               :: mbsh_out_ap(nr,np),mbsh_out_me(nr,np)
      real               :: mbsh_out_bs(nr,np),mbsh_out_bi(nr,np)
      real               :: mbsh_num,r_num,s_num
      real               :: fl,fh,temp
      real               :: mem(3), mem_min
      integer            :: flag,iflag(3),jp,temp2
!~~~~~~~~~~~~~~~~~~~ EXCUTE CODE ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      mdve= spval
      mdsw= spval
      mdz1= spval
      mdzr= spval
      mdph= spval
      mdrh= spval
      tdbz= spval
      tdph= spval
      mve=  spval

      tmp_mark =spval    

      mdzr_ra= spval
      mdph_ra= spval
      mdzr_ap= spval
      mdph_ap= spval
      mdzr_bs= spval
      mdph_bs= spval

!******** parameters for biological ******************
      mem_para_bs%MDZ(1)= 5
      mem_para_bs%MDZ(2)= 10
      mem_para_bs%MDZ(3)= 20
      mem_para_bs%MDZ(4)= 30

      mem_para_bs%MDZDR(1)= 0
      mem_para_bs%MDZDR(2)= 2
      mem_para_bs%MDZDR(3)= 10
      mem_para_bs%MDZDR(4)= 12

      mem_para_bs%MDRHO(1)= 0.3
      mem_para_bs%MDRHO(2)= 0.5
      mem_para_bs%MDRHO(3)= 0.8
!     mem_para_bs%MDRHO(4)= 0.83
      mem_para_bs%MDRHO(4)= 1.01

      mem_para_bs%TDBZ(1)= 1
      mem_para_bs%TDBZ(2)= 2
      mem_para_bs%TDBZ(3)= 4
      mem_para_bs%TDBZ(4)= 7

      mem_para_bs%TDBPHI(1)= 8
      mem_para_bs%TDBPHI(2)= 10
      mem_para_bs%TDBPHI(3)= 40
      mem_para_bs%TDBPHI(4)= 60

      wt_bs%wt_MDZ=0.4
      wt_bs%wt_MDZDR=0.6
      wt_bs%wt_MDRHO=1
      wt_bs%wt_TDBZ=0.8
      wt_bs%wt_TDBPHI=0.8

!******** FOR BIRD ************************
      mem_para_bi%MDPHI(1)= 50
      mem_para_bi%MDPHI(2)= 70
      mem_para_bi%MDPHI(3)= 140
      mem_para_bi%MDPHI(4)= 160

      mem_para_bi%MDZDR(1)= -3
      mem_para_bi%MDZDR(2)= -1
      mem_para_bi%MDZDR(3)= 3
      mem_para_bi%MDZDR(4)= 4

      wt_bi%wt_MDZDR=1
      wt_bi%wt_MDPHI=1
!******** parameters FOR AP ************************
      mem_para_ap%MDZ(1)= 15
      mem_para_ap%MDZ(2)= 20
      mem_para_ap%MDZ(3)= 70
      mem_para_ap%MDZ(4)= 80

      mem_para_ap%MDZDR(1)= -4
      mem_para_ap%MDZDR(2)= -2
      mem_para_ap%MDZDR(3)= 1
      mem_para_ap%MDZDR(4)= 2

      mem_para_ap%MDRHO(1)= 0.5
      mem_para_ap%MDRHO(2)= 0.8
!     mem_para_ap%MDRHO(2)= 0.6
      mem_para_ap%MDRHO(3)= 0.9
      mem_para_ap%MDRHO(4)= 0.95

      mem_para_ap%TDBZ(1)= 2
      mem_para_ap%TDBZ(2)= 4
      mem_para_ap%TDBZ(3)= 10
      mem_para_ap%TDBZ(4)= 15

      mem_para_ap%TDBPHI(1)= 30
      mem_para_ap%TDBPHI(2)= 40
      mem_para_ap%TDBPHI(3)= 50
      mem_para_ap%TDBPHI(4)= 60

!      wt_ap%wt_MDZ=0.2
!      wt_ap%wt_MDRHO=1
!      wt_ap%wt_TDBZ=0.6
      wt_ap%wt_MDZ=0.4
      wt_ap%wt_MDZDR=0.4
      wt_ap%wt_MDRHO=0.4
      wt_ap%wt_TDBZ=0.5
      wt_ap%wt_TDBPHI=0.8

!******** meteorological echo **********************
      mem_para_me%MDZ(1)= 5 
      mem_para_me%MDZ(2)= 10
      mem_para_me%MDZ(3)= 65
      mem_para_me%MDZ(4)= 75

!     mem_para_me%MDZDR(1)= fl-0.3 
!     mem_para_me%MDZDR(2)= fl 
!     mem_para_me%MDZDR(3)= fh
!     mem_para_me%MDZDR(4)= fh+0.3

      mem_para_me%MDRHO(1)= 0.85
      mem_para_me%MDRHO(2)= 0.97
      mem_para_me%MDRHO(3)= 1
      mem_para_me%MDRHO(4)= 1.05
!      mem_para_me%MDRHO(4)= 1.01

      mem_para_me%TDBZ(1)= 0
      mem_para_me%TDBZ(2)= 0.5
      mem_para_me%TDBZ(3)= 3
      mem_para_me%TDBZ(4)= 6

      mem_para_me%TDBPHI(1)= 0
      mem_para_me%TDBPHI(2)= 1
      mem_para_me%TDBPHI(3)= 15
      mem_para_me%TDBPHI(4)= 30

!     wt_me%wt_MDZ=0.8
      wt_me%wt_MDZ=1.0
!     wt_me%wt_MDZDR=0.8
      wt_me%wt_MDZDR=1.0
      wt_me%wt_MDRHO=0.6
      wt_me%wt_TDBZ=0.2
      wt_me%wt_TDBPHI=0.2

!***************************************************
      do j = 1,nazim_reff
      do i = 1, ngate_reff
       if (pho(i,j)>180 .and. pho(i,j).ne.spval) pho(i,j)=pho(i,j)-360
      end do
      end do  
        
!--------------------calculate of feature functions---------------------
!-----------------------------------------------------------------------
!     smooth  z, zdr, ve, pho, pho  along with radial.
!     smooth window: z,1km, zdr 2km, ve 2km , rho 2km, pho 6km.
!     1km= 5 successive samples
!-----------------------------------------------------------------------
      call median ( 5,reff,nr,np,1,ngate_reff,spval,mdz1 )
      call median ( 9,ve,nr,np,1,ngate_ve,spval,mdve )
      call median ( 9,zdr,nr,np,1,ngate_ve,spval,mdzr )
      call median ( 9,rho,nr,np,1,ngate_ve,spval,mdrh )
      call median (25,pho,nr,np,1,ngate_ve,spval,mdph )

!-----------------------------------------------------------------------
!     calculate the mean radial velocity
!-----------------------------------------------------------------------
      mve = mdve
      do j=1,nazim_ve
        do i= 3,ngate_ve-2
          sum_v = 0.0
          num=0
          if (mdve(i,j) < spval) then 
            do ii = i-2, i+1
              if (mdve(ii,j) < spval) then
                num=num+1
                sum_v= sum_v + mdve(ii,j)
              end if
            end do
            mve(i,j) = sum_v/num
          end if
        end do
      end do
!------------------------------------------------------------------------
!     calculate texture for z and pho along the radial.
!     the window: z 1km, pho 2km.
!------------------------------------------------------------------------
      call SDpara(mdz1,nr,np,3,ngate_reff,5,spval,tdbz) 
      call SDpara(mdph,nr,np,5,ngate_ve,9,spval,tdph)
!--------------------------member function------------------------------
!     Several steps to continue qc echo check
!     First : using abs(MVE) and fuzzy logical to check for ap
!     Second :
!     Third  :
!     Fouth  :
!-----------------------------------------------------------------------
      clutter = 999.2
      thresh_ap = 0.45
      do j=1,nazim_ve
       do i=1,ngate_ve
        mbsh_out_ap(i,j) = 0.0
        mbsh_num =0.0
!        if ( abs(MVE(i,j)) <= 1.0 ) then
        if ( abs(MVE(i,j)) <= 1.0 .or. (ve(i,j) == spval.and. MDZ1(i,j)/=spval)) then
          if ( MDZ1(i,j) /= spval ) then
            call mfun(mem_para_ap%MDZ,MDZ1(i,j),mbsh_s)
            mbsh_out_ap(i,j) = mbsh_out_ap(i,j) + mbsh_s*wt_ap%wt_MDZ
            mbsh_num = mbsh_num + wt_ap%wt_MDZ
          end if
          if ( MDZR(i,j) /= spval .and. MDZ1(i,j)/=spval ) then
            call mfun(mem_para_ap%MDZDR,MDZR(i,j),mbsh_s)
            mbsh_out_ap(i,j) = mbsh_out_ap(i,j) + mbsh_s*wt_ap%wt_MDZDR
            mbsh_num = mbsh_num + wt_ap%wt_MDZDR
          end if
          if ( MDRH(i,j) /= spval ) then
            call mfun(mem_para_ap%MDRHO,MDRH(i,j),mbsh_s)
            mbsh_out_ap(i,j) = mbsh_out_ap(i,j) + mbsh_s*wt_ap%wt_MDRHO
            mbsh_num = mbsh_num + wt_ap%wt_MDRHO
          end if
          if ( TDBZ(i,j) /= spval ) then
            call mfun(mem_para_ap%TDBZ,TDBZ(i,j),mbsh_s)
            mbsh_out_ap(i,j) = mbsh_out_ap(i,j) + mbsh_s*wt_ap%wt_TDBZ
            mbsh_num = mbsh_num + wt_ap%wt_TDBZ
          end if
          if ( TDPH(i,j) /= spval ) then
            call mfun(mem_para_ap%TDBPHI,TDPH(i,j),mbsh_s)
            mbsh_out_ap(i,j) = mbsh_out_ap(i,j) + mbsh_s*wt_ap%wt_TDBPHI
            mbsh_num = mbsh_num + wt_ap%wt_TDBPHI
          end if        
!          if (mbsh_num > 0) then
          if (mbsh_num > 1) then
            mbsh_out_ap(i,j) = mbsh_out_ap(i,j)/mbsh_num
          else
            mbsh_out_ap(i,j) = 0
          end if
        end if
       end do   ! enddo i
   
       do i=ngate_ve+1,ngate_reff
        mbsh_out_ap(i,j) = 0.0
        mbsh_num =0.0
        if ( MDZ1(i,j) /= spval ) then
          call mfun(mem_para_ap%MDZ,MDZ1(i,j),mbsh_s)
          mbsh_out_ap(i,j) = mbsh_out_ap(i,j) + mbsh_s*wt_ap%wt_MDZ
          mbsh_num = mbsh_num + wt_ap%wt_MDZ
        end if
        if ( MDZR(i,j)/=spval .and. MDZ1(i,j)/=spval ) then
          call mfun(mem_para_ap%MDZDR,MDZR(i,j),mbsh_s)
          mbsh_out_ap(i,j) = mbsh_out_ap(i,j) + mbsh_s*wt_ap%wt_MDZDR
          mbsh_num = mbsh_num + wt_ap%wt_MDZDR
        end if
        if ( MDRH(i,j) /= spval ) then
          call mfun(mem_para_ap%MDRHO,MDRH(i,j),mbsh_s)
          mbsh_out_ap(i,j) = mbsh_out_ap(i,j) + mbsh_s*wt_ap%wt_MDRHO
          mbsh_num = mbsh_num + wt_ap%wt_MDRHO
        end if
        if ( TDBZ(i,j) /= spval ) then
          call mfun(mem_para_ap%TDBZ,TDBZ(i,j),mbsh_s)
          mbsh_out_ap(i,j) = mbsh_out_ap(i,j) + mbsh_s*wt_ap%wt_TDBZ
          mbsh_num = mbsh_num + wt_ap%wt_TDBZ
        end if
        if (TDPH(i,j) /= spval) then
          call mfun(mem_para_ap%TDBPHI,TDPH(i,j),mbsh_s)
          mbsh_out_ap(i,j) = mbsh_out_ap(i,j) + mbsh_s*wt_ap%wt_TDBPHI
          mbsh_num = mbsh_num + wt_ap%wt_TDBPHI
        end if
!        if (mbsh_num > 0) then
        if (mbsh_num > 1) then
           mbsh_out_ap(i,j) = mbsh_out_ap(i,j)/mbsh_num
        else
           mbsh_out_ap(i,j) = 0
        end if
       end do      ! enddo i
      end do    ! enddo j
!     ------------------------------------
      rain=999.6
      count2=0
      count3=0
      count4=0
      thresh_me = 0.45
      do j=1,nazim_reff
       do i=1,ngate_reff
         mbsh_out_me(i,j) = 0.0
         mbsh_num =0.0
         if ( MDZ1(i,j)<spval ) then
           call mfun(mem_para_me%MDZ,MDZ1(i,j),mbsh_s)
           mbsh_out_me(i,j) = mbsh_out_me(i,j) + mbsh_s*wt_me%wt_MDZ
           mbsh_num = mbsh_num + wt_me%wt_MDZ
         end if
         if ( MDZR(i,j)<spval .and. MDZ1(i,j)<spval ) then
           fl=-0.50+2.50*10**(-3)*MDZ1(i,j)+7.50*10**(-4)*MDZ1(i,j)**2
           fh=0.08+3.64*10**(-2)*MDZ1(i,j)+3.57*10**(-4)*MDZ1(i,j)**2
           mem_para_me%MDZDR(1)= fl-0.3 
           mem_para_me%MDZDR(2)= fl 
           mem_para_me%MDZDR(3)= fh
           mem_para_me%MDZDR(4)= fh+0.3
           call mfun(mem_para_me%MDZDR,MDZR(i,j),mbsh_s)
           mbsh_out_me(i,j) = mbsh_out_me(i,j) + mbsh_s*wt_me%wt_MDZDR
           mbsh_num = mbsh_num + wt_me%wt_MDZDR
         end if
         if ( MDRH(i,j)<spval ) then
           call mfun(mem_para_me%MDRHO,MDRH(i,j),mbsh_s)
           mbsh_out_me(i,j) = mbsh_out_me(i,j) + mbsh_s*wt_me%wt_MDRHO
           mbsh_num = mbsh_num + wt_me%wt_MDRHO
         end if
         if ( TDBZ(i,j)<spval ) then
           call mfun(mem_para_me%TDBZ,TDBZ(i,j),mbsh_s)
           mbsh_out_me(i,j) = mbsh_out_me(i,j) + mbsh_s*wt_me%wt_TDBZ
           mbsh_num = mbsh_num + wt_me%wt_TDBZ
         end if
         if ( TDPH(i,j)<spval ) then
           call mfun(mem_para_me%TDBPHI,TDPH(i,j),mbsh_s)
           mbsh_out_me(i,j) = mbsh_out_me(i,j) + mbsh_s*wt_me%wt_TDBPHI
           mbsh_num = mbsh_num + wt_me%wt_TDBPHI
         end if
!         if ( mbsh_num> 0 ) then
         if (mbsh_num > 1) then
           mbsh_out_me(i,j) = mbsh_out_me(i,j)/mbsh_num
         else
           mbsh_out_me(i,j) = 0
         end if
       end do
      end do

      bios=999.7
      do j = 1, nazim_reff
      do i = 1, ngate_reff
        mbsh_out_bs(i,j) = 0.0
        mbsh_num =0.0

        if (MDZ1(i,j) < spval) then
          call mfun(mem_para_bs%MDZ,MDZ1(i,j),mbsh_s)
          mbsh_out_bs(i,j) = mbsh_out_bs(i,j) + mbsh_s * wt_bs%wt_MDZ
          mbsh_num = mbsh_num + wt_bs%wt_MDZ
        end if
        if (MDZR(i,j) <  spval .and. MDZ1(i,j) < spval) then
         fl=-0.50 + 2.50*10**(-3)*MDZ1(i,j) + 7.50*10**(-4)*MDZ1(i,j)**2
         fh=0.08 + 3.64*10**(-2)*MDZ1(i,j) + 3.57*10**(-4)*MDZ1(i,j)**2
         mem_para_me%MDZDR(1)= fl-0.3 
         mem_para_me%MDZDR(2)= fl 
         mem_para_me%MDZDR(3)= fh
         mem_para_me%MDZDR(4)= fh+0.3
         call mfun(mem_para_bs%MDZDR,MDZR(i,j),mbsh_s)
         mbsh_out_bs(i,j) = mbsh_out_bs(i,j) + mbsh_s * wt_bs%wt_MDZDR
         mbsh_num = mbsh_num + wt_bs%wt_MDZDR
        end if
        if (MDRH(i,j) < spval) then
          call mfun(mem_para_bs%MDRHO,MDRH(i,j),mbsh_s)
          mbsh_out_bs(i,j) = mbsh_out_bs(i,j) + mbsh_s * wt_bs%wt_MDRHO
          mbsh_num = mbsh_num + wt_bs%wt_MDRHO
        end if
        if (TDBZ(i,j) < spval) then
          call mfun(mem_para_bs%TDBZ,TDBZ(i,j),mbsh_s)
          mbsh_out_bs(i,j) = mbsh_out_bs(i,j) + mbsh_s * wt_bs%wt_TDBZ
          mbsh_num = mbsh_num + wt_bs%wt_TDBZ
        end if
        if (TDPH(i,j) < spval) then
          call mfun(mem_para_bs%TDBPHI,TDPH(i,j),mbsh_s)
          mbsh_out_bs(i,j) = mbsh_out_bs(i,j) + mbsh_s * wt_bs%wt_TDBPHI
          mbsh_num = mbsh_num + wt_bs%wt_TDBPHI
        end if
!        if (mbsh_num > 0) then
        if (mbsh_num > 1) then
          mbsh_out_bs(i,j) = mbsh_out_bs(i,j)/mbsh_num
        else
          mbsh_out_bs(i,j) = 0
        end if
      end do
      end do
      
      do j = 1, nazim_reff
      do i = 1, ngate_reff
        if (reff(i,j) < spval) then
          iflag(1) = 1
          iflag(2) = 2
          iflag(3) = 3
          mem(1) = mbsh_out_ap(i,j)
          mem(2) = mbsh_out_me(i,j)
          mem(3) = mbsh_out_bs(i,j)
          do ii =1, 3
            mem_min = mem(ii)
            do jj = ii+1, 3
              if (mem_min > mem(jj)) then
                temp = mem(jj)
                mem(jj)=mem(ii)
                mem(ii)=temp
                mem_min=mem(ii)
                temp2 = iflag(jj)
                iflag(jj) = iflag(ii)
                iflag(ii) = temp2
              endif
            end do
          end do
         if (iflag(3)==1 .and.                                     &
             (mbsh_out_ap(i,j)<0.55 .or.ve(i,j)>=spval.or.(reff(i,j)<15.or.reff(i,j)>=spval))) then
              flag = iflag(2)
                 if ( iflag(3)==2 .and.                                     &
                    (mbsh_out_me(i,j)<0.45 .or. (zdr(i,j)>6 .and. zdr(i,j)<spval).or.rho(i,j)<0.6) ) then !mem(3)<0.45
                      flag = iflag(1)
                 end if
          else
              flag = iflag(3)
              if ( iflag(3)==2 .and.                                     &
                  (mbsh_out_me(i,j)<0.45 .or. (zdr(i,j)>6 .and. zdr(i,j)<spval).or.rho(i,j)<0.6) ) then !mem(3)<0.45
                   flag = iflag(2)
                    if (iflag(3)==1 .and.                                    &
                        (mbsh_out_ap(i,j)<0.55 .or.ve(i,j)>=spval.or.(reff(i,j)<15.or.reff(i,j)>=spval))) flag = iflag(1)
               end if                   
          end if
          if (flag == 1) tmp_mark(i,j) = clutter
          if (flag == 2) tmp_mark(i,j) = rain
          if (flag == 3) tmp_mark(i,j) = bios
          if (flag == 0) tmp_mark(i,j) = spval
        end if
      end do
      end do
!----------------------------------------------------------------------------
    !* clean neighbor points for clutters
    do j = 1, nazim_reff
       do i = 3, ngate_reff-2
        if(tmp_mark(i,j) == clutter) then
           do jd = -2,+2
             jj =jd +jj
             do id = -2,2
               ii = id + i
               if(jj<0) jj=nazim_reff +jj
               if(jj>nazim_reff) jj= jj- nazim_reff
               if((abs(ve(ii,jj)).le.1 .and. (reff(ii,jj) <spval.and.reff(ii,jj)>15)) .or. &
                 (ve(ii,jj)>spval.and.(reff(ii,jj)<spval.and.reff(ii,jj)>15).and.mbsh_out_ap(ii,jj)>0.25))then
                 tmp_mark(ii,jj) =clutter
               end if
            end do
           end do
         end if
       end do
    end do  
  

   do j = 1, nazim_reff
       !* clean neighbor points for clutters along radial line
       do i = 11, ngate_reff-10
       if (tmp_mark(i,j) /= spval) then
         r_num = 0
         s_num = 0
         do jj = j-2,j+2
         do  ii = i-10, i+10
           jp = jj
           if(jp>nazim_reff) jp=jp-nazim_reff
           if(jp<0) jp =jp+nazim_reff
           if(tmp_mark(ii,jp) == clutter) r_num = r_num + 1
           if(tmp_mark(ii,jp) /= spval) s_num = s_num + 1
         end do
         end do
         if (r_num/s_num*1.0 >= 0.55.and.tmp_mark(i,j)/=clutter) tmp_mark(i,j)=-2
       end if
      end do
    end do
  
     
      do j = 1, nazim_reff
      do i = 1, ngate_reff
        if(tmp_mark(i,j)== -2 ) tmp_mark(i,j)=clutter
      end do
      end do

      !* clean neighbor points for rain, if rain prob is low, it will be
      !* bios
      do j = 1, nazim_reff
      do i = 11, ngate_reff-10
       if (tmp_mark(i,j) /= spval) then
         r_num = 0
         s_num = 0
         do jj = j-2,j+2
         do  ii = i-20, i+20
           jp = jj
           if(jp>nazim_reff) jp=jp-nazim_reff
           if(jp<0) jp =jp+nazim_reff
           if(tmp_mark(ii,jp) == rain) r_num = r_num + 1
           if(tmp_mark(ii,jp) /= spval) s_num = s_num + 1
         end do
         end do
         if (r_num/s_num*1.0 < 0.25.and.tmp_mark(i,j)==rain)  tmp_mark(i,j)=-1
         if (r_num/s_num*1.0 >= 0.75.and.tmp_mark(i,j)/=rain) tmp_mark(i,j)=-2
       end if
      end do
      end do

      do j = 1, nazim_reff
      do i = 1, ngate_reff
        if(tmp_mark(i,j)== -1 ) tmp_mark(i,j)=bios
        if(tmp_mark(i,j)== -2 ) tmp_mark(i,j)=rain
      end do
      end do

      !* add height, vad wind, model wind as part of function.
      do j = 1, nazim_reff
      do i = 1, ngate_reff
        if (tmp_mark(i,j)== bios) then
          mbsh_out_bi(i,j) = 0.0
          mbsh_num =0.0

          if (MDPH(i,j) < spval) then
            call mfun(mem_para_bi%MDPHI,MDPH(i,j),mbsh_s)
            mbsh_out_bi(i,j)=mbsh_out_bi(i,j) + mbsh_s*wt_bi%wt_MDPHI
            mbsh_num = mbsh_num + wt_bi%wt_MDPHI
          end if
          if (MDZR(i,j) <  spval) then
            call mfun(mem_para_bi%MDZDR,MDZR(i,j),mbsh_s)
            mbsh_out_bi(i,j)=mbsh_out_bi(i,j) + mbsh_s*wt_bs%wt_MDZDR
            mbsh_num = mbsh_num + wt_bi%wt_MDZDR
          end if
          if (mbsh_num > 1) then
             mbsh_out_bi(i,j) = mbsh_out_bi(i,j)/mbsh_num
          else
             mbsh_out_bi(i,j) = 0
          end if
        end if
      end do
      end do

      bird=999.8
!     do j = 1, nazim_reff
!     do i = 1, ngate_reff
!       if(mbsh_out_bi(i,j)/=spval.and.mbsh_out_bi(i,j)>0.45)then
!          tmp_mark(i,j)=bird
!       end if
!     end do
!     end do

!     do j = 1, nazim_reff
!     do i = 1, ngate_reff
!        if (reff(i,j)/=spval) then
!          if (reff(i,j) == clutter) then
!            tmp_mark(i,j)=clutter
!          else if (reff(i,j) == bios) then
!            tmp_mark(i,j)=bios
!            if(mbsh_out_bi(i,j)>0.6) then
!              tmp_mark(i,j)=bird
!            endif
!          end if
!        end if
!     end do
!     end do

      return
      end subroutine qc_nonm
