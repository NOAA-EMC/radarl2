!===========================================================================
!Purpose:
!  use dual-pol variable for reflectivity, radial wind and VAD wind QC
!  This is the second version of simplified DPQC. DPQC is done after 
!  regular radial widn and reflectivity QC. It should be applied before 
!  regular QC to remove clutter and other non-meteorological echo. 
!  However, there are dramatic impact on generating VAD wind at low
!  elevation since insects can not be well identified from biological
!  echos. Will update code after advance algorithm available.


!  author: Shun Liu
!  time:  08/08/2012

!input: CC, ZDR, KDP
!output: Vr, CC
!===========================================================================


      SUBROUTINE dpqc( nelv_ref,nelv_vel                         &
                      ,strct_in_ref,strct_in_vel                 &
                      ,strct_in_cc,strct_in_zdr, strct_in_kdp )

      use sdata

      implicit none

      real,parameter:: missing_data=999.0
      integer                     :: nelv_ref, nelv_vel
      type(ccout_strct)           :: strct_in_ref(nelv_ref)
      type(ccout_strct)           :: strct_in_vel(nelv_vel)
      type(ccout_strct)           :: strct_in_cc(nelv_vel)
      type(ccout_strct)           :: strct_in_cc1(nelv_vel)
      type(ccout_strct)           :: strct_in_zdr(nelv_vel)
      type(ccout_strct)           :: strct_in_kdp(nelv_vel)

      !----------------------------------------------------------
      !     Parameters used for removing ref 
      !----------------------------------------------------------
      integer           :: i,j,k,naz,ng,ng_cc,ii,tii,i_cc
      real              :: cc,ctmp,ccavg

      !----------------------------------------------------------
      !     Parameters used for NBF 
      !----------------------------------------------------------
      real              :: ref
      integer           :: iref35

!----------------------------------------------------------------
!     REF QC to remove clutters
!----------------------------------------------------------------
      do k=1,nelv_vel
        naz=strct_in_ref(k)%num_beam
        ng=strct_in_ref(k)%num_gate
        ng_cc=strct_in_cc(k)%num_gate
        do j=1,naz
        !--------------------------------------------------------
        !    non_uniform beam filling 
        !--------------------------------------------------------
        iref35=0
        do i=1,ng
          ref=strct_in_ref(k)%field(i,j)
          if(ref>35.0.and.ref<99.0)iref35=iref35+1
        end do
        !--------------------------------------------------------
        !    remove ref if cc<0.8  
        !--------------------------------------------------------
             strct_in_cc1=strct_in_cc
        do i=1,ng
           cc=0.0
           tii=0
           do ii=1,4
             i_cc=(i-1)*4+ii
             if(i_cc<ng_cc)then
             strct_in_cc1(k)%field(i_cc,j)=missing_data
             ctmp=strct_in_cc(k)%field(i_cc,j)
             if(abs(ctmp)<1.2)then
             tii=tii+1
             cc=cc+ctmp
             end if
             end if
           end do

           if(tii>0)then
             cc=cc/real(tii)
             if(cc<0.4) write(*,*)'cc=',cc,iref35
             !if(cc<0.9.and.iref35<10) strct_in_ref(k)%field(i,j)=missing_data 
             !if(cc<0.97) strct_in_ref(k)%field(i,j)=missing_data 
           end if

           do ii=1,4
             i_cc=(i-1)*4+ii
             if(i_cc<ng_cc)then
             strct_in_cc1(k)%field((i-1)*4+ii,j)=cc
             end if
           end do

        end do
        end do
      end do

!     do k=1,nelv_vel
!       naz=strct_in_cc(k)%num_beam
!       ng=strct_in_cc(k)%num_gate
!       do j=1,naz
!       do i=1,ng
!       strct_in_cc(k)%field(i,j)=missing_data
!       end do
!       end do
!     end do
        strct_in_cc=strct_in_cc1
!----------------------------------------------------------------
!     End REF QC to remove clutters
!----------------------------------------------------------------

      RETURN
      END SUBROUTINE dpqc
