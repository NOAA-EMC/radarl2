! The subroutine is for removing the ground and sea clutters 

!#######################################################################
!
!     AUTHOR: Liping Liu
!     09/25/2004.
!#########################################################################
!input:
! strct_in_ref,strct_in_vel,strct_in_sw: reflectivity,velocity and width spectrum
! ng,na: dimension in radial and azimuthal direction
! mazim_vel,nazim_ref: azimuthal transfer due datasort 
! mbsh_out: interesting values, >0.5 ground and sea clutters
!output: 
!strct_in_ref,strct_in_vel,strct_in_sw: the data after removed the clutters, set the points to spval
!############################################################################

Subroutine setapecho(strct_in_ref,strct_in_vel,strct_in_sw,ng,na, mazim_vel,nazim_ref,mbsh_out,cc)
USE sdata
implicit none
integer, parameter :: nazim_wk=367
integer ng,na
integer mazim_vel(na),nazim_ref(na)
real mbsh_out(ng,na),mbsh_tmp(ng,na),spval
character*2 cc
type(ccout_strct) :: strct_in_ref,strct_in_vel,strct_in_sw
integer i,j,k,jj,ir
spval=999.0
!set the AP echo to spval
    do j=1,na
    do i=1,ng
      mbsh_tmp(i,mazim_vel(j))=mbsh_out(i,j)
    enddo
    enddo
    do j=1,na 
    do i=1,ng
      mbsh_out(i,j)=mbsh_tmp(i,j)
    enddo
    enddo
     do j=1,na
        jj=nazim_ref(j)
     do i=1,ng
        ir=(i+2)/4
         if(mbsh_out(i,j).gt.0.5) then
!           strct_in_ref%field(ir,jj)=spval
            strct_in_vel%field(i,j)=spval
            strct_in_sw%field(i,j)=spval
          endif
     enddo
     enddo

!    output the modified data
     if(1.eq.0) then 
      open(61,file='ncar.vr')
      write(61,'(15f5.1)')(strct_in_vel%azim(i),i=1,nazim_wk)
      do j=1,nazim_wk
        write(61,'(20f6.1)')(strct_in_vel%field(i,j),i=1,920)
      end do
      close(61)
!     call system('pltv_c.x')
!     call system('mv wppi.eps ppiv0_trans.eps')


      open(61,file='input.dat')
      write(61,'(15f5.1)')(strct_in_ref%azim(i),i=1,nazim_wk)
      do j=1,nazim_wk
         write(61,'(20f6.1)')(strct_in_ref%field(i,j),i=1,230)
      end do
      close(61)
!     call system('pltref_c.x')
!     call system('mv ppiz.eps  ppiz0_trans.eps')


      open(61,file='input_mbsh.dat')
      write(61,'(15f5.1)')(strct_in_vel%azim(i),i=1,nazim_wk)
!      write(61,'(15f5.1)')(i*1.0,i=1,nazim_wk)
      do j=1,nazim_wk
         write(61,'(20f6.1)')(mbsh_out(i,j)*100,i=1,920)
      end do
      close(61)
!     call system('plt_mbsh')
!     call system('mv apmbsh.eps apmbsh_'//cc//'.eps')
     endif


   return
   end


     
