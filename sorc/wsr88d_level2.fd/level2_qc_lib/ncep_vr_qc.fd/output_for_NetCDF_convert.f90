!===========================================================================
!Purepose:
!  Output formatted data for converter of NetCDF 
!
!author:Pengfei Zhang  
!
!revision:
!	simplified by Xiaoyong Xu  
!       time  :11/29/2006 

!=========================================================================

subroutine output_for_NetCDF_convert(nelv_ref, strct_in_ref, strct_in_vel) 
use sdata1

implicit none
integer :: nelv_ref
type(ccout_strct),dimension(nelv_ref) :: strct_in_ref,strct_in_vel 
integer i, j


real,dimension(:),allocatable   ::  phi1d
real,dimension(:,:),allocatable :: ref2d
real,dimension(:,:),allocatable :: vel2d

real :: gatelgth
integer :: ii,jj


character(len=35) outfileName

!--------output formatted data for converter of NetCDF----------------------------------

    do i=1,nelv_ref

    gatelgth= strct_in_ref(i)%gateWidth(10)

print*,'strct_in_ref(i)%radar_name=',strct_in_ref(i)%radar_name
print*,strct_in_ref(i)%radlat, strct_in_ref(i)%radlon,  strct_in_ref(i)%radhgt

      write(outfileName,'(a,a,i4,2i2,a,3i2,a,i2,a)')         &
                   trim(strct_in_ref(i)%radar_name)     ,    &
                   '_', strct_in_ref(i)%year,                &
                        strct_in_ref(i)%month,               &
                        strct_in_ref(i)%day,                 &
                   '_', strct_in_ref(i)%hour,                & 
                        strct_in_ref(i)%minute,              &
                        strct_in_ref(i)%second,              &
                   '_ELE',i,'.Z.fmtdat'
       
          do j = 1, 30
           if(outfileName(j:j).eq.' ') outfileName(j:j) ='0'      
          enddo 

print*,outfileName

       open(21,file=outfileName,form='formatted',status='unknown')
        write(21,'(a4)') strct_in_ref(i)%radar_name
        write(21,'(i8)') strct_in_ref(i)%vcpnum
        write(21,'(6i8)') strct_in_ref(i)%year,&
                          strct_in_ref(i)%month,&
                          strct_in_ref(i)%day,&
                          strct_in_ref(i)%hour,&
                          strct_in_ref(i)%minute,&
                          strct_in_ref(i)%second
        write(21,'(3f8.3)') strct_in_ref(i)%radlat, &
                            strct_in_ref(i)%radlon, &
                            strct_in_ref(i)%radhgt 
        write(21,'(2f8.1)') strct_in_ref(i)%fstgatdis,&
                            gatelgth 
        write(21,'(f8.1)') strct_in_ref(i)%elev_angle
        write(21,'(2i8)') strct_in_ref(i)%num_beam,strct_in_ref(i)%num_gate

        allocate( phi1d(strct_in_ref(i)%num_beam) )
        allocate( ref2d(strct_in_ref(i)%num_gate,strct_in_ref(i)%num_beam) )

         phi1d=strct_in_ref(i)%azim(1:strct_in_ref(i)%num_beam)
         ref2d=strct_in_ref(i)%field(1:strct_in_ref(i)%num_gate,1:strct_in_ref(i)%num_beam)
        write(21,'(15f6.1)') (phi1d(jj),jj=1,strct_in_ref(i)%num_beam)
        write(21,'(20f6.1)') ((ref2d(ii,jj),ii=1,strct_in_ref(i)%num_gate),jj=1,strct_in_ref(i)%num_beam)
      close(21)

        deallocate(phi1d,ref2d)



    gatelgth= strct_in_vel(i)%gateWidth(10)

          outfileName(28:28) = 'V'

       open(22,file=outfileName,form='formatted',status='unknown')
        write(22,'(a4)') strct_in_vel(i)%radar_name
        write(22,'(i8)') strct_in_vel(i)%vcpnum
        write(22,'(6i8)') strct_in_vel(i)%year,&
                          strct_in_vel(i)%month,&
                          strct_in_vel(i)%day,&
                          strct_in_vel(i)%hour,&
                          strct_in_vel(i)%minute,&
                          strct_in_vel(i)%second
        write(22,'(3f8.3)') strct_in_vel(i)%radlat, &
                            strct_in_vel(i)%radlon, &
                            strct_in_vel(i)%radhgt   
        write(22,'(2f8.1)') strct_in_vel(i)%fstgatdis,&
                            gatelgth
        write(22,'(f8.1)') strct_in_vel(i)%elev_angle
        write(22,'(2i8)') strct_in_vel(i)%num_beam,strct_in_vel(i)%num_gate

        allocate( phi1d(strct_in_vel(i)%num_beam) )
        allocate( vel2d(strct_in_vel(i)%num_gate,strct_in_vel(i)%num_beam) )

         phi1d=strct_in_vel(i)%azim(1:strct_in_vel(i)%num_beam)
         vel2d=strct_in_vel(i)%field(1:strct_in_vel(i)%num_gate,1:strct_in_vel(i)%num_beam)
        write(22,'(15f6.1)') (phi1d(jj),jj=1,strct_in_vel(i)%num_beam)
        write(22,'(20f6.1)') ((vel2d(ii,jj),ii=1,strct_in_vel(i)%num_gate),jj=1,strct_in_vel(i)%num_beam)
      close(22)

         deallocate(phi1d,vel2d)

        end do ! i loop

!!!!!!End  for output formatted data for netCDF converter switch

return
end subroutine output_for_NetCDF_convert
