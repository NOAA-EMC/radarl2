!     ##################################################################
!     ####                                                          ####
!     ####                SUBROUTINE alignment.f90                  ####
!     ####                                                          ####
!     ##################################################################
!
!-----------------------------------------------------------------------
!
!     PURPOSE:
!         This file is a fortran file designed to rearrange the radar
!         data along the clockwise from the north direction.
!
!-----------------------------------------------------------------------
!
!     AUTHOR: Xiaoyong XU
!     11/03/2006
!
!     MODIFICATION HISTORY:
!
!-----------------------------------------------------------------------
      subroutine alignment( missing_value                    &
                           ,NUM_RAYS, nray, ngate            &
                           ,azim                             &
                           ,field_in,field_out               &
                           ,dltphi)
!
      implicit none

      real             ::  missing_value 
      integer          ::  NUM_RAYS                    ! ray number to be set
      integer          ::  nray                        ! previous ray number
      integer          ::  ngate
      real             ::  azim(nray)                  ! previous azimuth angle      
      real             ::  field_in(ngate,nray)        ! previous field 
      real             ::  field_out(ngate,NUM_RAYS)   ! new field
      real             ::  dltphi                      ! new azimuth space

      real             ::  ray_set(NUM_RAYS)
      real             ::  top_azm_diff, bot_azm_diff
      integer          ::  top_ray_index,bot_ray_index
      integer          ::  i, j 

      do i= 1, NUM_RAYS
         ray_set(i) = 999.
      enddo

      do i= 1, NUM_RAYS
      do j= 1, ngate
         field_out(j,i) = missing_value
      enddo
      enddo


      do 10 j=1, nray, 1

          if ( azim(j) .ge. 0.0 .and. azim(j) .lt. 360.0 ) then
              bot_ray_index = int(azim(j)/dltphi) + 1
              top_ray_index = bot_ray_index + 1
      
              bot_azm_diff = (azim(j)/dltphi + 1) -  bot_ray_index
              top_azm_diff = top_ray_index     -  (azim(j)/dltphi + 1)

              if(bot_ray_index > NUM_RAYS)  bot_ray_index = mod(bot_ray_index,  NUM_RAYS)
              if(top_ray_index > NUM_RAYS)  top_ray_index = mod(top_ray_index,  NUM_RAYS)

              if(ray_set(bot_ray_index) > bot_azm_diff)  then

                ray_set(bot_ray_index) = bot_azm_diff

                do i =1, ngate 
                field_out(i,bot_ray_index) = field_in(i,j)          
                enddo
                
              endif


              if(ray_set(top_ray_index) > top_azm_diff)  then

                ray_set(top_ray_index) = top_azm_diff

                do i =1, ngate
                field_out(i,top_ray_index) = field_in(i,j)   
                enddo
                
              endif

!              print*,'Alignment: ', j, azim(j), bot_ray_index, top_ray_index, field_in(10,j), &
!                     field_out(10,bot_ray_index), field_out(10,top_ray_index)
      
         endif

10   continue

      return
      end subroutine alignment
