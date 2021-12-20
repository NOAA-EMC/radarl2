!// ##########################################################################
!// readTiltData.cc: Function to get in one tilt of polar grid refl. data
!//                  from VOLUME_BASE_REFLECT object and do synchronization
!//                  between different tilt's data
!// ##########################################################################
!// Author: Shunxin Wang (CIMMS/NSSL)
!// Feb 20, 2005
!// ##########################################################################

!#include <iostream>
!#include <fstream>
!#include <cstdio>
!#include <cstdlib>
!#include <string>
!#include <cmath>
!#include <zlib.h>

!#include "CONFIG_PARS.h"
!#include "VOLUME_BASE_REFLECT.h"

!#define C_VEC_S         const std::vector< short >
!#define DELV_MARGIN 0.1

!using namespace std;

      SUBROUTINE readTiltData(vbr,refl_polar,thdr,numrows,numcols)
       
       use reflectivity
       use CONFIG_PARS
       implicit none

       type (ccout_strct_src)       , intent(in)  :: vbr
       integer                      , intent(in)  :: numrows,numcols   
       type (tilt_data_header)      , intent(out) :: thdr
       integer, intent(out) :: refl_polar(numrows*numcols)

       integer    index2,i,j


       thdr.radarname        = vbr.radar_name
       thdr.hgtrad           = vbr.radhgt
       thdr.latrad           = vbr.radlat
       thdr.lonrad           = vbr.radlon
       thdr.year             = vbr.year
       thdr.month            = vbr.month
       thdr.day              = vbr.day
       thdr.hour             = vbr.hour
       thdr.minute           = vbr.minute
       thdr.second           = vbr.second
       thdr.vcpmode          = vbr.vcpnum
       thdr.nrays            = vbr.num_beam
       thdr.ngates           = vbr.num_gate
       thdr.elv_angle        = vbr.elev
       thdr.azm1             = vbr.azim(1)
       thdr.azm_spc          = vbr.azim_spc
       thdr.gate1            = vbr.fstgatdis
       thdr.gate_spc         = vbr.gate_spc
       thdr.data_scale       = int(vbr.scale_value)
       thdr.data_missing_flag= int(vbr.missing_data)
       thdr.missing_refl     = real(thdr.data_missing_flag)

!    //get synchronization parameters
!    CONFIG_PARS* pp = vbr.get_config_Ptr();
!    long epoch_seconds = (long)vbr.get_QPETIME_vector()[tilt0].get_epoch_seconds();
!    pp->time_diff = (int)(pp->epoch_seconds - epoch_seconds);

!!!  Loop through this tilt, fill the appropriate values

       index2  = 0

       do j = 1, numrows, 1
       do i = 1, numcols, 1

        index2 = index2 + 1
        refl_polar( index2 ) = int(vbr.field_data(j,i))

!        if(index2.eq.10000)    print*,j,i
!        if(index2.eq.20000)    print*,j,i

       enddo
       enddo

!        print*,"Index2 = ", index2
 
       RETURN

      END SUBROUTINE readTiltData 

