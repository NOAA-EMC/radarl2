!/* ##########################################################################
! * dataSelect.cc: Find info from VOLUME_BASE_REFLECT object and 
! *                BRIGHTBAND object 
! * Author: Shunxin Wang (CIMMS/NSSL), Wenwu Xia (CIMMS/NSSL)
! * Feb 17, 2005
! * ########################################################################## */

!#include <iostream>
!#include <fstream>
!#include <cstdlib>
!#include <cstdio>
!#include <string.h>
!#include <zlib.h>

!#include "CONFIG_PARS.h"
!#include "VOLUME_BASE_REFLECT.h"
!#include "BRIGHTBAND.h"

      SUBROUTINE dataSelect(pp,vbr,bb_info,rd,                       &
                   ibb_mode, ibb_radar,bb_top_rad, bb_bot_rad )

       use reflectivity
       use BRIGHTBAND
       use CONFIG_PARS
       implicit none
     
       type (ccout_strct_src)            vbr
       type (CCOUT_BRIGHTBAND)           bb_info
       type (CCOUT_CONFIG_PARS)          pp
       type (radar)                      rd
  
       integer(2) ibb_mode
       integer(2) ibb_radar
       integer(2) bb_top_rad
       integer(2) bb_bot_rad


       rd.namrad  = vbr.radar_name
       rd.elvrad  = vbr.radhgt
       rd.latrad  = vbr.radlat
       rd.lonrad  = vbr.radlon
       rd.vcpmod  = int2(vbr.vcpnum)
       rd.vol     = 0                      !!!!!!!!!!

       
       print*,vbr.radar_name, vbr.radhgt, vbr.radlat, vbr.radlon, vbr.vcpnum

       ibb_mode   = int2(bb_info.bb_mode)
       ibb_radar  = int2(bb_info.volume_bb_exist)
       bb_top_rad = int2(bb_info.global_bb_top)
       bb_bot_rad = int2(bb_info.global_bb_bottom)

       pp.ctrlat  = rd.latrad
       pp.ctrlon  = rd.lonrad
       pp.trulat1 = rd.latrad
       pp.trulat2 = rd.latrad
       pp.trulon  = rd.lonrad

       RETURN

      END SUBROUTINE dataSelect
      

