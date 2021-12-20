!// ##########################################################################
!//
!//  get_radattr_realtime.cc
!//
!// ##########################################################################
!//  Purpose:  get time refference etc
!// ##########################################################################
!//
!//  Author: Shunxin (CIMMS/NSSL)
!//  Feb. 20, 2005
!//######################################################################

!#include <ctime>
!#include <string>
!#include <cstdio>

!#include "CONFIG_PARS.h"
!#include "VOLUME_BASE_REFLECT.h"

!using namespace std;

      SUBROUTINE get_radattr_realtime(pp,vbr, timeString,nc_timeString)

       use reflectivity
       use CONFIG_PARS
       implicit none

       type (ccout_strct_src)            vbr
       type (CCOUT_CONFIG_PARS)          pp
          
       character*20  timeString       
       character*20  nc_timeString  

       write(timeString,10)    vbr%year,    & 
                               vbr%month,   &
                               vbr%day,     &
                               vbr%hour,    &
                               vbr%minute  
10     format(i4.4,i2.2,i2.2,'.',i2.2,i2.2)


       write(nc_timeString,20) vbr%year,    &
                               vbr%month,   &
                               vbr%day,     &
                               vbr%hour,    &
                               vbr%minute   
20     format(i4.4,i2.2,i2.2,'_',i2.2,i2.2)
      
!     get year,month,day etc.

       pp.sec    = vbr%second
       pp.minute = vbr%minute
       pp.hr     = vbr%hour
       pp.day    = vbr%day
       pp.mo     = vbr%month
       pp.yr     = vbr%year


!      GMT and Local time difference and get UTC seconds.

!      pp.UTC_seconds = epoch_seconds + gmt_m_lcl;

       RETURN

      END SUBROUTINE get_radattr_realtime

