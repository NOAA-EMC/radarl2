// ##########################################################################
//
//  get_time_realtime.cc
//
// ##########################################################################
//
//  Purpose: The function to convert user-specified calendar
//           time into absolute seconds from 1-1-1970.
//
// ##########################################################################
//
//  Author: Jian Zhang (CIMMS/NSSL),  Wenwu Xia (CIMMS/NSSL)
//  Nov. 12, 2001
//
//  Modification History:
//  10/02/03  Jian Zhang (CIMMS/NSSL)
//  Added documentation.
//  Added an option to use the mean time of all volume scans as the
//  time stamp for product files.

//######################################################################

#include <ctime>
#include <string>
#include <fstream>
#include <iostream>
#include <cstdio>

#include "CONFIG_PARS.h"
#include "func_prototype.h"
#include "FuncSet.h"

using namespace std;

void get_time_realtime (CONFIG_PARS &pp,int &gmt_m_lcl, int &itime0)
{

//######################################################################
//
//  GMT and Local time difference.
//
//######################################################################

    time_t  now, now_gmt;
    now = time(NULL);

    struct tm *gmtime0 = gmtime(&now);

    cout<<"\n    Process runs at (UTC): "<<gmtime0->tm_mon+1<<'/'
        <<gmtime0->tm_mday <<'/' <<gmtime0->tm_year+1900<<','
        << gmtime0->tm_hour<<':'<<gmtime0->tm_min<<endl<<endl;

    if (pp.prod_timestamp_opt==0) 
        now = (now/pp.mosaic_freq)*pp.mosaic_freq;

    gmtime0 = gmtime(&now);

    gmtime0->tm_isdst   = -1;
    now_gmt = mktime (gmtime0);

    gmt_m_lcl = now_gmt - now;

//######################################################################
//
//  Setup product time for real-time mode
//
//######################################################################

    if( pp.arc_opt )
    {
       struct tm *gmt_time1 = gmtime(&now);
       gmt_time1->tm_sec = 0;
       gmt_time1->tm_min = pp.min;
       gmt_time1->tm_hour = pp.hr;
       gmt_time1->tm_mday = pp.day;
       gmt_time1->tm_mon  = pp.mo;
       gmt_time1->tm_year = pp.yr-1900;
       gmt_time1->tm_isdst = -1;

       gmt_m_lcl = 0;
       itime0= Make_Time(gmt_time1);
       pp.epoch_seconds = itime0 - gmt_m_lcl;
       pp.UTC_seconds = itime0;
    }
    else 
    {
       pp.min = gmtime0->tm_min;
       pp.hr = gmtime0->tm_hour;
       pp.day = gmtime0->tm_mday;
       pp.mo = gmtime0->tm_mon + 1;
       pp.yr = gmtime0->tm_year + 1900;
       pp.epoch_seconds = now_gmt-gmt_m_lcl;
       itime0 = static_cast<int>(now_gmt);
       pp.UTC_seconds = itime0;
    }
}

