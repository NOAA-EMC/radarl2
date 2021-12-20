// ##########################################################################
//
//  get_radattr_realtime.cc
//
// ##########################################################################
//
//  Purpose:  For a user specified time, find all radar volume scans within
//            a cretain window from the time.  Then based on the user-specified
//            time and the radar data times, define the time stamp for products.
//
// ##########################################################################
//
//  Author: Jian Zhang (CIMMS/NSSL),  Wenwu Xia (CIMMS/NSSL)
//  Nov. 12, 2001
//
//  Modification History:
//  10/2/2003  Jian Zhang
//  Revised documentation.
//
//  Modified code to allow two options for defining product time stamps:
//    1)  use user-specified time (in config file)
//    2)  use the mean of observational data times
//
//######################################################################

#include <ctime>
#include <string>
#include <fstream>
#include <iostream>
#include <cstdio>

#include "CONFIG_PARS.h"
#include "func_prototype.h"

using namespace std;

void get_attr_realtime (CONFIG_PARS &pp,
                        int &itime0,
                        int gmt_m_lcl,
                        char timeString[],
                        char nc_timeString[])
{
    //Define product time stamp.
    if( pp.arc_opt < 1 )
       itime0 = (itime0/pp.mosaic_freq)*pp.mosaic_freq;

    if(pp.debug_flag==1) 
        cout<<"Product time:"<<pp.yr<<"-"<<pp.mo<<"-"<<pp.day<<"-"
            <<pp.hr<<":"<<pp.min<<":"<<pp.sec<<endl;
  
    time_t seconds_f70 = itime0 - gmt_m_lcl;
    strftime (timeString, 20, "%Y%m%d.%H%M", gmtime(&seconds_f70));
    strftime (nc_timeString, 20, "%Y%m%d_%H%M", gmtime(&seconds_f70));
    if(pp.debug_flag==1) 
      cout<<"Product timestamp:"<<timeString<<endl;
}
