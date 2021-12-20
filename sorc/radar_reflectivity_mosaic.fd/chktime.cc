/* ##########################################################################
 *      chktime.cc: Check time window 
 *
 *      Author: Jian Zhang (CIMMS/NSSL),Wenwu Xia (CIMMS/NSSL)
 *      July 17, 2001
 *
 *      Modification History:
 *
 * ########################################################################## */




#include <iostream>
#include <ctime>
#include <cstdlib>
#include <string.h>

#include "FuncSet.h"

using namespace std;


int chktime (char temp_string[20], int itime, int &itime1, int window, short df)
{
    char yrstr[5], mostr[3], daystr[3], hrstr[3], minstr[3];
    strncpy (yrstr, &temp_string[0], 4);
    yrstr[4] = '\0';
    strncpy (mostr, &temp_string[4], 2);
    mostr[2] = '\0';
    strncpy (daystr, &temp_string[6], 2);
    daystr[2] = '\0';
    strncpy (hrstr, &temp_string[9], 2);
    hrstr[2] = '\0';
    strncpy (minstr, &temp_string[11], 2);
    minstr[2] = '\0';

//######################################################################
//
//    Check if the data time is most current
//
//######################################################################

    time_t  now, itime_data;
    now = time(NULL);

    struct tm *data_time = gmtime(&now);

    data_time->tm_sec = 0;
    data_time->tm_min = atoi (minstr);
    data_time->tm_hour = atoi(hrstr);
    data_time->tm_mday = atoi(daystr);
    data_time->tm_mon = atoi(mostr);//-1;
    data_time->tm_year = atoi(yrstr)-1900;
    data_time->tm_isdst = -1;

    //itime_data = mktime (data_time);
    itime_data = Make_Time(data_time);
    itime1 = itime_data;

    if(abs(itime_data-itime) < window) {
      return 1;
    }
    else {
      if(df) cout<<"++WARNING  data is more than "
          <<window/60<<" minutes old." <<endl;
      return 0;
    }
}  //end of chktime

