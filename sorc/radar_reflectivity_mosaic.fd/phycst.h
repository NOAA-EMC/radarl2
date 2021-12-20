/* ##########################################################################
 *
 * phycst.h:  Declaring physical constants. 
 *
 * Author: Jian Zhang (CIMMS/NSSL)
 * Sept. 15, 2001
 *
 * Modification History:
 *
 * ########################################################################*/


#ifndef PHYCST_H
#define PHYCST_H

#include <cmath>


//#######################################################################
//
//     Earth and other physical constants
//
//#######################################################################

    const float eradius=6.371e6; // meters
    const float d2rad=0.01745329252;
    const float r2deg=180./3.141592654;
    const float er_temp =eradius*d2rad;

    const float pi = 3.1415926;
    const float pi2r = 1.0/pi/pi;
  
#endif 
