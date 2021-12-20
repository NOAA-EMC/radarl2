/* ##########################################################################
 *     grid_vars.h:  Declaring Grid geometric variables class
 *
 *      Author: Wenwu Xia (CIMMS/NSSL),Jian Zhang (CIMMS/NSSL)
 *      July 10, 2000
 *
 *      Modification History:
 *
 * ########################################################################*/


#ifndef GRID_VARS_H
#define GRID_VARS_H

#include <iostream>
#include <cstdio>
#include <string>
#include <cstdlib>

#include "CONFIG_PARS.h"
#include "maplib3d.h"
#include "setgrd.h"
#include "CONFIG_PARS.h"


//
//     ##################################################################
//     ##################################################################
//     ######                                                      ######
//     ######              grid_vars.h                             ######
//     ######                                                      ######
//     ##################################################################
//     ##################################################################
//
//#######################################################################
//
//     PURPOSE:
//
//     This file defines some common arrays on the Cartesian grid.
//
//     Author:  Jian Zhang
//     09/01/1998
//
//     Modification History:
//     12/01/1999  Leilei Wang
//     Convert from Fortran to C++
//
//     06/10/2000  Jian Zhang
//     Major modifications for memory allocation.  Added destructors.
//
//#######################################################################
//

//#######################################################################
//
//     Grid configuration variables
//
//#######################################################################

class grid_vars 
{

private:
    int nx, ny, nz;

public:
    float *x;           //x-coord.(degrees for LAT/LON map 
                        //projection meters, otherwise) of 
                        //the physical grid.
    float *y;           //y-coord.(degrees for LAT/LON map 
                        //projection meters, otherwise) of 
                        //the physical grid.
    float *z;           //z-coord. of the computational grid.

    float *zp;          //z-coord. (msl) of the physical grid.

    float **hterain;  //Terrain height (msl).
    float **gridlat;
    float **gridlon;

  public:

    grid_vars():x(0),y(0),z(0),zp(0), hterain(0), gridlat(0), gridlon(0){};


    grid_vars(int nx_in, int ny_in, int nz_in);

    ~grid_vars();

    void ini_grd (CONFIG_PARS &pp,setmapr &sm);

};

#endif
