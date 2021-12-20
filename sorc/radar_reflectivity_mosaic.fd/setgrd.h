/* ##########################################################################
 *      setgrd.h:  Declaring class setgrd
 *
 *      Author: Jian Zhang (CIMMS/NSSL),Wenwu Xia (CIMMS/NSSL)
 *      July 10, 2000
 *
 *      Modification History:
 *
 * ########################################################################*/



//
//     ##################################################################
//     ##################################################################
//     ######                                                      ######
//     ######                CLASS SETGRD                          ######
//     ######                                                      ######
//     ##################################################################
//     ##################################################################
//
#ifndef SETGRD_H
#define SETGRD_H       

#include "CONFIG_PARS.h"
#include "maplib3d.h"
//      class setgrd
//      {
//
//#######################################################################
//
//     PURPOSE:
//
//     Set up a 2D Cartesian grid mesh for a set of given configuration 
//     parameters such as lat/lon of the center of the grid domain,
//     grid resolution, map projection, etc.
//
//#######################################################################
//
//     The structure of this program is as follows:
//
//     1. Get the map projection information.
//        (call subroutine setmapr)
//
//     2. Get the absolute coordinates of the model grid origin on map
//        grid with the origin at north pole.
//        (call subroutine lltoxy)
//
//     3. Set up the model origin.
//        (call subroutine setorig)
//
//#######################################################################
//
//     INPUT:
//
//       nx       Number of grid points for the model
//                grid in the east-west direction.
//       ny       Number of grid points for the model
//                grid in the north-south direction.
//
//     OUTPUT:
//
//       x        x-coordinates of the analysis grid points in the
//                e-w direction (meters from the SW corner of the grid)
//       y        y-coordinates of the analysis grid points in the
//                s-n direction (meters from the SW corner of the grid)
//      include 'mosaic_par.inc'
//
//#######################################################################
//

class setgrd
{
    int nx;     
    int ny;     

public:
      float *x;           //x-coord.(degrees for LAT/LON map
                          //projection meters, otherwise) of
                          //the physical grid.
      float *y;           //y-coord.(degrees for LAT/LON map
                          //projection meters, otherwise) of
                          //the physical grid.

public:

      setgrd(int mm,int nn){

        nx=mm;
        ny=nn;
        x = new float[nx];
        y = new float[ny];
      }

      ~setgrd(){
        delete [] x;
        delete [] y;
      }

      void set_grd (setmapr &sm, CONFIG_PARS &pp);
};

#endif
