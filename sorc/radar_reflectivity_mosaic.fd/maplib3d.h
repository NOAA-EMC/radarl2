#ifndef MAPLIB3D_H
#define MAPLIB3D_H

#include <iostream>
#include <cmath>
#include <cstdlib>

#include "phycst.h"
#include "CONFIG_PARS.h"

#define map_scale_fct 1.0

//     ##################################################################
//     ######                                                      ######
//     ######                maplib3d.h                            ######
//     ######                                                      ######
//     ##################################################################
//
//#######################################################################
//
//    PURPOSE:
//    This file include several classes related to map projection.
//
//    Author:  Jian Zhang
//    11/02/1998
//    
//    Modification History:
//    12/01/1999  Leilei Wang
//    Convert from Fortran to C++.
//
//    06/08/2000  Jian Zhang
//    Added sevaral destructors.
//    Documentation cleanup.
//
//#######################################################################



//     ##################################################################
//     ######                                                      ######
//     ######                CLASS SETMAPR                         ######
//     ######                                                      ######
//     ##################################################################
//

class setmapr
{

//#######################################################################
//
//    PURPOSE:
//
//     Set constants for map projections.
//
//#######################################################################
//
//     INPUT:
//
//       iproj        Map projection number
//                    1=North Polar Stereographic   (-1 South Pole)
//                    2=Northern Lambert Conformal  (-2 Southern)
//                    3=Mercator
//                    4=Lat,Lon
//
//       scale        Map scale factor,  at latitude=latnot
//                    Distance on map = (Distance on earth) * scale
//                    For ARPS model runs, generally this is 1.0
//                    For ARPS plotting this will depend on window
//                    size and the area to be plotted.
//
//       latnot(2)    Real "True" latitude(s) of map projection
//                    (degrees, positive north)
//                    Except for iproj=1, only latnot(1) is used 
//
//       orient       Longitude line that runs vertically on the map.
//                    (degrees, negative west, positive east)
//
//#######################################################################


  public:
    int jproj,jpole;
    float trulat1,trulat2,rota,scmap,xorig,yorig;
    float projc1,projc2,projc3,projc4,projc;

    setmapr(){}
    ~setmapr(){}
    void setmap_proj(CONFIG_PARS &ipars);
};


void getmapr(int &iproj,float &scale,float *latnot,float &orient,
                   float &x0,float &y0,  setmapr &mp);


void lltoxy_func(float rrlat,float rrlon,float &xloc,float &yloc,setmapr& a);

void xytoll_func(float **rlat,float **rlon,float *x,float *y,
     int idim, int jdim,setmapr &m);







//     ##################################################################
//     ##################################################################
//     ######                                                      ######
//     ######                CLASS SETORIG                         ######
//     ######                                                      ######
//     ##################################################################
//     ##################################################################
//
      class setorig
      {
//
//#######################################################################
//
//     PURPOSE:
//
//     Set the origin for the map projection.
//     This is call after subroutine mapproj if the origin
//     must be moved from the original position, which is the
//     pole for the polar stereographic projection and the
//     Lambert conformal, and the equator for Mercator.
//
//#######################################################################
//
//     INPUT:
//
//       iopt        origin setting option
//                   1: origin given in corrdinate x,y
//                   2: origin given in lat,lon on earth
//
//       x0          first coordinate of origin
//       y0          second coordinate of origin
// 
//
//#######################################################################
//
//     Variable Declarations.
//
//#######################################################################

      int iopt;       // origin setting option
      float x0;            // first coordinate of origin
      float y0;            // second coordinate of origin
public:
        setorig(int opt,float x1,float y1){
          iopt=opt;
          x0=x1;
          y0=y1;
        }
      void set_orig(setmapr &s);
};
     

class mapprojConst{
  public:
    float d2rad;
    float eradius;
    float r2deg;
    mapprojConst(){
      d2rad=3.141592654/180.;
      eradius= 6371000.;
      r2deg=180./3.141592654;
    }
};


#endif
