#include "setgrd.h"

using namespace std;


void lltoxy_func(float rrlat,float rrlon,float &xloc,float &yloc,setmapr& a);


void setgrd::set_grd(setmapr &sm, CONFIG_PARS &pp)
{

    float    dxscl;           // grid spacing (m) in x-direction 
                              // normalized by the map scale 
    float    dyscl;           // grid spacing (m) in y-direction 
                              // normalized by the map scale 
    float swx, swy;

//#######################################################################

    if( map_scale_fct!= 1.0){ 
      dxscl = pp.dx/map_scale_fct;
      dyscl = pp.dy/map_scale_fct;
    }
    else  {
      dxscl = pp.dx;
      dyscl = pp.dy;
    }

//#######################################################################
//
//     Find the x- and y-coordinates (i.e., ctrx and ctry, in meters) 
//     of the domain ceter point (ctrlat, ctrlon) with respect to 
//     the north pole, which is the origin.
//
//#######################################################################

    float xloc = 0.0, yloc = 0.0;
    lltoxy_func(pp.ctrlat,pp.ctrlon,xloc,yloc,sm); 


//#######################################################################
//
//     Find the x- and y-coordinates, in meters, of the south-west 
//     corner of the analysis domain (w.r.t. the north pole).
//     Define the SW corner as the new origin of the Cartesian grid
//     coordinate system.
//
//#######################################################################

    swx = xloc;
    swy = yloc;
    swx = swx - (float)(nx-1)/2. * dxscl;
    swy = swy - (float)(ny-1)/2. * dyscl;
    if(pp.mapproj==4)
    {
      for(int i=0;i<nx;i++) x[i] = swx + dxscl * float(i);
      for(int j=0;j<ny;j++) y[j] = swy + dyscl * float(j); 
    }

    else {
      setorig t( 1, swx, swy); 
      t.set_orig(sm);

//#######################################################################
//
//     Calculate the x- and y-coordinates for the rest
//     of the analysis grid points in earth meters*sclf ,  with the
//     SW corner as the origin.
//
//#######################################################################
     
      for(int i=0;i<nx;i++) x[i] = dxscl * float(i);
      for(int j=0;j<ny;j++) y[j] = dyscl * float(j); 
    }

};
