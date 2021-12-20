
// ########################################################################
//
//  produce_HSR_and_HSRH.cc : produce HSR and HSRH products 
//                     and output them in binary, netcdf formats etc.
//
// ########################################################################
//
//  Author: Shunxin Wang (CIMMS/NSSL)
//  Nov. 30, 2003
//
//  Input : DBZ reflectivity 3D array: ref, CONFIG_PARS object: pp,
//
//  Output: 2D HSR array: HSR and 2D HSRH array: HSRH
//
//  Modification History:
//  4/1/2004  Jian Zhang
//  Changed the unit of the HSRH from "km MSL" to "mAGL".
//
//  3/15/2006  Carrie Langston (CIMMS/NSSL)
//   Added LDM option for binary output
//
//  7/24/2006  Carrie Langston (CIMMS/NSSL)
//   Redefined LCREF from 4 kMSL to kmAGL
//
//##########################################################################

#include <iostream>
#include <cmath>
#include <cstdio>
 
#include "CONFIG_PARS.h"
#include "mosaic_adapt_pars.h"
#include "func_prototype.h"

#define LCREF_THRESHOLD 0.0

using namespace std;

void produce_LCREF_and_LCREFH_new(CONFIG_PARS &pp, float*** ref, 
                  float** LCREF, float** LCREFH)
{
  //declare and initialize some variables
  float temp_height,temp_ref; 
  int i,j,k;
  int highest_level = 0;
  int tmp_terrain;

  
  //loop through all grid points 
  for (j=0; j < pp.ny; j++)
  for (i=0; i < pp.nx; i++)
  {
    //get highest level value for this grid point
   // if(gv.hterain[j][i] == -999) tmp_terrain = 0;
   // else tmp_terrain = gv.hterain[j][i];
      tmp_terrain = 0;  

 
    highest_level = 0;
    for ( k=0; k < pp.nz; k++ )
    {      
      if( (pp.zp[k] - tmp_terrain) >= pp.lcref_max_hgt)
      {
        highest_level = k+1;
        break;
      }
    }
    if(highest_level >= pp.nz) highest_level = pp.nz - 1;
      
      
    temp_height = (float)crefh_miss;
    temp_ref    = (float)ref_miss;
      
    for (k=0; k < highest_level; k++)
    {
      if ( ref[k][j][i] > temp_ref )
      {
        temp_ref = ref[k][j][i];
        if ( temp_ref > LCREF_THRESHOLD) temp_height = pp.zp[k];
      }
    }
    
    LCREF[j][i]   = temp_ref;
    LCREFH[j][i]  = temp_height;
      
    if(temp_height != crefh_miss) LCREFH[j][i]  /= KM_TO_M;
  }

  return;
}

