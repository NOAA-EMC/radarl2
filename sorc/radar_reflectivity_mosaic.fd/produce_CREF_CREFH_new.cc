
// ########################################################################
//
//  produce_CREF_CREFH.cc : produce CREF and CREFH products 
//                     and output them in NIDS and netcdf formats.
//
// ########################################################################
//
//  Author: Jian Zhang (CIMMS/NSSL)
//  June 2, 2004
//
//  Input : DBZ reflectivity 3D array: ref, CONFIG_PARS object: pp,
//
//  Output: 2D composite reflectivity array CREF, and 2D height array: CREFH
//
//  Modification History:
//  6/2/2004  Jian Zhang
//  Removed all binary output for efficiency and easy maintenance.  
//  Now only support NIDS and NetCDF format.
//
//  2/10/2005  Jian Zhang
//  Fixed a minor bug which cause incorrect scale when output CREFH in NIDS format.
//
//  3/15/2006  Carrie Langstion (CIMMS/NSSL)
//   Added binary output for cref and chgt.
//   Added LDM option for binary output
//   Added 3 new parameters (gv, nradar_has_data, radarnam) for binary output
//
//##########################################################################

#include <iostream>
#include <cmath>
#include <cstdio>
 
#include "CONFIG_PARS.h"
#include "mosaic_adapt_pars.h"
#include "func_prototype.h"
#include "nids_output.h"

#define CREF_THRESHOLD 0.0

using namespace std;

void produce_CREF_CREFH_new (CONFIG_PARS &pp, float *** ref,
                         float** CREF, float** CREFH ) 
{

//######################################################################
//
// Declare and initialize composite refl and the height variables
//
//######################################################################

  float temp_height,temp_ref; 

  int i,j,k;


//######################################################################
//
// Derive composite refl and the height 
//
//######################################################################

  for (j=0; j < pp.ny; j++)
  for (i=0; i < pp.nx; i++)
  {
      temp_height = (float) crefh_miss;
      temp_ref    = (float) ref_miss;
      for (k=0; k < pp.nz; k++)
      {
          if ( ref[k][j][i] > temp_ref + 0.01 )
          {
            temp_ref = ref[k][j][i];
            if (temp_ref>CREF_THRESHOLD) temp_height = pp.zp[k]/KM_TO_M;
            //temp_height = pp.zp[k]/KM_TO_M;
          }
      }
 
      CREF[j][i]   = temp_ref;
      CREFH[j][i]  = temp_height;
  }


  return;
}
