
// ########################################################################
//
//  produce_ETP.cc : produce ETP product and output it in binary,
//                     netcdf formats etc. 
//
// ########################################################################
//
//  Author: Shunxin Wang (CIMMS/NSSL)
//  Nov. 30, 2003
//
//  Input :  DBZ reflectivity 3D array: ref, CONFIG_PARS object: pp,
//
//  Output: 2D ETP array: ETP
//
//  Modification History:
//   3/4/2006  Carrie Langstion (CIMMS/NSSL)
//    Changed the ordering of ref from [k][j][i] to [k][i][j]
//
//##########################################################################

#include <iostream>
#include <cmath>
#include <cstdio>
 
#include "CONFIG_PARS.h"
#include "mosaic_adapt_pars.h"
#include "func_prototype.h"

using namespace std;

void produce_ETP_new(CONFIG_PARS &pp, float*** ref, float ** ETP) 
{
  //declare and initialize some variables
   float ref0, temp, frac,temp_height; 
   int i,j,k;


   //loop through all grid points
   for (j=0; j < pp.ny; j++)
   for (i=0; i < pp.nx; i++)
   {
      temp_height = (float)etp_miss;
      ref0 = (float)ref_miss; 
      for (k= pp.nz-1; k >= 0; k--)
      {
          ref0 = ref[k][j][i];
          if ( ref0 < (float)ref_miss + 0.01 )
             continue;
          if( ref0 >= ETP_threshold )  
          {
            temp_height = pp.zp[k];
            break;
          }
      }

      if( temp_height > (float)etp_miss + 0.01 )
      {
          if( k < pp.nz-1 )
          {
             temp = ref[k+1][j][i];
             if( temp < 0)  temp = 0.0;   
             frac = 0.0;
             frac = (ref0 - ETP_threshold)/(ref0 - temp);
             frac = fabs(frac);
             ETP[j][i] = temp_height + frac*(pp.zp[k+1] - pp.zp[k]);
          }
          else 
             ETP[j][i] = temp_height + 0.5*(pp.zp[k] - pp.zp[k-1]);
      }
      else ETP[j][i] = temp_height;
   }


  return;
}
