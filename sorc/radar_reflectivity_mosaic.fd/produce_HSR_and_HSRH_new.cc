
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

void produce_HSR_and_HSRH_new(CONFIG_PARS &pp, float*** ref, 
                  float **HSR, float ** HSRH)
{
  //declare and initialize some variables
  float temp_height,temp_ref; 
  int i,j,k;


  //loop through all grid points 
  for (j=0; j < pp.ny; j++)
  for (i=0; i < pp.nx; i++)
  {
      temp_height = (float)hsrh_miss;
      temp_ref    = (float)hsr_miss;
      for (k=0; k < pp.nz; k++)
      {
          temp_ref = ref[k][j][i];

          if ( temp_ref > (float)hsr_miss + 0.01)
          {
            //Temporary fix for cases where SRC input
            //has error in field (missing value vs
            //no coverage value)
            if( (k < pp.nz-1) && 
                (temp_ref < (float)(-99.0 + 0.01) ) && 
                (ref[k+1][j][i] > (float)(-99.0)) )
            {
              continue;
            }
          
            if (k<pp.nz-1)
              temp_height = pp.zp[k] - 0.5*(pp.zp[k+1]-pp.zp[k]); //mAGL
            else temp_height = 0.5*(pp.zp[k]+pp.zp[k-1]);   //mAGL
          
           // if(temp_height<gv.hterain[j][i])
              if(temp_height < 0.) 
               continue;
            else break;
          }
      }
 
      HSR[j][i]   = temp_ref; 
   //  HSRH[j][i]   = temp_height - gv.hterain[j][i]; 
       HSRH[j][i]   = temp_height; 
      if(temp_height != hsrh_miss) HSRH[j][i] /= KM_TO_M;//kmAGL
  }


  return;
}

