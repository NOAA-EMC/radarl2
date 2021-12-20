
// ########################################################################
//
//  produce_VIL.cc : produce VIL product and output it in binary,
//                 netcdf formats etc. 
//
// ########################################################################
//
//  Author: Shunxin Wang (CIMMS/NSSL)
//  Nov. 30, 2003
//
//  Input : DBZ reflectivity 3D array: ref, CONFIG_PARS object: pp, 
//           VIL array: vil 
//
//  Output: accmulated VIL array:vil and VIL density array: vilD
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


void produce_VIL_and_VILD_new(CONFIG_PARS &pp, float*** ref, 
                   float **vil, float **vilD, float **terrain) 
{
  //declare and initialize some variables
  const float puissance= 4.0/7.0;
  float temp_vil,total_height,temp_ref; 
//  float height_diff[pp.nz];
  float height_diff[31];
  int index = 0;
  int i,j,k;

 // short flag[pp.ny][pp.nx];
    short **flag;
 //short iterain[pp.ny][pp.nx];
    short **iterain;

  //declare and initialize above varibles
  

 // for ( j=0; j < pp.ny; j++)
 // for ( i=0; i < pp.nx; i++)
 //     flag[j][i] = -1;

  flag = new short * [pp.ny];
  iterain = new short * [pp.ny];
  for(j=0; j<pp.ny; j++)
  {
     flag[j]  = new short [pp.nx];
     iterain[j] = new short [pp.nx];
     for( i = 0; i < pp.nx; i++)
     {
          flag[j][i] = -1;
          iterain[j][i] = 0;
     }
  }



  for(j=0; j<pp.ny; j++)
  for( i = 0; i < pp.nx; i++)
     {
          vil[j][i] = vil_miss;
          vilD[j][i] = vilD_miss;
          terrain[j][i] = 0.0;
     }


  // go through the heights 
  // and set up the height difference array
  height_diff[0] = pp.zp[0] - 0.0;
  for (int i=1; i < pp.nz; i++)
    height_diff[i] = pp.zp[i] - pp.zp[i-1];

  //get iterain array
  for ( j=0; j < pp.ny; j++)
  for ( i=0; i < pp.nx; i++)
  {
     for ( k=0; k < pp.nz; k++)
     {
        // if( gv.hterain[j][i] <= pp.zp[k] )
           if(terrain[j][i] <= pp.zp[k] )
         {
            iterain[j][i] = k;  
            break;
         }
         if( k == pp.nz -1 )  
            iterain[j][i] = k;
     }
  }  

  //loop through all grids
  for ( j=0; j < pp.ny; j++)
  for ( i=0; i < pp.nx; i++)
  {
      //get lowest position of each grid point   
      for ( k=0; k < pp.nz; k++)
      if(ref[k][j][i] > 5.0)
      {
         flag[j][i] = k;
         break;
      }

      if( flag[j][i] == -1 )
          continue;

      index = iterain[j][i];
      temp_vil = 0.0;  
      total_height = 0.0;
      for ( k = pp.nz - 1; k >= index; k--)
      {
         temp_ref = ref[k][j][i];
         if( temp_ref <= 5.0 && k > flag[j][i] )
             continue;
         else if( temp_ref <= 5.0 && k <= flag[j][i] )    
                temp_ref = ref[flag[j][i]][j][i];    
         if ( temp_ref > 55.0 )
             temp_ref = 55.0;
         //transfer temp_ref back to reflectivity from DBZ
         temp_ref = pow(10.0,(temp_ref/10.0));
          
         //accumulate vil and height 
         temp_vil += pow( temp_ref,puissance)* height_diff[k];
         total_height += height_diff[k];
      }

      //compute vil and vilD of each grid 
      temp_vil = 0.00000344*temp_vil;
      if( temp_vil > 0.0)
      {
          vil[j][i] = temp_vil;
          //vil[j][i] = 10*log(temp_vil);
          if( total_height > 0.0)
             vilD[j][i]= 1000*temp_vil/total_height;
      }
  }


  return;
}

