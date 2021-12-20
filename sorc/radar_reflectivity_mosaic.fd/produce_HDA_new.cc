
// ########################################################################
//
//  produce_HDA.cc : produce HDA product like SHI, POSH and MEHS
//                   and output them in binary, netcdf formats etc.
//
// ########################################################################
//
//  Author: Shunxin Wang (CIMMS/NSSL)
//  Nov. 30, 2003
//
//  Input :  DBZ reflectivity 3D array: ref, CONFIG_PARS object: pp,
//           index_temperature 3D array: i_t.
//
//  Output: 2D SHI array: shi, 2D POSH array: posh, 2D MEHS array: mehs.
//
//  Modification History:
//   3/4/2006  Carrie Langstion (CIMMS/NSSL)
//    Changed the ordering of ref from [k][j][i] to [k][i][j]
//
//##########################################################################

#include <iostream>
#include <cmath>
#include <cstdio>
#include <string>
 
#include "CONFIG_PARS.h"
#include "mosaic_adapt_pars.h"
#include "func_prototype.h"

using namespace std;

void produce_HDA_new(CONFIG_PARS &pp, float*** ref, float ***kh_tlvls,  
                 float **shi, float **posh, float ** mehs )            
{
  if(kh_tlvls == 0)
    cout<<"++WARNING: RUC temperature indices not available.  Will "
        <<"not be used in HDA (SHI) product"<<endl;


  //declare and initialize some variables
  float Adapt_DefaultHtOf273K = 3000;
  float Adapt_DefaultHtOf253K = 6000;
  float Zl = 40;
  float Zu = 50;
  float Zm = 75;
  float WT, ref0; 
  int kk; float frac;
  float i_273k, i_253k;
  float temp_shi, temp_posh, temp_mehs;
  float E,H273K,H253K;
  int i,j,k,index = 0;

/*
  i_t  = new float **[pp.nx];
  for(i = 0; i < pp.nx; i++)
  {
     i_t[i] = new float *[pp.ny];
     for(j = 0; j < pp.ny; j++)
        i_t[i][j] = new float[n_tlevels];
  }

  //get 3-D index_temperature array
  get_index_temp(i_t,itime0,pp,radarnam,gv,config_file_name,timeString);
*/
  // get the height difference
//  float height_diff[pp.nz];
    float height_diff[31]; 
  height_diff[0] = pp.zp[1] - pp.zp[0];
  for (int k=1; k < pp.nz; ++k)
     height_diff[k] = pp.zp[k] - pp.zp[k-1];

  //loop through all storm grid points
  for ( j=0; j < pp.ny; j++)
  for ( i=0; i < pp.nx; i++)
  {
      H273K = Adapt_DefaultHtOf273K;
      H253K = Adapt_DefaultHtOf253K;

      if(kh_tlvls != 0)//ensures kh_tlvl is valid
      {
        if( kh_tlvls[i][j][2] >= 0.0 )      
        {
          i_273k = kh_tlvls[i][j][2];
          kk = (int)i_273k;
          frac = i_273k - (float) kk;
          H273K = frac*pp.zp[kk+1] +(1.0-frac)*pp.zp[kk];
        }
      
        if( kh_tlvls[i][j][4] >= 0.0 )      
        { 
          i_253k = kh_tlvls[i][j][4];
          kk = (int)i_253k;
          frac = i_253k - (float) kk;
          H253K = frac*pp.zp[kk+1] +(1.0-frac)*pp.zp[kk];
        }
      }
      
      WT = (57.5 * H273K / 1000) - 121;
      if( WT < 20 )   WT = 20;
 
      temp_shi  =  0.0;
      temp_posh =  0.0;
      temp_mehs =  0.0;

      float WZ = 0.0;
      float WH = 0.0;

      for ( k=0; k < pp.nz; k++)
      {
          ref0 = ref[k][j][i];
          if ( ref0 < (float)ref_miss +0.01 )
             continue;
          if (ref0 <=Zl)
             WZ = 0;      
          else if (Zl< ref0 && ref0 < Zu)
             WZ = pow(10.0,(0.084*ref0))*(ref0-Zl)/(Zu-Zl);
          else if ( ref0>=Zu)
             WZ = pow(10.0,0.084*ref0)*1.0;  
          else if ( ref0>=Zm ) 
             WZ = pow(10.0,0.084*Zm)*1.0;
          else
             cout << " error!!!! No Point Value for WZ \n"<<" ref0 = "<<ref0<<endl;;

          E = 0.000005 * WZ;

          if ( pp.zp[k] <=H273K)
             WH = 0;
          else if( H273K < pp.zp[k] && pp.zp[k] < H253K )
             WH = ( pp.zp[k]-H273K)/(H253K-H273K);
          else if ( pp.zp[k] >= H253K )
             WH = 1;
          else
             cout << " error!!!!  No PointValue for WH\n";

          temp_shi += E * WH * height_diff[k];
      }
 
      if (temp_shi <= 1.0)
        temp_shi = (float)shi_miss;
      else
        temp_shi = 0.1*temp_shi ;
      
      // Compute POSH
      if (temp_shi <= 0)
        temp_posh = (float)posh_miss;
      else 
      {
        temp_posh = 29*log(temp_shi/WT) + 50;
        if (temp_posh<=0) temp_posh = (float)posh_miss;
        else if (temp_posh>=100) temp_posh = 100;
      }

      // Compute Mehs
      if (temp_shi <= 0)
        temp_mehs = (float)mehs_miss;
      else 
      {
        temp_mehs = 2.54 * (float)pow( (float)temp_shi, (float)0.5 );
        if (temp_mehs<=0) temp_mehs = (float)mehs_miss;
        else if (temp_mehs>=100) temp_mehs = 100; 
      }

      shi[j][i]  = temp_shi;
      posh[j][i] = temp_posh;
      mehs[j][i] = temp_mehs;
  }

  return;
}

