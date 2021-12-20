
// ########################################################################
//
//  produce_PCPFLAG.cc : produce PCP FLAG product
//                     and output in binary format.
//
// ########################################################################
//
//  Author: Carrie Langston (CIMMS/NSSL)
//  March 16, 2006
//
//  Input : DBZ reflectivity 3D array: ref, CONFIG_PARS object: pp,
//
//  Output: 2D array PCPFLAG 
//
//  Modification History:
//
//##########################################################################

#include <iostream>
#include <cmath>
#include <cstdio>
 
#include "CONFIG_PARS.h"
#include "mosaic_adapt_pars.h"
#include "func_prototype.h"
#include "nids_output.h"


using namespace std;

//main function
void produce_PCPFLAG_new (CONFIG_PARS &pp, float*** ref, float ***kh_tlvls,
                      short **PCPFLAG)                     
{

   if(kh_tlvls == 0)
     cout<<"++WARNING: RUC temperature indices not available.  Will "
         <<"not be used in pcp_flag product"<<endl;

//######################################################################
//
// Declare and initialize pcp flag and t_level variables
//
//######################################################################

  short temp_flag; 
  int height_index_m10C;

//  float ***i_t = 0;
  
  int i,j,k;

/*  
  i_t  = new float **[pp.nx];
  for(i = 0; i < pp.nx; i++)
  {
     i_t[i] = new float *[pp.ny];
     for(j = 0; j < pp.ny; j++)
        i_t[i][j] = new float[n_tlevels];
  }
*/
  

//######################################################################
//
// Read in height of -10C from RUC
//
//######################################################################
/*
   ruc = read_ruc_thgt(pp, pp.epoch_seconds, ruc_nx, ruc_ny, ruc_dx,
                       ruc_dy, ruc_nw_lat, ruc_nw_lon);
   
   if(ruc == 0) ruc_missing = true;
   else ruc_missing = false;
*/

  //get 3-D index_temperature array
  //get_index_temp(i_t,itime0,pp,radarnam,gv,config_file_name,timeString);


//######################################################################
//
// Derive pcp flag 
//
//######################################################################

  for (j=0; j < pp.ny; j++)
  for (i=0; i < pp.nx; i++)
  {
      //set default value
      //temp_flag = stratiform_flag;
      temp_flag = pcpflag_miss;
      
      
      //get index of RUC -10C height
      if( (kh_tlvls != 0) && (kh_tlvls[i][j][index_m10C] >= 0.0) )
        height_index_m10C = (int)kh_tlvls[i][j][index_m10C];     
      else
        height_index_m10C = pp.nz; //never reached
      
      
      //Loop through column
      for (k=0; k < pp.nz; k++)
      {
          if(ref[k][j][i] >= pp.strat_thresh)
            temp_flag = stratiform_flag;
            
          if(ref[k][j][i] >= pp.conv_thresh)
          {
            temp_flag = conv_flag;
            break;
          }
          else if( (k >= height_index_m10C) &&
                   (ref[k][j][i] >= pp.conv_thresh_m10c) )
          {
            temp_flag = conv_flag;
            break;            
          }
          
      }//end k-loop
 
      PCPFLAG[j][i]   = temp_flag;
      
  }//end i,j-loop


/*  
  for(i = 0; i< pp.nx; i++)
  {
     for(j = 0; j < pp.ny; j++) delete [] i_t[i][j];
     delete [] i_t[i];
  }
  delete [] i_t;
*/
  return;
}

