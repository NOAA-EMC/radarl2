
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
#include <cstring>
 
#include "CONFIG_PARS.h"
#include "mosaic_adapt_pars.h"
#include "func_prototype.h"

using namespace std;

void produce_ETP(CONFIG_PARS &pp, float*** ref, int itime0, int nradar_has_data,
                  grid_vars &gv, char* timeString, char radarnam[][5],
                  short* i2tem2d )
{
  //declare and initialize some variables
   float ref0, temp, frac,temp_height; 
   float ** ETP;
   int index = 0;
   int i,j,k;

   ETP = new float * [pp.ny];
   for(j=0; j<pp.ny; j++) ETP[j] = new float[pp.nx];

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

  for(j=0; j<pp.ny; j++)
  for(i=0; i<pp.nx; i++)
  {
     index = j*pp.nx + i;
     if( ETP[j][i] < (float)etp_miss + 0.01)
        i2tem2d[index] = get_int((float)etp_miss*etp_scale);
     else
        i2tem2d[index] = get_int(0.001*ETP[j][i]*etp_scale);
  }

  //output ETP product in binary and netcdf format
  char vfname[200];
  char varname[20];
  char varunit[7];
  
  sprintf(vfname,"%s%s","ETP18.",timeString);
  strcpy(varname,"ETP");
  strcpy(varunit,"km");
  
  if(pp.bi_etp_flag)
  {  
     if(pp.debug_flag)          
          cout<<"Writing binary file:"<<vfname<<endl;
    
     i2wrtvar_cart3d(pp.bi_etp_dir,vfname,
               latlon_scale,gv.gridlon[0][pp.ny-1],
               gv.gridlat[0][pp.ny-1],latlon_scale,
               dlatdlon_scale,meter_scale,varname,
               varunit,nradar_has_data,radarnam,
               i2tem2d,etp_scale, etp_miss, pp,
               gv,pp.bb_mode,1);  

    //transfer etp data to nmqserver by using LDM
    if(pp.ldm_binary_opt == 1 )
    {
      char ldm_command[200];
      char fileName[200];
      sprintf(fileName,"%s/%s.gz", pp.bi_etp_dir, vfname);
      sprintf(ldm_command, "%s -v -l %s -q %s -f WMO %s", pp.pqinsert_file,
               pp.ldmd_log_file,pp.ldm_pq_file,fileName);
      if(pp.debug_flag == 1)
        cout<<"Implement "<<ldm_command<<endl;
      system(ldm_command);
    }   

  }

  for(j=0; j<pp.ny; j++)
      delete [] ETP[j];
  delete [] ETP;

  return;
}
