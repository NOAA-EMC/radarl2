
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
#include <cstring>
 
#include "CONFIG_PARS.h"
#include "mosaic_adapt_pars.h"
#include "func_prototype.h"

using namespace std;


void produce_VIL_and_VILD(CONFIG_PARS &pp, float*** ref, int itime0,
                   int nradar_has_data,grid_vars &gv, char* timeString, 
                   char radarnam[][5], short **i2tem2d)
{
  //declare and initialize some variables
  const float puissance= 4.0/7.0;
  float temp_vil,total_height,temp_ref; 
//  float height_diff[pp.nz];
  float height_diff[31];
  float **vil, **vilD;
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



  vil = new float * [pp.ny];
  vilD = new float * [pp.ny];
  for(j=0; j<pp.ny; j++)
  {
     vil[j]  = new float[pp.nx];
     vilD[j] = new float[pp.nx];
     for( i = 0; i < pp.nx; i++)
     {
          vil[j][i] = vil_miss;
          vilD[j][i] = vilD_miss;
     }
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
         if( gv.hterain[j][i] <= pp.zp[k] )
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

  //transfer 2D array to 1D array
  for(j=0; j<pp.ny; j++)
  for(i=0; i<pp.nx; i++)
  {
     index = j*pp.nx + i;
     if( vil[j][i] < vil_miss + 0.01)
        i2tem2d[0][index] = get_int(vil_miss*vil_scale);
     else
        i2tem2d[0][index] = get_int(vil[j][i]*vil_scale);

     if( vilD[j][i] < vilD_miss + 0.01)
        i2tem2d[1][index] = get_int(vilD_miss*vilD_scale);
     else
        i2tem2d[1][index] = get_int(vilD[j][i]*vilD_scale);
  } 

  //output VIL product in binary and netcdf format
  char vfname[200];
  char varname[20];
  char varunit[7];

  sprintf(vfname,"%s%s","VIL.",timeString);  
  strcpy(varname,"VIL");
  strcpy(varunit,"kg/m2");

  if(pp.bi_vil_flag)
  {
     if(pp.debug_flag)          
          cout<<"Writing binary file:"<<vfname<<endl;

     i2wrtvar_cart3d(pp.bi_vil_dir,vfname,
               latlon_scale,gv.gridlon[0][pp.ny-1],
               gv.gridlat[0][pp.ny-1],latlon_scale,
               dlatdlon_scale,meter_scale,varname,
               varunit,nradar_has_data,radarnam,
               i2tem2d[0],vil_scale, vil_miss, pp, 
               gv,pp.bb_mode,1);          

      //transfer vil data to nmqserver by using LDM
      if(pp.ldm_binary_opt == 1 )
      {
        char ldm_command[200];
        char fileName[200];
        sprintf(fileName,"%s/%s.gz", pp.bi_vil_dir, vfname);
        sprintf(ldm_command, "%s -v -l %s -q %s -f WMO %s", pp.pqinsert_file,
                 pp.ldmd_log_file,pp.ldm_pq_file,fileName);
        if(pp.debug_flag == 1)
          cout<<"Implement "<<ldm_command<<endl;
        system(ldm_command);
      }   

  }

  //output VIL density product in binary and netcdf format
  sprintf(vfname,"%s%s","VILD.",timeString);
  strcpy(varname,"VILD");
  strcpy(varunit,"g/m3");

  if(pp.bi_vilD_flag)
  {
     if(pp.debug_flag)
          cout<<"Writing binary file:"<<vfname<<endl;

     i2wrtvar_cart3d(pp.bi_vilD_dir,vfname,
               latlon_scale,gv.gridlon[0][pp.ny-1],
               gv.gridlat[0][pp.ny-1],latlon_scale,
               dlatdlon_scale,meter_scale,varname,
               varunit,nradar_has_data,radarnam,
               i2tem2d[1],vilD_scale, vilD_miss, pp,       
               gv,pp.bb_mode,1);        

      //transfer vilD data to nmqserver by using LDM
      if(pp.ldm_binary_opt == 1 )
      {
        char ldm_command[200];
        char fileName[200];
        sprintf(fileName,"%s/%s.gz", pp.bi_vilD_dir, vfname);
        sprintf(ldm_command, "%s -v -l %s -q %s -f WMO %s", pp.pqinsert_file,
                 pp.ldmd_log_file,pp.ldm_pq_file,fileName);
        if(pp.debug_flag == 1)
          cout<<"Implement "<<ldm_command<<endl;
        system(ldm_command);
      }   

  }

  //release all claimed spaces
  for(j=0; j<pp.ny; j++)
  {
     delete [] vil[j];
     delete [] vilD[j];
  }
  delete [] vil;
  delete [] vilD;


 for(j=0; j<pp.ny; j++)
  {
     delete [] flag[j];
     delete [] iterain[j];
  }
  delete [] flag;
  delete [] iterain;






  return;
}

