
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
#include <cstring>
 
#include "CONFIG_PARS.h"
#include "mosaic_adapt_pars.h"
#include "func_prototype.h"

#define LCREF_THRESHOLD 0.0

using namespace std;

void produce_LCREF_and_LCREFH(CONFIG_PARS &pp, float*** ref, int itime0, int nradar_has_data,
                  grid_vars &gv, char* timeString,char radarnam[][5],short **i2tem2d)
{
  //declare and initialize some variables
  float temp_height,temp_ref; 
  float **LCREF, **LCREFH;
  int index = 0;
  int i,j,k;
  int highest_level = 0;
  int tmp_terrain;

  LCREF = new float * [pp.ny];
  LCREFH = new float * [pp.ny];
  for(j=0; j< pp.ny; j++)
  {
      LCREF[j] = new float [pp.nx];
      LCREFH[j] = new float [pp.nx];
  }

  
  //loop through all grid points 
  for (j=0; j < pp.ny; j++)
  for (i=0; i < pp.nx; i++)
  {
    //get highest level value for this grid point
    if(gv.hterain[j][i] == -999) tmp_terrain = 0;
    else tmp_terrain = gv.hterain[j][i];
    
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

  for(j=0; j<pp.ny; j++)
  for(i=0; i<pp.nx; i++)
  {
     index = j*pp.nx + i;

     if( LCREF[j][i] < (float)ref_miss + 0.01)
        i2tem2d[0][index] = get_int(ref_miss*ref_scale);
     else
       i2tem2d[0][index] = get_int(LCREF[j][i]*(float)ref_scale);

     if( LCREFH[j][i] < (float)crefh_miss + 0.01)
       i2tem2d[1][index] = get_int(crefh_miss*crefh_scale);
     else
       i2tem2d[1][index] = get_int(LCREFH[j][i]*crefh_scale);
  }

  //output LCREF product in binary format
  char vfname[200];
  char varname[20];
  char varunit[7];

  sprintf(vfname,"%s%s","LCREF.",timeString);
  strcpy(varname,"LCREF");
  strcpy(varunit,"dBZ");

  if(pp.bi_lcref_flag)
  {
     if(pp.debug_flag)
          cout<<"Writing binary file:"<<vfname<<endl;

     i2wrtvar_cart3d(pp.bi_lcref_dir,vfname,
               latlon_scale,gv.gridlon[0][pp.ny-1],
               gv.gridlat[0][pp.ny-1],latlon_scale,
               dlatdlon_scale,meter_scale,varname,
               varunit,nradar_has_data,radarnam,
               i2tem2d[0],ref_scale, ref_miss, pp,
               gv,pp.bb_mode,1);         

      //transfer lcref data to nmqserver by using LDM
      if(pp.ldm_binary_opt == 1 )
      {
        char ldm_command[200];
        char fileName[200];
        sprintf(fileName,"%s/%s.gz", pp.bi_lcref_dir, vfname);
        sprintf(ldm_command, "%s -v -l %s -q %s -f WMO %s", pp.pqinsert_file,
                 pp.ldmd_log_file,pp.ldm_pq_file,fileName);
        if(pp.debug_flag == 1)
          cout<<"Implement "<<ldm_command<<endl;
        system(ldm_command);
      }   

  }

  //output LCREFH product in binary format
  sprintf(vfname,"%s%s","LCREFH.",timeString);
  strcpy(varname,"LCREFH");
  strcpy(varunit,"kmMSL");

  if(pp.bi_lcrefh_flag)
  {
     if(pp.debug_flag)
          cout<<"Writing binary file:"<<vfname<<endl;

     i2wrtvar_cart3d(pp.bi_lcrefh_dir,vfname,
               latlon_scale,gv.gridlon[0][pp.ny-1],
               gv.gridlat[0][pp.ny-1],latlon_scale,
               dlatdlon_scale,meter_scale,varname,
               varunit,nradar_has_data,radarnam,
               i2tem2d[1],crefh_scale, crefh_miss, pp,
               gv,pp.bb_mode,1);        

      //transfer lcrefh data to nmqserver by using LDM
      if(pp.ldm_binary_opt == 1 )
      {
        char ldm_command[200];
        char fileName[200];
        sprintf(fileName,"%s/%s.gz", pp.bi_lcrefh_dir, vfname);
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
      delete [] LCREF[j];
      delete [] LCREFH[j];
  }
  delete [] LCREF;
  delete [] LCREFH;

  return;
}

