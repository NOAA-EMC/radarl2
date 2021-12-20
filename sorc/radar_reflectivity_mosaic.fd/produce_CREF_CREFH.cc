
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
#include <cstring>
 
#include "CONFIG_PARS.h"
#include "mosaic_adapt_pars.h"
#include "func_prototype.h"
#include "nids_output.h"

#define CREF_THRESHOLD 0.0

using namespace std;

void produce_CREF_CREFH (CONFIG_PARS &pp, nids_pars &np, string config_file_name, 
                         float*** ref,  short **tem, char* timeString, 
                         grid_vars &gv, int nradar_has_data, char radarnam[][5])
{

//######################################################################
//
// Declare and initialize composite refl and the height variables
//
//######################################################################

  float temp_height,temp_ref; 

  float **CREF, **CREFH;
  int index = 0;
  int i,j,k;

  CREF = new float * [pp.ny];
  CREFH = new float * [pp.ny];
  for(j=0; j< pp.ny; j++)
  {
      CREF[j] = new float [pp.nx];
      CREFH[j] = new float [pp.nx];
  }

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

//######################################################################
//
// Output Composite reflectivity field in NIDS output 
//
//######################################################################

  float ** cref_nids;

  cref_nids = new float * [pp.nx];
  for (i=0; i< pp.nx; i++) cref_nids[i] = new float [pp.ny];

  for (j=0; j < pp.ny; j++)
  for (i=0; i < pp.nx; i++)
    cref_nids[i][j] = CREF[j][i];

  char nidscompreffname[200];
  sprintf(nidscompreffname,"%s/%s%s.mos", pp.nids_cref_dir,"CREF.",timeString);

  short QPEcode=201;
  float r_max_scale = 1.0;

  if( pp.nids_cref_flag )
  {
      if(pp.debug_flag) cout<<"Writing NIDS file:"<<nidscompreffname<<endl;
    
      nids_output(config_file_name, nidscompreffname, pp,
                cref_nids, np.dlon_n, np.dlat_n, np.nx_n, np.ny_n, QPEcode,
                REF_VALUES_TAG, REF_COLORS_TAG,
                (float)ref_miss, r_max_scale);

      //transfer mosaic data to nmqserver by using LDM
      if(pp.ldm_nids_opt == 1 )
      {
          char ldm_command[200];
          sprintf (ldm_command, "%s -v -l %s -q %s -f EXP %s", pp.pqinsert_file,
                pp.ldmd_log_file,pp.ldm_pq_file,nidscompreffname);
          if(pp.debug_flag == 1)
             cout<<"Implement "<<ldm_command<<endl;
          system(ldm_command);
      }
  }

//######################################################################
//
// Output Height of Composite reflectivity field in NIDS output 
//
//######################################################################

  char nidschgtfname[200];
  sprintf(nidschgtfname,"%s/%s%s.mos", pp.nids_chgt_dir,"CHGT.",timeString);

  QPEcode=202;
  r_max_scale = 0.01;

  for (j=0; j < pp.ny; j++)
  for (i=0; i < pp.nx; i++)
  {
      if (CREFH[j][i] > (float) meter_miss + 0.01)
          cref_nids[i][j] = CREFH[j][i];
      else cref_nids[i][j] = (float) meter_miss;
  }

  if( pp.nids_chgt_flag )
  {
      if(pp.debug_flag) cout<<"Writing NIDS file:"<<nidschgtfname<<endl;
    
      nids_output (config_file_name, nidschgtfname, pp,
                cref_nids, np.dlon_n, np.dlat_n, np.nx_n, np.ny_n, QPEcode,
                HEIGHT_VALUES_TAG, HEIGHT_COLORS_TAG,
                (float) meter_miss, r_max_scale);

      //transfer mosaic data to nmqserver by using LDM
      if(pp.ldm_nids_opt == 1 )
      {
          char ldm_command[200];
          sprintf (ldm_command, "%s -v -l %s -q %s -f EXP %s", pp.pqinsert_file,
                pp.ldmd_log_file,pp.ldm_pq_file,nidschgtfname);
          if(pp.debug_flag == 1)
             cout<<"Implement "<<ldm_command<<endl;
          system(ldm_command);
      }
  }

  for(i=0; i<pp.nx; i++) delete [] cref_nids[i];
  delete [] cref_nids;

//######################################################################
//
// Prepare for NetCDF output
//
//######################################################################

  for(j=0; j<pp.ny; j++)
  for(i=0; i<pp.nx; i++)
  {
     index = j*pp.nx + i;

     if( CREF[j][i] < (float) ref_miss + 0.01) tem[0][index] = get_int(ref_miss*ref_scale);
     else tem[0][index] = get_int(CREF[j][i]*ref_scale);

     if( CREFH[j][i] < (float) crefh_miss + 0.01) tem[1][index] = get_int(crefh_scale*crefh_miss);
     else tem[1][index] = get_int(CREFH[j][i]*crefh_scale);  
  }


  //output LCREF product in binary format
  char vfname[200];
  char varname[20];
  char varunit[7];

  sprintf(vfname,"%s%s","CREF.",timeString);
  strcpy(varname,"CREF");
  strcpy(varunit,"dBZ");

  if(pp.bi_cref_flag)
  {
     if(pp.debug_flag)
          cout<<"Writing binary file:"<<vfname<<endl;

     i2wrtvar_cart3d(pp.bi_cref_dir,vfname,
               latlon_scale,gv.gridlon[0][pp.ny-1],
               gv.gridlat[0][pp.ny-1],latlon_scale,
               dlatdlon_scale,meter_scale,varname,
               varunit,nradar_has_data,radarnam,
               tem[0],ref_scale, ref_miss, pp,
               gv,pp.bb_mode,1);         

      //transfer lcref data to nmqserver by using LDM
      if(pp.ldm_binary_opt == 1 )
      {
        char ldm_command[200];
        char fileName[200];
        sprintf(fileName,"%s/%s.gz", pp.bi_cref_dir, vfname);
        sprintf(ldm_command, "%s -v -l %s -q %s -f WMO %s", pp.pqinsert_file,
                 pp.ldmd_log_file,pp.ldm_pq_file,fileName);
        if(pp.debug_flag == 1)
          cout<<"Implement "<<ldm_command<<endl;
        system(ldm_command);
      }   

  }

  //output CREFH product in binary format
  sprintf(vfname,"%s%s","CREFH.",timeString);
  strcpy(varname,"CREFH");
  strcpy(varunit,"kmMSL");

  if(pp.bi_chgt_flag)
  {
     if(pp.debug_flag)
          cout<<"Writing binary file:"<<vfname<<endl;

     i2wrtvar_cart3d(pp.bi_chgt_dir,vfname,
               latlon_scale,gv.gridlon[0][pp.ny-1],
               gv.gridlat[0][pp.ny-1],latlon_scale,
               dlatdlon_scale,meter_scale,varname,
               varunit,nradar_has_data,radarnam,
               tem[1],crefh_scale, crefh_miss, pp,
               gv,pp.bb_mode,1);        

      //transfer lcrefh data to nmqserver by using LDM
      if(pp.ldm_binary_opt == 1 )
      {
        char ldm_command[200];
        char fileName[200];
        sprintf(fileName,"%s/%s.gz", pp.bi_chgt_dir, vfname);
        sprintf(ldm_command, "%s -v -l %s -q %s -f WMO %s", pp.pqinsert_file,
                 pp.ldmd_log_file,pp.ldm_pq_file,fileName);
        if(pp.debug_flag == 1)
          cout<<"Implement "<<ldm_command<<endl;
        system(ldm_command);
      }   

  }



  for(j=0; j<pp.ny; j++)
  {
      delete [] CREF[j];
      delete [] CREFH[j];
  }
  delete [] CREF;
  delete [] CREFH;

  return;
}
