
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
#include <cstring>
 
#include "CONFIG_PARS.h"
#include "mosaic_adapt_pars.h"
#include "func_prototype.h"

using namespace std;

void produce_HSR_and_HSRH(CONFIG_PARS &pp, float*** ref, int itime0, int nradar_has_data,
                  grid_vars &gv, char* timeString,char radarnam[][5],short **i2tem2d)
{
  //declare and initialize some variables
  float temp_height,temp_ref; 
  float **HSR, **HSRH;
  int index = 0;
  int i,j,k;

  HSR = new float * [pp.ny];
  HSRH = new float * [pp.ny];
  for(j=0; j< pp.ny; j++)
  {
      HSR[j] = new float [pp.nx];
      HSRH[j] = new float [pp.nx];
  }

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
          
            if(temp_height<gv.hterain[j][i]) 
               continue;
            else break;
          }
      }
 
      HSR[j][i]   = temp_ref; 
      HSRH[j][i]   = temp_height - gv.hterain[j][i]; 
      
      if(temp_height != hsrh_miss) HSRH[j][i] /= KM_TO_M;//kmAGL
  }

  for(j=0; j<pp.ny; j++)
  for(i=0; i<pp.nx; i++)
  {
     index = j*pp.nx + i;

     if( HSR[j][i] < (float)hsr_miss + 0.01)
        i2tem2d[0][index] = get_int(hsr_miss*hsr_scale);
     else
       i2tem2d[0][index] = get_int(HSR[j][i]*(float)hsr_scale);

     if( HSRH[j][i] < (float)hsrh_miss + 0.01)
       i2tem2d[1][index] = get_int(hsrh_miss*hsrh_scale);
     else
       i2tem2d[1][index] = get_int(HSRH[j][i]*hsrh_scale);
  }

  //output HSR product in binary and netcdf format
  char vfname[200];
  char varname[20];
  char varunit[7];

  sprintf(vfname,"%s%s","HSR.",timeString);
  strcpy(varname,"HSR");
  strcpy(varunit,"dBZ");

  if(pp.bi_hsr_flag)
  {
     if(pp.debug_flag)
          cout<<"Writing binary file:"<<vfname<<endl;

     i2wrtvar_cart3d(pp.bi_hsr_dir,vfname,
               latlon_scale,gv.gridlon[0][pp.ny-1],
               gv.gridlat[0][pp.ny-1],latlon_scale,
               dlatdlon_scale,meter_scale,varname,
               varunit,nradar_has_data,radarnam,
               i2tem2d[0],hsr_scale, hsr_miss, pp,
               gv,pp.bb_mode,1);         

      //transfer hsr data to nmqserver by using LDM
      if(pp.ldm_binary_opt == 1 )
      {
        char ldm_command[200];
        char fileName[200];
        sprintf(fileName,"%s/%s.gz", pp.bi_hsr_dir, vfname);
        sprintf(ldm_command, "%s -v -l %s -q %s -f WMO %s", pp.pqinsert_file,
                 pp.ldmd_log_file,pp.ldm_pq_file,fileName);
        if(pp.debug_flag == 1)
          cout<<"Implement "<<ldm_command<<endl;
        system(ldm_command);
      }   

  }

  //output HSRH product in binary and netcdf format
  sprintf(vfname,"%s%s","HSRH.",timeString);
  strcpy(varname,"HSRH");
  strcpy(varunit,"kmAGL");

  if(pp.bi_hsrh_flag)
  {
     if(pp.debug_flag)
          cout<<"Writing binary file:"<<vfname<<endl;

     i2wrtvar_cart3d(pp.bi_hsrh_dir,vfname,
               latlon_scale,gv.gridlon[0][pp.ny-1],
               gv.gridlat[0][pp.ny-1],latlon_scale,
               dlatdlon_scale,meter_scale,varname,
               varunit,nradar_has_data,radarnam,
               i2tem2d[1],hsrh_scale, hsrh_miss, pp,
               gv,pp.bb_mode,1);        

      //transfer hsrh data to nmqserver by using LDM
      if(pp.ldm_binary_opt == 1 )
      {
        char ldm_command[200];
        char fileName[200];
        sprintf(fileName,"%s/%s.gz", pp.bi_hsrh_dir, vfname);
        sprintf(ldm_command, "%s -v -l %s -q %s -f WMO %s", pp.pqinsert_file,
                 pp.ldmd_log_file,pp.ldm_pq_file,fileName);
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

/*
  char nidschgtfname[200];
  sprintf(nidschgtfname,"%s/%s%s.mos", pp.nids_chgt_dir,"CHGT.",timeString);

  if(pp.debug_flag) cout<<"Writing NIDS file:"<<nidschgtfname<<endl;
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
*/

/////////////////////////////////////////////////////

  //release all claimed spaces
  for(j=0; j<pp.ny; j++)
  {
      delete [] HSR[j];
      delete [] HSRH[j];
  }
  delete [] HSR;
  delete [] HSRH;

  return;
}

