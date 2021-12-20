
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
#include <cstring>
 
#include "CONFIG_PARS.h"
#include "mosaic_adapt_pars.h"
#include "func_prototype.h"

using namespace std;

void produce_HDA(CONFIG_PARS &pp, float*** ref, float ***kh_tlvls, int itime0, 
                 int nradar_has_data, grid_vars &gv, char* timeString,            
                 string config_file_name, char radarnam[][5],short **i2tem2d)
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
  float **shi, **posh, **mehs; // ***i_t;
  int kk; float frac;
  float i_273k, i_253k;
  float temp_shi, temp_posh, temp_mehs;
  float E,H273K,H253K;
  int i,j,k,index = 0;

  shi  = new float * [pp.ny];
  posh = new float * [pp.ny];
  mehs = new float * [pp.ny];
  for(j=0; j<pp.ny; j++)
  {
     shi[j]  = new float [pp.nx];
     posh[j] = new float [pp.nx];
     mehs[j] = new float [pp.nx];
  }
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

  //transfer 2D array to 1D array
  for(j=0; j<pp.ny; j++)
  for(i=0; i<pp.nx; i++)
  {
     index = j*pp.nx + i;
     if( shi[j][i] < shi_miss + 0.01)
        i2tem2d[0][index] = get_int(shi_miss*shi_scale);
     else
        i2tem2d[0][index] = get_int(shi[j][i]*shi_scale);

     if( posh[j][i] < posh_miss + 0.01)
        i2tem2d[1][index] = get_int(posh_miss*posh_scale);
     else
         i2tem2d[1][index] = get_int(posh[j][i]*posh_scale);

     if( mehs[j][i] < mehs_miss + 0.01)
        i2tem2d[2][index] = get_int(mehs_miss*mehs_scale);
     else
        i2tem2d[2][index] = get_int(mehs[j][i]*mehs_scale);
  }


  //output SHI product in binary and netcdf format
  char vfname[200];
  char varname[20];
  char varunit[7];

  sprintf(vfname,"%s%s","SHI.",timeString);
  strcpy(varname,"SHI");
  strcpy(varunit,"       ");

  if(pp.bi_shi_flag)
  {
     if(pp.debug_flag)
          cout<<"Writing binary file:"<<vfname<<endl;

     i2wrtvar_cart3d(pp.bi_shi_dir,vfname,
               latlon_scale,gv.gridlon[0][pp.ny-1],
               gv.gridlat[0][pp.ny-1],latlon_scale,
               dlatdlon_scale,meter_scale,varname,
               varunit,nradar_has_data,radarnam,
               i2tem2d[0],shi_scale, shi_miss, pp,
               gv,pp.bb_mode,1);       

      //transfer shi data to nmqserver by using LDM
      if(pp.ldm_binary_opt == 1 )
      {
        char ldm_command[200];
        char fileName[200];
        sprintf(fileName,"%s/%s.gz", pp.bi_shi_dir, vfname);
        sprintf(ldm_command, "%s -v -l %s -q %s -f WMO %s", pp.pqinsert_file,
                 pp.ldmd_log_file,pp.ldm_pq_file,fileName);
        if(pp.debug_flag == 1)
          cout<<"Implement "<<ldm_command<<endl;
        system(ldm_command);
      }   

  }

  //output POSH product in binary and netcdf format
  sprintf(vfname,"%s%s","POSH.",timeString);
  strcpy(varname,"POSH");
  strcpy(varunit,"       ");

  if(pp.bi_posh_flag)
  {
     if(pp.debug_flag)
          cout<<"Writing binary file:"<<vfname<<endl;

     i2wrtvar_cart3d(pp.bi_posh_dir,vfname,
               latlon_scale,gv.gridlon[0][pp.ny-1],
               gv.gridlat[0][pp.ny-1],latlon_scale,
               dlatdlon_scale,meter_scale,varname,
               varunit,nradar_has_data,radarnam,
               i2tem2d[1],posh_scale, posh_miss, pp,
               gv,pp.bb_mode,1);    

      //transfer posh data to nmqserver by using LDM
      if(pp.ldm_binary_opt == 1 )
      {
        char ldm_command[200];
        char fileName[200];
        sprintf(fileName,"%s/%s.gz", pp.bi_posh_dir, vfname);
        sprintf(ldm_command, "%s -v -l %s -q %s -f WMO %s", pp.pqinsert_file,
                 pp.ldmd_log_file,pp.ldm_pq_file,fileName);
        if(pp.debug_flag == 1)
          cout<<"Implement "<<ldm_command<<endl;
        system(ldm_command);
      }   

  }

  //output MEHS product in binary and netcdf format
  sprintf(vfname,"%s%s","MEHS.",timeString);
  strcpy(varname,"MEHS");
  strcpy(varunit,"mm");

  if(pp.bi_mehs_flag)
  {
     if(pp.debug_flag)
          cout<<"Writing binary file:"<<vfname<<endl;

     i2wrtvar_cart3d(pp.bi_mehs_dir,vfname,
               latlon_scale,gv.gridlon[0][pp.ny-1],
               gv.gridlat[0][pp.ny-1],latlon_scale,
               dlatdlon_scale,meter_scale,varname,
               varunit,nradar_has_data,radarnam,
               i2tem2d[2],mehs_scale, mehs_miss, pp,
               gv, pp.bb_mode,1);         
 
      //transfer mehs data to nmqserver by using LDM
      if(pp.ldm_binary_opt == 1 )
      {
        char ldm_command[200];
        char fileName[200];
        sprintf(fileName,"%s/%s.gz", pp.bi_mehs_dir, vfname);
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
      delete [] shi[j];
      delete [] posh[j];
      delete [] mehs[j];
  }
  delete [] shi;
  delete [] posh;
  delete [] mehs;
/*
  for(i = 0; i< pp.nx; i++)
  {
     for(j = 0; j < pp.ny; j++)
         delete [] i_t[i][j];
     delete [] i_t[i];
  }
  delete [] i_t;
*/
  return;
}

