// ##########################################################################
//
//  get_index_temp.cc: To get 3D index vs temperature level array. 
//  This program is based on t_level_deriv.cc from Jian and Wenwu.
//
// ##########################################################################
//
//  Author: SHUNXIN WANG(CIMMS/NSSL)
//  DEC. 20, 2003
//
//  Inputs : 3D reflectivity array, CONFIG_PARS object, radar object, grid_vars 
//          object, instant time and time string
//  Outputs: 3D index vs temperature array: kh_tlvls
//
//  Modification history:
//  
//######################################################################

#include <ctime>
#include <string>
#include <fstream>
#include <cstdio>
#include <cstring>

#include "mosaic_adapt_pars.h"
#include "CONFIG_PARS.h"
#include "func_prototype.h"
#include "grid_vars.h"
#include "nids_output.h"

using namespace std;

float *** get_index_temp(int itime0, CONFIG_PARS &pp, char radarnam[][5], 
                         grid_vars &gv, string config_file_name, 
                         char timeString[20])
{
    float ***kh_tlvls = 0;

    char varname[20];
    char varunit[7];
    int i, j, k, file_size = -999;
    bool l_model = 0, l_snd = 0;
    int nf_model =0;
    
    int istatus = -1;
    char latest_f1[200];
    sprintf(latest_f1,"%s/.latest",pp.model_dat_dir);
   
    if(pp.debug_flag) cout<<"Read in file:"<<latest_f1<<endl; 
    ifstream lateF1(latest_f1);
    char temp_string[30];
    char time_str_model[20];
    
    if(!lateF1) {
      cout<<"++WARNING: can not open latest model file"<<latest_f1<<endl;
      lateF1.close();
      l_model = 0;
    }
    else
    {

//#######################################################################
//
//  Latest model data available.  Check the time of the latest
//  model data.
//
//#######################################################################

      if(lateF1 >> nf_model)
      {

        if(nf_model > 0 && nf_model < max_model_prods)
        {
          l_model = 0;
          int itime1;

          for (int ii=0; ii<nf_model; ii++)
          {
            lateF1 >> temp_string;
    
            if(strncmp(temp_string, "TLVL_INX", 8) == 0)
            {
              strncpy(time_str_model,&temp_string[9],13);
              time_str_model[13] = '\0';
      
              l_model = chktime(time_str_model, itime0,
                             itime1, model_dat_window, pp.debug_flag);
              continue;  // exit the ii-loop once tlvl_inx file found.
            }
          }  // end ii-loop
    
        }  // end if nf_model>0:  at least one model field available
    
        else
        {
          cout<<"Bad number of model products = "<<nf_model<<endl;
          cout<<"Will try rawinsonde for temperature profiles."<<endl;
          l_model = 0;
        } 
      }  // end if successfully reading "nf_model"

      else
      {
        cout<<"++WARNING failed reading file: "<<latest_f1<<endl;
        cout<<"Will try rawinsonde for temperature profiles."<<endl;
        l_model = 0;
      } 
        
      lateF1.close();
          
    }  // successfully open latest_f1
          
//#######################################################################
//
//  Latest model data available.  Reading the height index (with respect
//  to the mosaic grid) of the pre-specified temperature levels.
//  The index array is derived from the model data.
//
//#######################################################################
    short int *i2tem1 = 0;
    int var_scale1;
    
    if(l_model)
    {
      char tlvl_index_file[200];
      sprintf(tlvl_index_file, "%s/%s",pp.model_dat_dir,temp_string);
      ifstream inF3(tlvl_index_file);
      if(!inF3)
      {
        cout<<"++WARNING failed reading file: "<<tlvl_index_file<<endl;
        cout<<"Will try rawinsonde for temperature profiles."<<endl;
        l_model = 0;
        inF3.close();
      }
      else
      {

        float nw_x,nw_y;
        int map_scale, xy_scale, dxy_scale, z_scale;
        int idummy, imissing;
       // char radnm[pp.nradars][5];
        char radnm[160][5];
        int remap_conus_opt = 1;

        i2tem1=new short int[pp.nx*pp.ny*n_tlevels];

        file_size = i2readvar_cart3d(tlvl_index_file, 
                     remap_conus_opt, map_scale,
                     nw_x,nw_y,
                     xy_scale, dxy_scale, z_scale,
                     varname,varunit,idummy,radnm,
                     i2tem1, var_scale1, imissing, 
                     pp, istatus, idummy);

        //Check 3 values to see if file was valid
        if(file_size == 0)
        {
          cout<<"++ERROR++ File "<<tlvl_index_file<<" is empty!"<<endl;
          //cout<<"Aborting from get_index_temp function."<<endl; 
          l_model = 0;
          
          //return kh_tlvls;
          //exit(0);
        }
        
        if(istatus!=0)
        {
          cout<<"++ERROR++ failed reading: "<<tlvl_index_file<<endl;
          //cout<<"Aborting from get_index_temp function"<<endl;
          l_model = 0;
          //return kh_tlvls;
          //exit(0);
        }
      
        if(var_scale1 ==0)
        {
          cout<<"++ERROR++ bad scaling factor in: "<<tlvl_index_file<<endl;
          //cout<<"Aborting from get_index_temp function"<<endl; 
          l_model = 0;
          //return kh_tlvls;
          //exit(0);
        }
        
        
        //Print message and clean up
        if(!l_model)
        {
          cout<<"Will try to find model sounding."<<endl;
          if(i2tem1 != 0)
          {
            delete [] i2tem1;
            i2tem1 = 0;
          }
        }
        
      }//end if l_model:model data file exist.
      
      if(l_model && (i2tem1 != 0) )
      {
        float kh_max=-99.0, kh_min= 99.0;
        
        //Initialize kh_tlvls
        kh_tlvls = new float **[pp.nx];
        for(i = 0; i < pp.nx; i++)
        {
          kh_tlvls[i] = new float *[pp.ny];
          for(j = 0; j < pp.ny; j++)
            kh_tlvls[i][j] = new float[n_tlevels];
        }
   
        for(int kt=0; kt<n_tlevels; kt++)
        {
          for(i=0; i<pp.nx; i++)
          {
            for(j=0; j<pp.ny; j++)
            {
              int index = kt*pp.ny*pp.nx + j*pp.nx + i;
              if(i2tem1[index]/var_scale1 > ref_miss)
              {
                kh_tlvls[i][j][kt] = (float)i2tem1[index]/(float)var_scale1;
                if(kh_tlvls[i][j][kt]>kh_max) kh_max = kh_tlvls[i][j][kt];
                if(kh_tlvls[i][j][kt]<kh_min) kh_min = kh_tlvls[i][j][kt];
              }
            }  // end of j-loop
          }  // end of i-loop
        }  // end of kt-loop
        delete [] i2tem1;

        l_model = 1;

      }   // end if inF3 successfully opened

    }  // end if l_model:model data file exist and is valid

    if(!l_model)
    {
    
//#######################################################################
//
//  Using sounding temperature levels.
//
//#######################################################################

      int t_z_flag = -1;
      
      float zt_snd[n_tlevels] = { 0, 1500, 3000, 4500, 6000, 7500, 9000, 10500, 12000 };

      char sounding_file[200];

      sprintf(sounding_file,"%s/%s.env_sounding.latest",pp.sounding_dir,radarnam[0]);

      ifstream in(sounding_file);

      if(in)
      {
        string radar_name_snd(radarnam[0]);

        t_z_flag = get_t_z_snd (radar_name_snd,
                                  config_file_name,
                                  n_tlevels,
                                  zt_snd,
                                  pp.debug_flag);
      }
      else
      {
        cout<<"++WARNING:  No sounding available."<<endl;
        cout<<"Using pre-specified T-level heights."<<endl;
        cout<<endl<<"T: ";
        for(k=0; k<n_tlevels; k++) cout<<t_snd[k]<<",  ";
        cout<<endl<<"z: ";
        for(k=0; k<n_tlevels; k++) cout<<zt_snd[k]<<",  ";
        cout<<endl;
        t_z_flag = 1;
      }
    
//#######################################################################
//
//  Find the location of pre-specified temperature levels 
//  in the mosaic analysis grid.
//  
//#######################################################################

      if(t_z_flag==1)
      {
        //Initialize kh_tlvls
        kh_tlvls = new float **[pp.nx];
        for(i = 0; i < pp.nx; i++)
        {
          kh_tlvls[i] = new float *[pp.ny];
          for(j = 0; j < pp.ny; j++)
            kh_tlvls[i][j] = new float[n_tlevels];
        }
        
        for(int kt=0; kt<n_tlevels; kt++)
        {
          float arg = -1.0;

          if(zt_snd[kt]<=t_z_miss+0.1) continue;

          for(k=0; k<pp.nz-2; k++)
          {
            if(gv.zp[k]<zt_snd[kt] && gv.zp[k+1]>=zt_snd[kt])
            {
              arg = (float)k + (zt_snd[kt]-gv.zp[k]) / (gv.zp[k+1]-gv.zp[k]);
              break;
            }
          }  // end of k-loop

          if(gv.zp[0] >= zt_snd[kt]) arg = 0.0;
          if(gv.zp[pp.nz-1] < zt_snd[kt]) arg = -1.0;

          for(i=0; i<pp.nx; i++)
          for(j=0; j<pp.ny; j++)
            kh_tlvls[i][j][kt] = arg;

        }  // end of kt-loop

        if (pp.debug_flag)
          cout<<"Using single sounding to find heights for"
              <<" pre-specified temperatures."<<endl;
        l_snd = 1;

      } // end if t_z_flag ==1

      else // t_z_flag !=1  no sounding available.
      {
        cout<<"++ERROR:  No sounding available."<<endl;
        cout<<"No Temp level refl products generated."<<endl;
        l_snd = 0;
      }

    }  // end if (!l_model) : no model data available
    
    return kh_tlvls;

}
