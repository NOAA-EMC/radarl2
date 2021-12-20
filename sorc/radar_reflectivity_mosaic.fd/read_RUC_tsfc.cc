// ##########################################################################
//
//  read_RUC_tsfc.cc: Read in the RUC surface temperature binary file
//    as written by model_remap.  
//
// ##########################################################################
//
//  Author: Carrie Langsotn(CIMMS/NSSL)
//  May 2006
//
//  Inputs : Path to tsfc file, current epoch time, and max age 
//           of data allowed
//  Outputs: 2D array of floats storing RUC sfc temperature
//
//  Modification history:
//  
//######################################################################

#include <ctime>
#include <string>
#include <cstring>
#include <fstream>
#include <cstdio>

#include "mosaic_adapt_pars.h"
#include "func_prototype.h"
#include "CONFIG_PARS.h"

using namespace std;

float ** read_RUC_tsfc(CONFIG_PARS &pp, char data_path[], 
                        long current_epoch, int max_age, int debug_flag)
{
    float **data = 0;

    char varname[20];
    char varunit[7];
    int i, j, k, file_size = -999;
    bool l_model = 0, l_snd = 0;
    int nf_model =0;
    
    int istatus = -1;
    char latest_f1[200];
    sprintf(latest_f1, "%s/.latest", data_path);
   
    if(pp.debug_flag) cout<<"Read in file:"<<latest_f1<<endl; 
    ifstream lateF1(latest_f1);
    char temp_string[30];
    char time_str_model[20];
    
    if(!lateF1) 
    {
      cout<<"++WARNING: can not open latest Tsfc model file"<<latest_f1<<endl;
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

      if(lateF1 >> nf_model) //if first read is successful
      {

        if(nf_model > 0 && nf_model < max_model_prods)
        {
          l_model = 0;
          int itime_ruc;

          for (int ii=0; ii<nf_model; ii++)
          {
            lateF1 >> temp_string;
    
            if(strncmp(temp_string, "TSFC_MODEL", 10) == 0)
            {
              strncpy(time_str_model,&temp_string[11],15);
              time_str_model[15] = '\0';

              l_model = chktime(time_str_model, (int)current_epoch,
                             itime_ruc, max_age, debug_flag);
              continue;  // exit the ii-loop once tsfc file found.
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
      char tsfc_file[200];
      sprintf(tsfc_file, "%s/%s",data_path,temp_string);
      ifstream inF3(tsfc_file);
      if(!inF3)
      {
        cout<<"++WARNING failed reading file: "<<tsfc_file<<endl;
        l_model = 0;
        inF3.close();
      }
      else
      {

        float nw_x,nw_y;
        int map_scale, xy_scale, dxy_scale, z_scale;
        int idummy, imissing;
    //    char radnm[pp.nradars][5];
        char radnm[160][5]; 
        int remap_conus_opt = 0; //keep in native coordinates

        i2tem1=new short int[pp.conus_nx*pp.conus_ny];

        file_size = i2readvar_cart3d(tsfc_file, remap_conus_opt,
                     map_scale,
                     nw_x,nw_y,
                     xy_scale, dxy_scale, z_scale,
                     varname,varunit,idummy,radnm,
                     i2tem1, var_scale1, imissing, 
                     pp, istatus, idummy);

        //Check 3 values to see if file was valid
        if(file_size == 0)
        {
          cout<<"++ERROR++ File "<<tsfc_file<<" is empty!"<<endl;
          //cout<<"Aborting from get_index_temp function."<<endl; 
          l_model = 0;
          
          //return kh_tlvls;
          //exit(0);
        }
        
        if(istatus!=0)
        {
          cout<<"++ERROR++ failed reading: "<<tsfc_file<<endl;
          //cout<<"Aborting from get_index_temp function"<<endl;
          l_model = 0;
          //return kh_tlvls;
          //exit(0);
        }
      
        if(var_scale1 ==0)
        {
          cout<<"++ERROR++ bad scaling factor in: "<<tsfc_file<<endl;
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
        //Initialize data array
        data = new float *[pp.conus_nx];
        for(i = 0; i < pp.conus_nx; i++) data[i] = new float [pp.conus_ny];
   
        for(i=0; i<pp.conus_nx; i++)
        {
          for(j=0; j<pp.conus_ny; j++)
          {
            int index = j*pp.conus_nx + i;
            if(i2tem1[index]/var_scale1 > ref_miss)
            {
              data[i][j] = (float)i2tem1[index]/(float)var_scale1;
            }
            
          }  // end of j-loop
        }  // end of i-loop
        
        delete [] i2tem1;

        l_model = 1;

      }   // end if inF3 successfully opened

    }  // end if l_model:model data file exist and is valid

    
    return data;

}
