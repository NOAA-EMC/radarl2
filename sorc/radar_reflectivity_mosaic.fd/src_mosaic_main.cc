//     ##################################################################
//     ######              PROGRAM SRC MOSAIC                      ######
//     ##################################################################
//#######################################################################
//  PURPOSE:
//    Remap 3D full resolution  reflectivity fields from multiple
//    radars onto a 3D Cartesian grid,  and derive composite reflectivity mosaic.
// ##########################################################################
//   Author: Shunxin Wang (CIMMS/NSSL)
//   June 15, 2004
//   Modification History:
//   
//   3/4/2006  Carrie Langstion (CIMMS/NSSL)
//    Changed the ordering of sum_wref from [k][j][i] to [k][i][j]
//    Added code to delete sum_wref array at end of program
//
//   1/9/2013  Shun Liu (EMC/NCEP)
//     add cstring for WCOSS
// ##########################################################################

#include <iostream>
#include <fstream>
#include <string>
#include <cstring>
#include <map>
#include <cmath>
#include <ctime>
#include <cstdlib>
#include <vector>
#include <cstdio>

#include "CONFIG_PARS.h"
#include "func_prototype.h"
//#include "NETCDF_HANDLER.h"
#include "nids_output.h"
#include "mosaic_adapt_pars.h"
#include "maplib3d.h"
#include "grid_vars.h"
#include "radar_sum.h"
#include "phycst.h"
#include "setgrd.h"
#include "SRC_DATA_HEADER.h"

//includes for WDSS-II file I/O (2/17/2006 CL)
//#include "ioW2.h"
//#include "W2DataField.h"
//#include "LatLonHeightField.h"

using namespace std;

int main(int argc,  char* argv[])
{

//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
//  Beginging of the mosaic main program
//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

    cout<<"\n\n"<<endl;
    cout<<"      *************************************************"<<endl;
    cout<<"      *                                               *"<<endl;
    cout<<"      *         WELCOME TO SRC MOSAIC PROGRAM         *"<<endl;
    cout<<"      *                                               *"<<endl;
    cout<<"      *************************************************"<<endl;
    cout<<endl;

//#################################################################
//  Begin timing the code....
//#################################################################

    int i, j, k;    
    time_t start,end;
    clock_t cpu_start, cpu_end;

    double clocks_per_second = (double) CLOCKS_PER_SEC;
    start = time(NULL);
    cpu_start = clock();

//#################################################################
//  Get config file
//#################################################################

    if(argc!=2)
    {
      cout<<"Usage:  mosaic /path/config_file"<<endl;
      cout<<"Exiting from src_mosaic_main."<<endl;
      exit(0);
    }

    string configfile;
    configfile.assign(argv[1]);

//#################################################################
//  Get configuration variables
//#################################################################
    time_t  now;
    now = time(NULL);

    struct tm *gmtime0 = gmtime(&now);

    cout<<"\n    Process runs at (UTC): "<<gmtime0->tm_mon+1<<'/'
        <<gmtime0->tm_mday <<'/' <<gmtime0->tm_year+1900<<','
        << gmtime0->tm_hour<<':'<<gmtime0->tm_min<<endl<<endl;

    CONFIG_PARS pp(configfile);      // a class includes all config parameters

    //get all parameters from config file
    pp.get_config();

    //get corner coordinate of each radar in domain
    //and corner coordinate of domain
    pp.get_corner_coordinate();

    if(pp.debug_flag)
        cout<<"This is version May 10,2006 "<<endl;

//######################################################################
//  GMT and Local time difference.
//######################################################################   

    int gmt_m_lcl = 0;
    int itime0 = 0;

    char timeString[20];   // time string used in product file names
    char nc_timeString[20];   // time string used in product file names

    get_time_realtime(pp, gmt_m_lcl, itime0);

//######################################################################   
//  Find the available polar data files in archive mode.
//######################################################################
    get_attr_realtime(pp,itime0, gmt_m_lcl,
                     timeString,nc_timeString);

//#######################################################################
//  update radar number and related radar id array in specific domain
//#######################################################################

   int xmin,xmax,ymin,ymax;
   //int id[pp.radar_number];
   int id[160];

   //set parameters for specific domain
   xmin = pp.xmin;  
   xmax = pp.xmax;
   ymin = pp.ymin;
   ymax = pp.ymax;

   long int num = 0;
   //check each radar whether it lies in the domain
   for( i = 0; i< pp.radar_number; i++)
   {
      if( pp.radar_pars_list[i].xmin <= xmin && pp.radar_pars_list[i].xmax >= xmin) 
      {
          if( ( pp.radar_pars_list[i].ymin <= ymin && pp.radar_pars_list[i].ymax >= ymin )||
              ( pp.radar_pars_list[i].ymin <= ymax && pp.radar_pars_list[i].ymax >= ymax )||
              ( pp.radar_pars_list[i].ymin >= ymin && pp.radar_pars_list[i].ymax <= ymax ))
          {
              id[num] = i;
              num++;
          } 
      }
      else if(pp.radar_pars_list[i].xmin <= xmax && pp.radar_pars_list[i].xmax >= xmax) 
      { 
          if( (pp.radar_pars_list[i].ymin <= ymin && pp.radar_pars_list[i].ymax >= ymin) ||
              (pp.radar_pars_list[i].ymin <= ymax && pp.radar_pars_list[i].ymax >= ymax) ||
              (pp.radar_pars_list[i].ymin >= ymin && pp.radar_pars_list[i].ymax <= ymax) )
          {
              id[num] = i;
              num++;
          } 
      }
      else if( pp.radar_pars_list[i].xmin >= xmin && pp.radar_pars_list[i].xmax <= xmax )
      {
          if( (pp.radar_pars_list[i].ymin <= ymin && pp.radar_pars_list[i].ymax >= ymin) ||
              (pp.radar_pars_list[i].ymin <= ymax && pp.radar_pars_list[i].ymax >= ymax) ||
              (pp.radar_pars_list[i].ymin >= ymin && pp.radar_pars_list[i].ymax <= ymax) )
          {
              id[num] = i;
              num++;
          }
      }
   }

   pp.radar_number = num;
 
   if( num == 0 ) 
   {
      cout<<"No any radar data in this domain"<<endl;
      exit(0);
   }


//#######################################################################
// read satellite qc mask
//#######################################################################

time_t start2,end2;
clock_t cpu_start2, cpu_end2;
start2 = time(NULL);
cpu_start2 = clock();   

     short *ir_mask = 0;
     short *eca_mask = 0;
     short *ctp_mask = 0;
     short *sat_mask = 0;
     int num_mask = pp.nx*pp.ny;
/*
     //read mask data and reset all relevent options
     //set array sat_mask
     if( pp.sat_mask_qc_opt == 1 )
     {
        if( pp.ir_mask_opt == 1 && read_sat_qc_mask( ir_mask, pp, 1 ) == 0 ) 
               pp.ir_mask_opt = 0;
        if( pp.eca_mask_opt == 1 && read_sat_qc_mask( eca_mask, pp, 2 ) == 0 )
               pp.eca_mask_opt = 0;
        if( pp.ctp_mask_opt == 1 && read_sat_qc_mask( sat_mask, pp, 3 ) == 0 )
               pp.ctp_mask_opt = 0;
 
        //set array sat_mask
        if( pp.ir_mask_opt == 0 && pp.eca_mask_opt == 0 && pp.ctp_mask_opt == 0 )
            pp.sat_mask_qc_opt = 0;
        else
        {
           if( (pp.ctp_mask_opt == 0) )
               pp.sat_mask_qc_opt = 0;
           else
           {
              for( int i = 0; i < num_mask; i++ )
                  sat_mask[i] = ctp_mask[i];
           }
        }
     }
*/
     if( (pp.sat_mask_qc_opt == 0) || 
         ( (pp.ir_mask_opt == 0) && (pp.eca_mask_opt == 0) && (pp.ctp_mask_opt == 0) ) )
     {
       cout<<"Satellite QC Mask options are turned off"<<endl<<endl;
     }
     else
     {
       ir_mask = new short[num_mask];
       eca_mask = new short[num_mask];
       ctp_mask = new short[num_mask];
       sat_mask = new short[num_mask];
       for( int i = 0; i < num_mask; i++ )
       {
           ir_mask[i] = 1;
           eca_mask[i] = 1;
           ctp_mask[i] = 1;
           sat_mask[i] = 1;
       }

       //read mask data and reset all relevent options
       //set array sat_mask
       if( pp.sat_mask_qc_opt == 1 )
       {
         if( pp.ir_mask_opt == 1 && read_sat_qc_mask( ir_mask, pp, 1 ) == 0 ) 
         {
               cout<<"Not using satellite IR QC mask"<<endl;
               pp.ir_mask_opt = 0;
         }
         if( pp.eca_mask_opt == 1 && read_sat_qc_mask( eca_mask, pp, 2 ) == 0 )
         {
               cout<<"Not using satellite ECA QC mask"<<endl;
               pp.eca_mask_opt = 0;
         }
         if( pp.ctp_mask_opt == 1 && read_sat_qc_mask( ctp_mask, pp, 3 ) == 0 )
         {
               cout<<"Not using satellite CTP QC mask"<<endl;
               pp.ctp_mask_opt = 0;
         }
         
         //set array sat_mask
         if( pp.ir_mask_opt == 0 && pp.eca_mask_opt == 0 && pp.ctp_mask_opt == 0 )
         {
            cout<<"No Satellite QC Mask data available!"<<endl<<endl;
            pp.sat_mask_qc_opt = 0;
         }
         else
         {
           bool keep;
           
           for(int i = 0; i < num_mask; i++)
           {
             keep = false;
             
             if( pp.ir_mask_opt && ir_mask[i] ) keep = true;              
             if( pp.eca_mask_opt && eca_mask[i] ) keep = true;             
             if( pp.ctp_mask_opt && ctp_mask[i] ) keep = true;
             
             if(!keep) sat_mask[i] = 0;
             
           }//end i-loop
           
         }//end mask opt check if-blk
         
       }//end pp.sat_mask_qc_opt == 1 if-blk

       //reclaim above spaces
       delete [] ir_mask;
       delete [] eca_mask;
       delete [] ctp_mask;
       
     }//end satellite flag if-blk

end2 = time(NULL);
cpu_end2 = clock();

//#######################################################################
// read RUC sfc temperature
//#######################################################################
    float** ruc_tsfc = 0;
    bool ruc_tsfc_valid = false;
//    ruc_tsfc = read_RUC_tsfc(pp, pp.ruc_tsfc_dir, pp.epoch_seconds,
//                             pp.ruc_tsfc_window, pp.debug_flag);
    
    if(ruc_tsfc != 0) ruc_tsfc_valid = true;
/*
    cout<<"ruc_tsfc = "<<ruc_tsfc<<endl;
    cout<<pp.conus_nx<<" "<<pp.conus_ny<<endl;
    ofstream outF("./tsfc_tmp.txt");
    if(outF)
    {
      outF<<pp.conus_nx<<" "<<pp.conus_ny<<endl;
      for(int j = pp.conus_ny-1; j >= 0 ; j--)
      {
        for(int i = 0; i < pp.conus_nx; i++) outF<<ruc_tsfc[i][j]<<" ";
        outF<<endl;
      }
    }
    outF.close();
    
    for(int i = 0; i < pp.conus_nx; i++) delete [] ruc_tsfc[i];
    delete [] ruc_tsfc;
    
    return 0;    
*/

    cout<<"radar num: "<< pp.radar_number<<endl;
//#######################################################################
//  initialize the varibles
//#######################################################################
//    short *i2tem = 0;
    short *init = 0;
    float *sum_wgt = 0;
    float *sum_ref = 0;
    short ref_miss_temp = -9990;
    short radar_cover_flag_temp = -990;   // a reflectivity flag indicating radar coverage

    cout<<"tile_nx,ny,nz: "<<pp.nx<<","<<pp.ny<<","<<pp.nz<<endl;
//Big loop for all output data types
    num = pp.nx*pp.ny*pp.nz;

    init = new short[num];
    for( i = 0; i < num; i++) init[i] = 0;

    cout<<"INIT"<<endl;

    sum_ref = new float[num];
    for( i = 0; i < num; i++) sum_ref[i] = (float)ref_miss;

    cout<<"SUM_REF"<<endl; 

    sum_wgt = new float[num];
    for( i = 0; i < num; i++) sum_wgt[i] = 0;

    cout<<"SUM_WGT"<<endl;


//#######################################################################
//  loop for each radar......
//#######################################################################
    int tempXmin,tempYmin,tempXmax,tempYmax,index0;
    int tempXmin0,tempYmin0;
    int xstart = 0;
    int xend   = 0;
    int ystart = 0;
    int yend   = 0;
    int nradar_has_data = 0;
//    char inputfile[200];
    string input_file;
    string input_file_temp;
    short file_yes = 0;
    long int index = 0;
    double x,y;
    float wgt;
//    char temp_nc_timeString[20];
  //  char radarnam[pp.radar_number][5];
      char radarnam[160][5];
    float varscale = 10.0;
    float R2 = pow(25.0,2.0);
//    int time_num = 0;
///    time_t epoch_seconds = 0;

    //Variable declarations for WDSS-II file I/O (2/17/2006 CL)
//    W2DataField* w2DF = 0;
//    LatLonHeightField* llhF = 0;
//    vector<string> input_file_list;
    long oldest_epoch = pp.epoch_seconds - pp.mosaic_window;
    //Note pp.epoch_seconds is already normalized to mosaic_freq

time_t start3,end3;
clock_t cpu_start3, cpu_end3;
double clock_time3 = 0, cpu_time3 = 0; 
/*
start3 = time(NULL);
cpu_start3 = clock();   
*/
time_t start4,end4;
clock_t cpu_start4, cpu_end4;
double clock_time4 = 0, cpu_time4 = 0; 

time_t start5,end5;
clock_t cpu_start5, cpu_end5;
double clock_time5 = 0, cpu_time5 = 0; 

    //loop through all radar 
    pp.variable_index = 0;
    char path_char[100];
    char time_char[20];
    char fname_char[200];
    float ***src_data; 
    struct SRC_DATA_HEADER src_header; 
    short file_exist_flag = 0; 
   
    cout<<endl<<endl;
    
    for (int m = 0; m< pp.radar_number; m++)
    {    
      index0 = id[m];

      if( (pp.radar_pars_list[index0].node_no1 < 0) && 
          (pp.radar_pars_list[index0].node_no2 < 0) ) continue;
      
      if(pp.debug_flag) cout<<"------- Processing radar "<<(m+1)<<endl;
      
      if(pp.input_flag)  //--- Read in SRC data! ---
      {
        //Build path to the level 2 data        
        sprintf(path_char, "%s/%s", pp.input_dir, pp.radar_pars_list[index0].radar_name);
        
        //Search for level 2 data    
        if(pp.debug_flag) 
          cout<<"  Searching files for "<<pp.radar_pars_list[index0].radar_name<<endl;
        
        file_exist_flag = 0;   

        for(int itime = pp.epoch_seconds; itime > oldest_epoch; itime --)   
         {
           time_t seconds_epoch = itime;
           strftime(time_char, 20, "%Y%m%d_%H%M%S", gmtime(&seconds_epoch) );
           sprintf(fname_char, "%s/%s_%s.src", path_char,pp.radar_pars_list[index0].radar_name, time_char);

   //        cout<<"file: "<<fname_char<<endl;

           ifstream inFile(fname_char, ios::binary | ios:: in);
           if(inFile)
           { 
             cout<<" Opening file: "<<fname_char<<endl;            
             file_exist_flag = 1;
             inFile.close();
             break;
           } 
         } // for (itime)

         if( file_exist_flag == 1 )
         {

           ifstream inFile(fname_char, ios::binary | ios:: in);
           inFile.seekg(4);
           inFile.read((char*)&src_header, sizeof(struct SRC_DATA_HEADER));
 
           cout<<"src_header: "<<endl;
           cout<<src_header.dim_1<<endl;
           cout<<src_header.dim_2<<endl;
           cout<<src_header.dim_3<<endl;
           cout<<src_header.dx<<endl; 
           cout<<src_header.dy<<endl;
           cout<<src_header.nw_lat<<endl;
           cout<<src_header.nw_lon<<endl;
           cout<<src_header.missing_value<<endl;

start4 = time(NULL);
cpu_start4 = clock();   

          //Check dimensions and lat/lon in config file vs input file.  
          //If mis-matched, continue to next radar.
          float config_nw_lat, config_nw_lon;
          config_nw_lat = pp.radar_pars_list[index0].ctrlat + 
                         (pp.radar_pars_list[index0].ny/2)*pp.radar_pars_list[index0].dy;
          config_nw_lon = pp.radar_pars_list[index0].ctrlon - 
                         (pp.radar_pars_list[index0].nx/2)*pp.radar_pars_list[index0].dx;
                         
                    
          if( (pp.radar_pars_list[index0].nx != src_header.dim_1) ||
              (pp.radar_pars_list[index0].ny != src_header.dim_2) ||
              (pp.radar_pars_list[index0].nz != src_header.dim_3) ||
              (pp.radar_pars_list[index0].dx != src_header.dx) ||
              (pp.radar_pars_list[index0].dy != src_header.dy) ||
              (fabs(src_header.nw_lat - config_nw_lat) > 0.1) ||
              (fabs(src_header.nw_lon - config_nw_lon) > 0.1) ) 
          {
            cout<<"++WARNING: Domain parameters for "
                <<pp.radar_pars_list[index0].radar_name<<" do not match!"<<endl;
            cout<<"   input file  :  config file"<<endl;
            cout<<"   "<<src_header.dim_2<<" != "<<pp.radar_pars_list[index0].nx<<endl;
            cout<<"   "<<src_header.dim_3<<" != "<<pp.radar_pars_list[index0].ny<<endl;
            cout<<"   "<<src_header.dim_1<<" != "<<pp.radar_pars_list[index0].nz<<endl;
            cout<<"   "<<src_header.dx<<" != "<<pp.radar_pars_list[index0].dx<<endl;
            cout<<"   "<<src_header.dy<<" != "<<pp.radar_pars_list[index0].dy<<endl;
            cout<<"   "<<src_header.nw_lat<<" != "<<config_nw_lat<<endl;
            cout<<"   "<<src_header.nw_lon<<" != "<<config_nw_lon<<endl;
            cout<<"Skipping this radar!"<<endl;
            
            continue;
          }
          
          
          //Check height values for all 31 levels. If more than one
          //height level is at zero MSL, then it is likely that the 
          //SRC is corrupt and should be ignored.
          bool found_zero_msl = false;
          
          for(k = 0; k < NUM_LEVEL; k++)
          {
            if(src_header.zp[k] <= 0.0) 
            {
              found_zero_msl = true;
              break;
            }
          }
          
          if(found_zero_msl)
          {
            cout<<"++WARNING: SRC may be corrupt.  Skipping!"<<endl;
            continue;
          }

          src_data = new float **[src_header.dim_3];
          for( k=0; k<src_header.dim_3; k++)
          {
           src_data[k] = new float *[src_header.dim_2];
           for( j=0; j<src_header.dim_2; j++)  
           {
            src_data[k][j] = new float [src_header.dim_1];
            for( i=0; i<src_header.dim_1; i++)
            src_data[k][j][i] = src_header.missing_value;     
           }
          }
 
         int file_position = 12 + sizeof(struct SRC_DATA_HEADER);
         inFile.seekg(file_position);


          for(i = 0; i<src_header.dim_1; i++)
           for(j = 0; j<src_header.dim_2; j++)
            for(k = 0; k<src_header.dim_3; k++)
             inFile.read((char *)&src_data[k][j][i], sizeof(float)); 

          inFile.close();

          cout<<endl;
          cout<<"src_data: "<<endl;

          cout<<src_data[0][0][0]<<","<<src_data[0][238][297]<<endl;
          cout<<src_data[0][238][298]<<endl; 
          cout<<src_data[3][416][314]<<endl;
          cout<<src_data[2][223][291]<<endl;

          
end4 = time(NULL);
cpu_end4 = clock();

clock_time4 += difftime(end4,start4);
cpu_time4 += (double)(cpu_end4-cpu_start4)/clocks_per_second;


          //Set some values needed by rest of program
          float ref_miss_float = src_header.missing_value;
          float no_coverage_float = ref_miss_float - 3;
          varscale = 1.0;
          radar_cover_flag_temp = (short)(radar_cover_flag*varscale - 0.49 );
          
          strcpy(pp.varname[0] , "mosaicked refl");
        //  strcpy(pp.varunit[0], llhF->variable_units.c_str());
          pp.varscale[0] = ref_scale; //1;
          pp.missing[0]  = ref_miss*ref_scale; //(short)llhF->missing_value;
          pp.mint[0] = -200;
          pp.maxt[0] = 800;
          
          strcpy(radarnam[nradar_has_data],pp.radar_pars_list[index0].radar_name);
          nradar_has_data++;
          file_yes = 1;
        
start3 = time(NULL);
cpu_start3 = clock();   
          
          //Perform bloom QC check.
          if(ruc_tsfc_valid)
          {

          }
          else
          {
            cout<<"++WARNING: RUC Tsfc data invalid.  Not applying"
                <<" bloom QC logic!"<<endl;
          }
end3 = time(NULL);
cpu_end3 = clock();

clock_time3 += difftime(end3,start3);
cpu_time3 += (double)(cpu_end3-cpu_start3)/clocks_per_second;

               
start5 = time(NULL);
cpu_start5 = clock();   
          //get boundary limit parameters    
          tempXmin = pp.radar_pars_list[index0].xmin;
          tempYmin = pp.radar_pars_list[index0].ymin;
          tempXmax = tempXmin + pp.radar_pars_list[index0].nx -1;  
          tempYmax = tempYmin + pp.radar_pars_list[index0].ny -1;    
    
          if( tempXmin <= xmin )
          {
            xstart = xmin - tempXmin;
            tempXmin0 = 0;
          }
          else 
          {      
            xstart = 0;
            tempXmin0 = tempXmin - xmin;  
          }

          if( tempXmax >= xmax )
            xend = xmax - tempXmin;
          else xend = tempXmax - tempXmin; 

          if( tempYmin <= ymin )
          {
            ystart = ymin - tempYmin;
            tempYmin0 = 0;
          }
          else if( tempYmin >= ymin )
          {
            ystart = 0;
            tempYmin0 = tempYmin - ymin;
          }

          if( tempYmax <= ymax )
            yend = tempYmax - tempYmin;
          else yend = ymax - tempYmin;
 
          //patch single radar data onto specific domain 
          tempYmin0 = tempYmin0 - ystart;
          tempXmin0 = tempXmin0 - xstart; 
          
          int w2_j = -1;
          
          cout<<"  Mosaicking data from "
              <<pp.radar_pars_list[index0].radar_name<<endl<<endl;

          for( k = 0; k < pp.radar_pars_list[index0].nz; k++)
          for( i = xstart; i <= xend; i++)
          for( j = ystart; j <= yend; j++)
          {
            
            index = k*pp.nx*pp.ny + (j + tempYmin0 )*pp.nx + i + tempXmin0; 
            
            if(init[index] == 0 )
            {
               init[index] = 1;
               
               if(src_data[k][j][i] > ref_miss_float)
               {
                 init[index] = 2;
                 x = (double)(pp.radar_pars_list[index0].xmin + i - pp.radar_pars_list[index0].ctrx);
                 y = (double)(pp.radar_pars_list[index0].ymin + j - pp.radar_pars_list[index0].ctry);
                 wgt = exp( -(pow(x,2.0) + pow(y, 2.0) )/R2 );
                 if( (wgt < 1.0e-30 ) || (wgt > 1.0) )    wgt = 1.0e-30;
                 sum_wgt[index] = wgt;
                 sum_ref[index] = (float)src_data[k][j][i]*wgt;
               }
               else if(src_data[k][j][i] > no_coverage_float)
                 sum_ref[index] = (float)radar_cover_flag_temp;
               else sum_ref[index] = (float)ref_miss;
               
            }
            else if(init[index] == 1 )
            {

               if(src_data[k][j][i] > ref_miss_float)
               {
                 init[index] = 2;
                 x = (double)(pp.radar_pars_list[index0].xmin + i - pp.radar_pars_list[index0].ctrx);
                 y = (double)(pp.radar_pars_list[index0].ymin + j - pp.radar_pars_list[index0].ctry);
                 wgt = exp( -(pow(x,2.0) + pow(y, 2.0) )/R2 );
                 if( (wgt < 1.0e-30 ) || (wgt > 1.0) )    wgt = 1.0e-30;
                 sum_wgt[index] = wgt;
                 sum_ref[index] = (float)src_data[k][j][i]*wgt;               
               }
               else if( (src_data[k][j][i] > no_coverage_float) &&
                        (sum_ref[index] == ref_miss) ) //ref_miss_float) )
               {
                 sum_ref[index] = (float)radar_cover_flag_temp;
               }
              
            }
            else if(init[index] == 2 )
            {

              if(src_data[k][j][i] > ref_miss_float)
              {
                x = (double)(pp.radar_pars_list[index0].xmin + i - pp.radar_pars_list[index0].ctrx);
                y = (double)(pp.radar_pars_list[index0].ymin + j - pp.radar_pars_list[index0].ctry);
                wgt = exp( -(pow(x,2.0) + pow(y, 2.0) )/R2 );
                if( (wgt < 1.0e-30) || (wgt > 1.0) )    wgt = 1.0e-30;
                sum_wgt[index] += wgt;
                sum_ref[index] += (float)src_data[k][j][i]*wgt;
              }
              
            }//end init[index] if-blk
            
          }//end k,j,i loop
          
                            
                            
          //Free-up space
          //delete src_data
          for(k = 0; k<src_header.dim_3; k++)
          {
           for(j = 0; j<src_header.dim_2; j++)  
            delete [] src_data[k][j];
           delete [] src_data[k];
          }
           delete [] src_data; 

end5 = time(NULL);
cpu_end5 = clock();

clock_time5 += difftime(end5,start5);
cpu_time5 += (double)(cpu_end5-cpu_start5)/clocks_per_second;

        }
        else
        {
          cout<<"++WARNING: no files found for "<<pp.radar_pars_list[index0].radar_name<<endl;
        }
                               
      }
      else //--- Read in AWIPS NetCDF data! ---
      {

      }//end pp.wdssii_flag if-blk
      
      //cout<<"-------  Done processing radar "<<(m+1)<<endl;
      
    } //end of for(m)

//end3 = time(NULL);
//cpu_end3 = clock();

    if(ruc_tsfc != 0)
    {
    }

    pp.nradars = nradar_has_data; 

    if(!file_yes)
    {
       cout<<"NO data within time window.Exiting mosaic..."<<endl;
       delete [] sat_mask;
       delete [] init;
       delete [] sum_ref;
       delete [] sum_wgt;
       exit(0);
    }

//#######################################################################
//  Mosaic or compute the final weighted mean
//#######################################################################

     cout<<endl<<endl<<"****Starting final mosaic"<<endl;

     int index_for_mask;
     float*** sum_wref;
     float echo_pcnt = 0;
/*
time_t start6,end6;
clock_t cpu_start6, cpu_end6;
start6 = time(NULL);
cpu_start6 = clock();   
*/
 //   i2tem = new short[num];
 //   for( i = 0; i < num; i++) i2tem[i] = ref_miss_temp;

     sum_wref = new float**[pp.nz];
     for( k = 0; k < pp.nz; k++)
     {
         sum_wref[k] = new float*[pp.ny];
         for( j = 0; j < pp.ny; j++)
             sum_wref[k][j] = new float[pp.nx];
     }

//end6 = time(NULL);
//cpu_end6 = clock();

time_t start7,end7;
clock_t cpu_start7, cpu_end7;
start7 = time(NULL);
cpu_start7 = clock();   


     for( k = 0; k < pp.nz; k++)
     {
       float echo_count = 0;
       for( j = 0; j < pp.ny; j++)
       {
          index = k*pp.nx*pp.ny + j*pp.nx;
          index_for_mask = j*pp.nx;
          for( i = 0; i < pp.nx; i++)
          {

              if( init[index + i] == 2 )
              {     
                 if( sum_wgt[index + i] > 0.0 )
                 {
                   //  i2tem[index + i] = (short)(sum_ref[index + i]/sum_wgt[index + i] + 0.49 )*ref_scale;
                     sum_wref[k][j][i] = sum_ref[index + i]/(sum_wgt[index + i]*varscale);

                     //check the mask and do qc
                     if( pp.sat_mask_qc_opt == 1 &&
                         sat_mask[index_for_mask+i] == 0 )
                     {
                       //  i2tem[index + i]  = radar_cover_flag_temp*ref_scale;
                         sum_wref[k][j][i] = radar_cover_flag;
                     }
                 }
                 else 
                 {
                   //  i2tem[index + i]  = radar_cover_flag_temp*ref_scale;
                     sum_wref[k][j][i] = radar_cover_flag; 
                 }
              } 
              else if( init[index + i] == 1 )
              {
                 //i2tem[index + i]  = (short)sum_ref[index + i]*ref_scale;
                 sum_wref[k][j][i] = sum_ref[index + i]/varscale ;
              }
              else if( init[index + i] == 0 )
              {
                 // i2tem[index + i]  = ref_miss*ref_scale; //ref_miss_temp;
                 sum_wref[k][j][i] = (float)ref_miss;
              }
       
              if (sum_wref[k][j][i] > (float) dbz_clear_air + 1.0e-5) echo_count +=1.0;
          }   
       }//end of (j)
       if (echo_count > echo_pcnt) echo_pcnt = echo_count;
     }//end of (k)

     echo_pcnt = echo_pcnt / (float) (pp.nx*pp.ny) * 100.0; 
    
     
     //Delete summation reflectivity and weight arrays
    // delete [] sat_mask;
     delete [] init;
     delete [] sum_ref;
     delete [] sum_wgt;

end7 = time(NULL);
cpu_end7 = clock();

//*** construct mosaic_header *****//
 struct MOSAIC_DATA_HEADER mosaic_header;

      mosaic_header.nx = pp.nx;
      mosaic_header.ny = pp.ny;
      mosaic_header.nz = pp.nz;
      mosaic_header.dx = pp.dx;
      mosaic_header.dy = pp.dy; 
      mosaic_header.ctrl_lat = pp.ctrlat;
      mosaic_header.ctrl_lon = pp.ctrlon;
      
      for(int i=0; i< pp.nz; i++) 
       mosaic_header.zp[i] = pp.zp[i] ;

      mosaic_header.missing_value = radar_cover_flag; 
      mosaic_header.no_radar_cover = (float)ref_miss; 


// ***** output sum_wref  *********************//

   char output_filename[500];
   char timetmp[20];


   time_t seconds_epoch = pp.epoch_seconds ;
   strftime(timetmp, 20, "%Y%m%d_%H%M", gmtime(&seconds_epoch)); 
   sprintf(output_filename, "%s/%s.mosaic", pp.mosaic3d_netcdf_dir, timetmp);
 
   cout<<"Writting file: "<<output_filename<<endl;
   cout<<"Sliu output: "<<mosaic_header.nx<<endl;
   cout<<"Sliu output: "<<mosaic_header.ny<<endl;
   cout<<"Sliu output: "<<mosaic_header.nz<<endl;

   ofstream outFile(output_filename, ios::binary | ios:: out);

   if(!outFile)
   {
     cout<<"ERROR, writting...  "<<output_filename<<endl;
     exit(0);
   }

   outFile.write(reinterpret_cast<const char*>(&mosaic_header), sizeof(struct MOSAIC_DATA_HEADER));
    cout<<"      *SLIU*****************************"<<endl;

   cout<<"Sliu output: "<<sizeof(struct MOSAIC_DATA_HEADER)<<","<<sizeof(struct SRC_DATA_HEADER)<<endl;



   for(k=0; k<pp.nz; k++)
    for(j=0; j<pp.ny; j++)
     outFile.write(reinterpret_cast<const char*>(&sum_wref[k][j][0]), pp.nx*sizeof(float));

/**********************************************/
/**** 6. compute and  output 2D products  *****/
/**********************************************/

/****     6.1 CREF and CREFH    ********/

  float **CREF, **CREFH;

  CREF = new float * [pp.ny];
  CREFH = new float * [pp.ny];
  for(j=0; j< pp.ny; j++)
  {
      CREF[j] = new float [pp.nx];
      CREFH[j] = new float [pp.nx];
  }

 produce_CREF_CREFH_new (pp, sum_wref, CREF, CREFH ); 

    for(j=0; j<pp.ny; j++)
     outFile.write(reinterpret_cast<const char*>(&CREF[j][0]), pp.nx*sizeof(float));

     for(j=0; j<pp.ny; j++)
     outFile.write(reinterpret_cast<const char*>(&CREFH[j][0]), pp.nx*sizeof(float));


 for(j=0; j<pp.ny; j++)
  {
      delete [] CREF[j];
      delete [] CREFH[j];
  }
  delete [] CREF;
  delete [] CREFH;
 
/****    6.2 ETP     *****************/

   float ** ETP;

   ETP = new float * [pp.ny];
   for(j=0; j<pp.ny; j++) ETP[j] = new float[pp.nx];

    produce_ETP_new(pp, sum_wref, ETP);

    for(j=0; j<pp.ny; j++)
     outFile.write(reinterpret_cast<const char*>(&ETP[j][0]), pp.nx*sizeof(float));


   for(j=0; j<pp.ny; j++)
      delete [] ETP[j];
   delete [] ETP;

/****   6.3 HDA: SHI, POSH, MEHS *******/

  float **shi, **posh, **mehs;

  shi  = new float * [pp.ny];
  posh = new float * [pp.ny];
  mehs = new float * [pp.ny];
  for(j=0; j<pp.ny; j++)
  {
     shi[j]  = new float [pp.nx];
     posh[j] = new float [pp.nx];
     mehs[j] = new float [pp.nx];
  }

  float*** kh_tlvls_tmp = 0;

  
 produce_HDA_new(pp, sum_wref, kh_tlvls_tmp,
                 shi, posh, mehs );

  for(j=0; j<pp.ny; j++)
     outFile.write(reinterpret_cast<const char*>(&shi[j][0]), pp.nx*sizeof(float));

     for(j=0; j<pp.ny; j++)
     outFile.write(reinterpret_cast<const char*>(&posh[j][0]), pp.nx*sizeof(float));
   for(j=0; j<pp.ny; j++)
     outFile.write(reinterpret_cast<const char*>(&mehs[j][0]), pp.nx*sizeof(float));

   for(j=0; j<pp.ny; j++)
  {
      delete [] shi[j];
      delete [] posh[j];
      delete [] mehs[j];
  }
  delete [] shi;
  delete [] posh;
  delete [] mehs;

/****   6.4 HSR and HSRH_MSL   *****/

   float **HSR, **HSRH;
 
  HSR = new float * [pp.ny];
  HSRH = new float * [pp.ny];
  for(j=0; j< pp.ny; j++)
  {
      HSR[j] = new float [pp.nx];
      HSRH[j] = new float [pp.nx];
  }

    produce_HSR_and_HSRH_new(pp, sum_wref,
                  HSR, HSRH) ;

   for(j=0; j<pp.ny; j++)
     outFile.write(reinterpret_cast<const char*>(&HSR[j][0]), pp.nx*sizeof(float));

     for(j=0; j<pp.ny; j++)
     outFile.write(reinterpret_cast<const char*>(&HSRH[j][0]), pp.nx*sizeof(float));



    for(j=0; j<pp.ny; j++)
  {
      delete [] HSR[j];
      delete [] HSRH[j];
  }
  delete [] HSR;
  delete [] HSRH;


/****   6.5 LCREF and LCREFH_MSL *****/
  float **LCREF, **LCREFH;

  LCREF = new float * [pp.ny];
  LCREFH = new float * [pp.ny];
  for(j=0; j< pp.ny; j++)
  {
      LCREF[j] = new float [pp.nx];
      LCREFH[j] = new float [pp.nx];
  }

produce_LCREF_and_LCREFH_new(pp, sum_wref,
                  LCREF, LCREFH);

   for(j=0; j<pp.ny; j++)
     outFile.write(reinterpret_cast<const char*>(&LCREF[j][0]), pp.nx*sizeof(float));

     for(j=0; j<pp.ny; j++)
     outFile.write(reinterpret_cast<const char*>(&LCREFH[j][0]), pp.nx*sizeof(float));


    for(j=0; j<pp.ny; j++)
  {
      delete [] LCREF[j];
      delete [] LCREFH[j];
  }
  delete [] LCREF;
  delete [] LCREFH;




/****    6.6 PCP_FLAG *********/
  short **PCPFLAG = 0;

  PCPFLAG = new short *[pp.ny];
  for(j=0; j< pp.ny; j++) PCPFLAG[j] = new short [pp.nx];

 produce_PCPFLAG_new (pp, sum_wref, kh_tlvls_tmp,
                      PCPFLAG);
   for(j=0; j<pp.ny; j++)
     outFile.write(reinterpret_cast<const char*>(&PCPFLAG[j][0]), pp.nx*sizeof(short));

 

   for(j=0; j<pp.ny; j++) delete [] PCPFLAG[j];
  delete [] PCPFLAG;



/*****   6.7 VIL and VILD ******/
  
  float **vil, **vilD;
  float **terrain;

  vil = new float * [pp.ny];
  vilD = new float * [pp.ny];
  terrain = new float *[pp.ny]; 
  for(j=0; j<pp.ny; j++)
  {
     vil[j]  = new float[pp.nx];
     vilD[j] = new float[pp.nx];
     terrain[j] = new float[pp.nx];
  }

 produce_VIL_and_VILD_new(pp, sum_wref,
                   vil, vilD, terrain);

   for(j=0; j<pp.ny; j++)
     outFile.write(reinterpret_cast<const char*>(&vil[j][0]), pp.nx*sizeof(float));

     for(j=0; j<pp.ny; j++)
     outFile.write(reinterpret_cast<const char*>(&vilD[j][0]), pp.nx*sizeof(float));


   for(j=0; j<pp.ny; j++)
  {
     delete [] vil[j];
     delete [] vilD[j];
     delete [] terrain[j];
  }
  delete [] vil;
  delete [] vilD;
  delete [] terrain;





/***********************************/
/*** output end ********************/
/***********************************/



   outFile.close();

   cout<<"Writting Done"<<endl;

//   cout<<sum_wref[0][0][0]<<endl;
  
//   for(k=0; k<pp.nz; k++)
//    for(j=0; j<pp.ny; j++)
//     for(i=0; i<pp.nx; i++)
//       {
//         if(sum_wref[k][j][i] > -10.) 
//          cout<< sum_wref[k][j][i] <<","<<k<<","<<j<<","<<i<<endl; 
//       }      



//#######################################################################
//  output the data into netcdf file
//#######################################################################

time_t start8,end8;
clock_t cpu_start8, cpu_end8;

start8 = time(NULL);
cpu_start8 = clock();
end8 = time(NULL);
cpu_end8 = clock();


//time_t start9,end9;
//clock_t cpu_start9, cpu_end9;

    if( pp.mosaic3d_wdssii_netcdf_flag )
    {


start8 = time(NULL);
cpu_start8 = clock();   

      if(pp.debug_flag) cout<<"***Writing WDSS-II 3D file***"<<endl;

end8 = time(NULL);
cpu_end8 = clock();

//start9 = time(NULL);
//cpu_start9 = clock();   
      
      //If remote option required, copy WDSS-II ncdf to remote directory
      if( pp.mosaic3d_netcdf_remote_output_opt )
      {

      }//end mosaic3d_netcdf_remote_output_opt if-blk for W2 output


      //transfer mosaic data to nmqserver by using LDM
      if(pp.ldm_netcdf_opt == 1 )
      {
      }   
//end9 = time(NULL);
//cpu_end9 = clock();

    }//end mosaic3d_wdssii_netcdf_flag if-blk

    if( pp.mosaic3d_netcdf_flag )
    {
        if(pp.debug_flag) cout<<"***Writing AWIPS 3D file***"<<endl;

       if( pp.mosaic3d_netcdf_remote_output_opt )
       {
       }
        //transfer mosaic data to nmqserver by using LDM
        if(pp.ldm_netcdf_opt == 1 )
        {
        }   
    } //end mosaic3d_netcdf_flag 

//#######################################################################
//  Setup map projection
//######################################################################
time_t start10,end10;
clock_t cpu_start10, cpu_end10;
start10 = time(NULL);
cpu_start10 = clock();   

    setmapr sm;
    sm.setmap_proj(pp);

    if(pp.debug_flag) cout<<pp.dx<<" "<<pp.dy<<" "<<pp.ctrlat<<endl;

//######################################################################
//  Setup Cartesian analysis grid and find lat/lon for each grid point
//######################################################################

    grid_vars gv (pp.nx, pp.ny, pp.nz);
    gv.ini_grd (pp, sm);
    for(k = 0; k < pp.nz; k++) gv.zp[k] = pp.zp[k];
    
end10 = time(NULL);
cpu_end10 = clock();

//#######################################################################
//  Output 3D mosaic Binary files.
//#######################################################################
time_t start11,end11;
clock_t cpu_start11, cpu_end11;
start11 = time(NULL);
cpu_start11 = clock();   


    if( pp.bi_mref_flag )
    {
    }
    
    //Delete array storing binary values
//    delete [] i2tem;


end11 = time(NULL);
cpu_end11 = clock();

//######################################################################
//  Re-project the mosaic grid into a 1 km x 1 km NIDS raster grid.
//######################################################################
    const float dx_nids_km = 1.0;  //km
    double deg2rad = 0.0174532;
    double dx_km = (double)pp.dx *111.20 * cos (deg2rad*pp.ctrlat);
    double dy_km = (double)pp.dy *111.20;

    nids_pars np;
    np.nx_n = (int) ( (double)pp.nx * dx_km/dx_nids_km);
    np.ny_n = (int) ( (double)pp.ny * dy_km/dx_nids_km);
    np.dlon_n = (double)pp.dx * dx_nids_km/dx_km;
    np.dlat_n = (double)pp.dy * dx_nids_km/dy_km;

///    short QPEcode=0;
//    float r_max_scale = 1.0;

//#######################################################################
//  Deriving pre-specified height level relectivity products(nids).
//#######################################################################
    
    if(pp.lref_opt==1)
    {
    } 

//###########################################################################
//  produce CREF and CREFH products and output them in nids and
//  netcdf formats etc.
//  produce VIL and VIL density products and output them in binary,
//  netcdf formats etc.
//  produce HDA products like SHI, POSH, MEHS and output them in binary,
//  netcdf formats etc.
//  produce ETP product and output it in binary, netcdf formats etc.
//  produce HSR and HSRH products and output them in binary,netcdf formats
//   etc.
//###########################################################################
time_t start1,end1;
clock_t cpu_start1, cpu_end1;
start1 = time(NULL);
cpu_start1 = clock();   
   
    
   if(pp.debug_flag) cout<<"Reading in model temperature index levels ..."<<endl;

   float ***kh_tlvls = 0;
/*
   kh_tlvls = new float **[pp.nx];
   for(i = 0; i < pp.nx; i++)
   {
     kh_tlvls[i] = new float *[pp.ny];
     for(j = 0; j < pp.ny; j++)
        kh_tlvls[i][j] = new float[n_tlevels];
   }
*/
   //get 3-D index_temperature array
   kh_tlvls = get_index_temp(itime0, pp, radarnam, gv, configfile, timeString);
   
   if(kh_tlvls == 0) 
     cout<<"++WARNING: failed to read in model temperature index levels!"<<endl;
     
end1 = time(NULL);
cpu_end1 = clock();

        
time_t start13,end13;
clock_t cpu_start13, cpu_end13;
start13 = time(NULL);
cpu_start13 = clock();   

   if( pp.debug_flag )
      cout<<"Produce and output 2D products ..."<<endl;

//   produce_2D_products (nc_timeString, pp, np, sum_wref, itime0, nradar_has_data,
//                 gv,timeString, configfile.c_str(), radarnam );

   produce_2D_products (nc_timeString, pp, np, sum_wref, kh_tlvls, itime0, 
                 nradar_has_data, gv,timeString, configfile.c_str(), radarnam);

end13 = time(NULL);
cpu_end13 = clock();

//#######################################################################
//  If pp.tref_opt is not equal to one, skip the derivation for
//  temperature level relectivity products.
//  Misc initializations are performed here to keep compiler happy.
//#######################################################################
time_t start12,end12;
clock_t cpu_start12, cpu_end12;
start12 = time(NULL);
cpu_start12 = clock();   

    if(pp.tref_opt==1) 
      { 
//      t_level_deriv (sum_wref, kh_tlvls, itime0, pp, radarnam, gv,np, configfile, nc_timeString,timeString);
      }
    else
      if(pp.debug_flag==1) cout<<endl<<"No temperature refl. products generated."<<endl;

end12 = time(NULL);
cpu_end12 = clock();

//###########################################################################
//  Free up the occupied space 
//###########################################################################

//    delete [] sat_mask;
//    delete [] i2tem;
//    delete [] init;
//    delete [] sum_ref;
//    delete [] sum_wgt;
    
    for(k = 0; k < pp.nz; k++)
    {
      for(i = 0; i < pp.ny; i++) delete [] sum_wref[k][i];
      delete [] sum_wref[k];
    }
    delete [] sum_wref;
    
    if(kh_tlvls != 0)
    {
      for(i = 0; i < pp.nx; i++)
      {
        for(j = 0; j < pp.ny; j++) delete [] kh_tlvls[i][j];
        delete [] kh_tlvls[i];
      }
      delete [] kh_tlvls;
    }
    
//}// end of data_type_index 
//#######################################################################
//  End Timing the code...
//#######################################################################
 
    double clock_time,cpu_time; 
    double f_clock_time, f_cpu_time;
    end = time(NULL);
    cpu_end = clock();

    clock_time = difftime(end2,start2);
    cpu_time   = (double)(cpu_end2-cpu_start2)/clocks_per_second;

    cout<<endl<<"Sat QC clock time: "<<clock_time<<endl;
    cout<<"Sat QC Total CPU: "<<cpu_time<<endl;
/*
    clock_time = difftime(end3,start3);
    cpu_time   = (double)(cpu_end3-cpu_start3)/clocks_per_second;

    cout<<endl<<"File input + wgt calc clock time: "<<clock_time<<endl;
    cout<<"File input + wgt calc Total CPU: "<<cpu_time<<endl;
*/
    cout<<endl<<"Radar input clock time: "<<clock_time4<<endl;
    cout<<"Radar input Total CPU: "<<cpu_time4<<endl;

    cout<<endl<<"Bloom QC clock time: "<<clock_time3<<endl;
    cout<<"Bloom QC Total CPU: "<<cpu_time3<<endl;

    cout<<endl<<"Wgt calc clock time: "<<clock_time5<<endl;
    cout<<"Wgt calc Total CPU: "<<cpu_time5<<endl;
/*
    clock_time = difftime(end6,start6);
    cpu_time   = (double)(cpu_end6-cpu_start6)/clocks_per_second;

    cout<<endl<<"sum_wref alloc clock time: "<<clock_time<<endl;
    cout<<"sum_wref alloc Total CPU: "<<cpu_time<<endl;
*/
    clock_time = difftime(end7,start7);
    cpu_time   = (double)(cpu_end7-cpu_start7)/clocks_per_second;

    cout<<endl<<"Mosaicking clock time: "<<clock_time<<endl;
    cout<<"Mosaicking Total CPU: "<<cpu_time<<endl;

    clock_time = difftime(end8,start8);
    cpu_time   = (double)(cpu_end8-cpu_start8)/clocks_per_second;

    cout<<endl<<"Wrt 3D W2 NCDF clock time: "<<clock_time<<endl;
    cout<<"Wrt 3D W2 NCDF Total CPU: "<<cpu_time<<endl;
/*
    clock_time = difftime(end9,start9);
    cpu_time   = (double)(cpu_end9-cpu_start9)/clocks_per_second;

    cout<<endl<<"Copy to RAID clock time: "<<clock_time<<endl;
    cout<<"Copy to RAID Total CPU: "<<cpu_time<<endl;
*/
    clock_time = difftime(end10,start10);
    cpu_time   = (double)(cpu_end10-cpu_start10)/clocks_per_second;

    cout<<endl<<"Map proj setup clock time: "<<clock_time<<endl;
    cout<<"Map proj setup Total CPU: "<<cpu_time<<endl;

    clock_time = difftime(end11,start11);
    cpu_time   = (double)(cpu_end11-cpu_start11)/clocks_per_second;

    cout<<endl<<"Wrt 3D WISH binary clock time: "<<clock_time<<endl;
    cout<<"Wrt 3D WISH binary Total CPU: "<<cpu_time<<endl;

    clock_time = difftime(end1,start1);
    cpu_time   = (double)(cpu_end1-cpu_start1)/clocks_per_second;

    cout<<endl<<"Read RUC time: "<<clock_time<<endl;
    cout<<"Read RUC Total CPU: "<<cpu_time<<endl;

    clock_time = difftime(end12,start12);
    cpu_time   = (double)(cpu_end12-cpu_start12)/clocks_per_second;

    cout<<endl<<"TREFL gen + output clock time: "<<clock_time<<endl;
    cout<<"TREFL gen + output Total CPU: "<<cpu_time<<endl;

    clock_time = difftime(end13,start13);
    cpu_time   = (double)(cpu_end13-cpu_start13)/clocks_per_second;

    cout<<endl<<"2D prod gen + output clock time: "<<clock_time<<endl;
    cout<<"2D prod gen + output Total CPU: "<<cpu_time<<endl;

    f_clock_time = difftime(end,start);
    f_cpu_time   = (double)(cpu_end-cpu_start)/clocks_per_second;
   
    cout<<endl<<"Total clock time: "<<f_clock_time<<endl;
    cout<<"Final Total CPU: "<<f_cpu_time<<endl;


    if(pp.mosaic_info_flag )
    {
        if ( pp.debug_flag ) cout<<"Produce and output mosaic info ..."<<endl;
        output_mosaic_info (f_clock_time, f_cpu_time, nradar_has_data,echo_pcnt, timeString, pp);
    }

    cout<<endl<<"Program Done."<<endl<<endl;
    return 0;
}
