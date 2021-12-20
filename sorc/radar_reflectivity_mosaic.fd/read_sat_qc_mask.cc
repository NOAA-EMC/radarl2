// ##########################################################################
// read_sat_qc_mask.cc:  function to read bi format file into 1D short array.
// ##########################################################################
//      Shunxin Wang (CIMMS/NSSL)
//      May, 28, 2005
//      Read and map satellite qc mask conus data into a specific domain
// ##########################################################################

#include <iostream>
#include <fstream>
#include <cstring>
#include <cstdio>
#include <cmath>
#include <cstdlib>
#include <zlib.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

#include "func_prototype.h"
#include "CONFIG_PARS.h"

using namespace std;

int read_sat_qc_mask(short int *i2var, CONFIG_PARS &mp, int type )
{

//#######################################################################
//     Misc. local variables:
//#######################################################################

      int temp;
      int map_scale;
      float nw_lon, nw_lat;
      int xy_scale, dxy_scale;
      char varname[20];
      char varunit[20];
      int nradars;
      char radarnam[200][5];
      int var_scale;
      int imissing, i_bb_mode;

//#######################################################################
//   make file name and check if it exists
//   type = 1 for IR mask,type = 2 for ECA mask and type = 3 for ctp mask
//#######################################################################
      char file_name[200];
      char temp_timeString[200];
      int time_num = 0;
      time_t epoch_seconds = 0; 

      if( type == 1 )
         time_num = mp.ir_mask_window/mp.ir_mask_freq;
      else if( type == 2 )
         time_num = mp.eca_mask_window/mp.eca_mask_freq;
      else if( type == 3 )
         time_num = mp.ctp_mask_window/mp.ctp_mask_freq;

      for( int i = 0; i < time_num; i++ )
      {
          if( type == 1 )
          {
              epoch_seconds = ((mp.epoch_seconds - i*mp.ir_mask_freq)/mp.ir_mask_freq)*mp.ir_mask_freq;
              strftime (temp_timeString, 20, "%Y%m%d.%H%M", gmtime(&epoch_seconds));
              sprintf(file_name,"%s%s%s%s",mp.ir_mask_dir,"sat_ir_qcmask.",
                      temp_timeString,".gz");
          }
          else if( type == 2 )
          {
              epoch_seconds = ((mp.epoch_seconds - i*mp.eca_mask_freq)/mp.eca_mask_freq)*mp.eca_mask_freq;
              strftime (temp_timeString, 20, "%Y%m%d.%H%M", gmtime(&epoch_seconds));
              sprintf(file_name,"%s%s%s%s",mp.eca_mask_dir,"sat_eca_qcmask.",
                      temp_timeString,".gz");
          }
          else if( type == 3 )
          {
              epoch_seconds = ((mp.epoch_seconds - i*mp.ctp_mask_freq)/mp.ctp_mask_freq)*mp.ctp_mask_freq;
              strftime (temp_timeString, 20, "%Y%m%d.%H%M", gmtime(&epoch_seconds));
              sprintf(file_name,"%s%s%s%s",mp.ctp_mask_dir,"sat_ctp_qcmask.",
                      temp_timeString,".gz");
          }
          else
          {
              cout<<"Wrong mask data type input. No mask data available."<<endl;
              return 0;
          }   

          //check the file exists or not
          //cout<<"CHECKING FOR: "<<file_name<<endl;
          ifstream inF0(file_name);
          if(inF0)
          {
             inF0.close();
             break;
          }
          else if( i == time_num -1 )
          {
              if(mp.debug_flag == 1 && type == 1 )
                 cout<<"No IR QC mask file exists within time window."<<endl;
              else if(mp.debug_flag == 1 && type == 2 )
                 cout<<"No ECA QC mask file exists within time window."<<endl;
              else if(mp.debug_flag == 1 && type == 3 )
                 cout<<"No CTP QC mask file exists within time window."<<endl;
              return 0; 
          }
      }

//#######################################################################
//   set parameters 
//#######################################################################

      int nx1, ny1, nz1;
      int nx = 0, ny = 0, nz = 0;
      float dx1 = 0.0, dy1 = 0.0, dx = 0.0, dy = 0.0;
      float startlat,startlon;

      nx = mp.sat_conus_nx;
      ny = mp.sat_conus_ny;
      nz = mp.sat_conus_nz;
      dx = mp.sat_conus_dx;
      dy = mp.sat_conus_dy;

//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
//     Beginning of executable code...
//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

      char projection[5];
      int proj=4;
    
      char open_mode[3];

      gzFile   fp_gzip;

      sprintf(open_mode,"%s","rb");

      open_mode[2] = '\0';

      if ( (fp_gzip = gzopen(file_name,open_mode) ) == (gzFile) NULL )
      {
         cout<<"++ERROR open "<<file_name<<endl;
         exit(0);
      }
      else cout<<"Reading "<<file_name<<" ."<<endl;

      for (int i=0; i<6; i++) {gzread( fp_gzip, &temp,sizeof(int)) ;}

      gzread( fp_gzip,&nx1,sizeof(int)) ;
      gzread( fp_gzip, &ny1,sizeof(int)) ;
      gzread( fp_gzip, &nz1,sizeof(int)) ;

      if ( (nx1!=nx) || (ny1!= ny) )  {
        cout<<endl<<"++ERROR++ inconsistent dimensions."<<endl;
        cout<<"input were:"<<nx<<"  "<<ny<<endl;
        cout<<"read were:"<<nx1<<"  "<<ny1<<endl;
        cout<<"Aborting from read_sat_qc_mask."<<endl<<endl;
        exit(0);
      }

      gzread(fp_gzip,&projection,4*sizeof(char));

      projection[4] = '\0';
      if (strncmp(projection, "    ", 4)==0 ) proj=0; 
      if (strncmp(projection, "PS  ", 4)==0 ) proj=1;
      if (strncmp(projection, "LAMB", 4)==0 ) proj=2; 
      if (strncmp(projection, "MERC", 4)==0 ) proj=3; 
      if (strncmp(projection, "LL  ", 4)==0 ) proj=4; 

      if(proj!=mp.mapproj) {
        cout<<endl<<"++ERROR++ inconsistent map projections."<<endl;
        cout<<"input were:"<<mp.mapproj<<endl;
        cout<<"read were:"<<proj<<endl;
        cout<<"Aborting from read_sat_qc_mask."<<endl<<endl;
        exit(0);
      }

      gzread(fp_gzip,&map_scale,sizeof(int));
      for(int i = 0; i < 3; i++) {gzread(fp_gzip,&temp,sizeof(int));}
      int nw_lon1, nw_lat1;
      gzread(fp_gzip,&nw_lon1,sizeof(int));
      nw_lon = (float)nw_lon1;
      gzread(fp_gzip,&nw_lat1,sizeof(int));
      nw_lat = (float)nw_lat1;

      gzread(fp_gzip,&xy_scale,sizeof(int));

      nw_lon = nw_lon/float(map_scale);
      nw_lat = nw_lat/float(map_scale);
   
      gzread(fp_gzip,&temp,sizeof(int));
      dx1 = (float)(temp);
      gzread(fp_gzip,&temp,sizeof(int));
      dy1 = (float)(temp);

      gzread(fp_gzip,&dxy_scale,sizeof(int));
      dx1 = dx1/float(dxy_scale);
      dy1 = dy1/float(dxy_scale);

      startlon = nw_lon; 
      startlat = nw_lat - dy1*(float)(ny1 - 1);

      if ( (fabs(dx1-dx)>1.0e-6) || (fabs(dy1- dy)>1.0e-6) )  {
        cout<<endl<<"++ERROR++ inconsistent dimensions."<<endl;
        cout<<"input were:"<<dx<<"  "<<dy<<endl;
        cout<<"read were:"<<dx1<<"  "<<dy1<<endl;
        cout<<"Aborting from read_sat_qc_mask."<<endl<<endl;
        exit(0);
      }

//      float zhgt[nz1];
      
      for(int k=0; k<nz1; k++){
        gzread(fp_gzip,&temp,sizeof(int));
//          zhgt[k] = (float)temp;
      }

      gzread(fp_gzip,&temp,sizeof(int)); //z_scale

      gzread(fp_gzip,&i_bb_mode,sizeof(int));

      for(int j = 0; j < 9; j++){
        gzread(fp_gzip,&temp,sizeof(int)); //for future use
      }
      gzread(fp_gzip,varname,20*sizeof(char));
      varname[19]='\0';

      gzread(fp_gzip,varunit,6*sizeof(char));
      varunit[5]='\0';

      gzread(fp_gzip,&var_scale,sizeof(int));

      gzread(fp_gzip,&imissing,sizeof(int));

      gzread(fp_gzip,&nradars,sizeof(int));
      for(int i=0;i<nradars;i++){
        gzread(fp_gzip,radarnam[i],4*sizeof(char));
        radarnam[i][4]='\0';
      }

      int num = nx1*ny1*nz1;
      short int *i2var1;
      i2var1 = new short int[num];
      gzread(fp_gzip,i2var1,num*sizeof(short int));
        
      gzclose( fp_gzip );

      //map conus data into specific domain
      int i,j,u,v,index,index1;
      int ratiox,ratioy,xmin,ymin;
      int i0,j0,radar_ctrx,radar_ctry;
      ratiox = (int)(mp.sat_conus_dx/mp.dx);
      ratioy = (int)(mp.sat_conus_dy/mp.dy);
      radar_ctrx = (int)((mp.ctrlon - startlon)/mp.sat_conus_dx);
      radar_ctry = (int)((mp.ctrlat - startlat)/mp.sat_conus_dy);
      i0 = mp.nx/(2*ratiox);
      j0 = mp.ny/(2*ratioy);
      xmin = radar_ctrx - i0;
      ymin = radar_ctry - j0;
      nx = nx1;   nx1 = mp.nx;  
      ny = ny1;   ny1 = mp.ny; 
      for( j = 0; j < ny1; j++ )
      for( i = 0; i < nx1; i++ )
      {
          u = i/ratiox + xmin;
          if( u >= nx )  u = nx -1;
          if( u < 0 )  u = 0;
          v = j/ratioy + ymin;
          if( v >= ny )   v = ny -1;
          if( v < 0 )   v = 0;
          index = j*nx1 + i;
          index1 = v*nx + u;
          i2var[index] = i2var1[index1];
      }

      //reclaim abve space
      delete [] i2var1;

      // Find out the size of the binary file
      struct stat buf;
      int ret_code;
      ret_code = stat( file_name, &buf );
      return buf.st_size;
}

