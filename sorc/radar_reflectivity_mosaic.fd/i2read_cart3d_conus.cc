// ##########################################################################
// i2readvar_cart3d.cc:  function to read bi format file nto 1D short array.
// ##########################################################################
//      Author: Jian Zhang (CIMMS/NSSL), Wenwu Xia (CIMMS/NSSL)
//      July 20, 2000
//
//      Modification History:
//      Wenwu Xia (CIMMS/NSSL)
//      Febrary, 10, 2001
//      Modified to read gzipped binary file.
//
//      Wenwu Xia (CIMMS/NSSL)
//      April, 26, 2002
//      Modified to return the size of the file.
//
//      Shunxin Wang (CIMMS/NSSL)
//      May, 28, 2005
//      Map conus data into a specific domain
// ##########################################################################

#include <iostream>
#include <fstream>
#include <cstdio>
#include <cstring>
#include <cmath>
#include <cstdlib>
#include <zlib.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

#include "func_prototype.h"
#include "CONFIG_PARS.h"

using namespace std;

int i2readvar_cart3d(char *vfname, int remap_conus_opt,
                     int &map_scale,
                     float &nw_lon,  float &nw_lat,
                     int &xy_scale, int &dxy_scale, int &z_scale,
                     char *varname, char *varunit,
                     int &nradars,  char radarnam[][5],
                     short int *i2var,int &var_scale,
                     int &imissing, CONFIG_PARS &mp,  int &istatus, int &i_bb_mode)
{
//#######################################################################
//     Misc. local variables:
//#######################################################################

      int temp;
      int nx1, ny1, nz1;
      int nx = 0, ny = 0, nz = 0;
      float dx1 = 0.0, dy1 = 0.0, dx = 0.0, dy = 0.0;

      if( mp.conus_ref_opt == 1 )
      {
          nx = mp.conus_nx;
          ny = mp.conus_ny;
          nz = mp.conus_nz;
          dx = mp.conus_dx;
          dy = mp.conus_dy;
      }
      else if ( mp.conus_ref_opt == 0 )
      {
          nx = mp.nx;
          ny = mp.ny;
          nz = mp.nz;
          dx = mp.dx;
          dy = mp.dy; 
      }

//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
//     Beginning of executable code...
//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

      istatus = -1;
      char projection[5];
      int proj=4;
    
      char open_mode[3];

      gzFile   fp_gzip;

      sprintf(open_mode,"%s","rb");

      open_mode[2] = '\0';

      if ( (fp_gzip = gzopen(vfname,open_mode) ) == (gzFile) NULL )
      {
         cout<<"++ERROR open "<<vfname<<endl;
         return 0;
         //exit(0);
      }

      for (int i=0; i<6; i++) {gzread( fp_gzip, &temp,sizeof(int)) ;}

      gzread( fp_gzip,&nx1,sizeof(int)) ;
      gzread( fp_gzip, &ny1,sizeof(int)) ;
      gzread( fp_gzip, &nz1,sizeof(int)) ;

      if ( (nx1!=nx) || (ny1!= ny) )  {
        cout<<endl<<"++ERROR++ inconsistent dimensions."<<endl;
        cout<<"input were:"<<nx<<"  "<<ny<<endl;
        cout<<"read were:"<<nx1<<"  "<<ny1<<endl;
        cout<<"Aborting from i2readvar_cart3d."<<endl<<endl;
        return 0;
        //exit(0);
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
        cout<<"Aborting from i2readvar_cart3d."<<endl<<endl;
        return 0;
        //exit(0);
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


      if ( (fabs(dx1-dx)>1.0e-6) || (fabs(dy1- dy)>1.0e-6) )  {
        cout<<endl<<"++ERROR++ inconsistent dimensions."<<endl;
        cout<<"input were:"<<dx<<"  "<<dy<<endl;
        cout<<"read were:"<<dx1<<"  "<<dy1<<endl;
        cout<<"Aborting from i2readvar_cart3d."<<endl<<endl;
        
        return 0;
        //exit(0);
      }

     // float zhgt[nz1];
      
      for(int k=0; k<nz1; k++){
        gzread(fp_gzip,&temp,sizeof(int));
      //    zhgt[k] = (float)temp;
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
      if( mp.conus_ref_opt == 0 )
         gzread(fp_gzip,i2var,num*sizeof(short int));
      else if( (mp.conus_ref_opt == 1) && (remap_conus_opt == 1) )
         gzread(fp_gzip,i2var1,num*sizeof(short int));
      else if( (mp.conus_ref_opt == 1) && (remap_conus_opt == 0) )
         gzread(fp_gzip,i2var,num*sizeof(short int));
      
      gzclose( fp_gzip );

      //map conus data into specific domain
      if(mp.conus_ref_opt == 1)
      {
        if(remap_conus_opt == 1) 
        {
          int i,j,u,v,k,index,index1;
          int ratiox,ratioy,xmin,ymin;
          int i0,j0,radar_ctrx,radar_ctry;
          ratiox = (int)(mp.conus_dx/mp.dx);
          ratioy = (int)(mp.conus_dy/mp.dy);
          radar_ctrx = (int)((mp.ctrlon - mp.startlon)/mp.conus_dx);
          radar_ctry = (int)((mp.ctrlat - mp.startlat)/mp.conus_dy);
          i0 = mp.nx/(2*ratiox);
          j0 = mp.ny/(2*ratioy);
          xmin = radar_ctrx - i0;
          ymin = radar_ctry - j0;
          nx1 = mp.nx;  nx = mp.conus_nx;
          ny1 = mp.ny;  ny = mp.conus_ny;
          for( k = 0; k < nz1; k++ )
          for( j = 0; j < ny1; j++ )
          for( i = 0; i < nx1; i++ )
          {
              u = i/ratiox + xmin;
              if( u >= nx ) u = nx -1;
              if( u < 0 ) u = 0;
              v = j/ratioy + ymin;
              if( v >= ny ) v = ny -1;
              if( v < 0 ) v = 0;
              index = k*nx1*ny1 + j*nx1 + i;
              index1 = k*nx*ny + v*nx + u;
              i2var[index] = i2var1[index1];
          }
        }
      } //end (mp.conus_ref_opt == 1) if-blk

      //reclaim abve space
      delete [] i2var1;

      z_scale = 1;
      istatus = 0;

      // Find out the size of the binary file
      struct stat buf;
      int ret_code;
      ret_code = stat( vfname, &buf );
      return buf.st_size;
}

