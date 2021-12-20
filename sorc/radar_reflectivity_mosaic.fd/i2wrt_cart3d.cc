// ##########################################################################
//
// i2wrtvar_cart3d.cc:  function to write 1D short array into bi format file.
//
// ##########################################################################
//
//      Author: Jian Zhang (CIMMS/NSSL), Wenwu Xia (CIMMS/NSSL)
//      July 20, 2000
//
//      Modification History:
//
// ##########################################################################


#include <fstream>
#include <iostream>
#include <zlib.h>
#include <cstdio>
#include <cstdlib>
#include <string>
#include <cstring>

#include "CONFIG_PARS.h"
#include "grid_vars.h"


//extern bool WISH_notify( const std::string&, const std::string&, const std::string&);

using namespace std;

template <class basetype>
int get_int(const basetype &x)
{
  int result;
  if((x-(int)x)>=0.500) result=(int)x+1;
  else result=(int)x;
  return(result);
}

//#######################################################################

void i2wrtvar_cart3d(char *dirname, char *filename,
                     int map_scale,
                     float nw_lon, float nw_lat,
                     int xy_scale, int dxy_scale, int z_scale,
                     char *varname,char *varunit,
                     int nradars, char radarnam[][5],
                     short int *i2var, int var_scale, int imissing,
                     CONFIG_PARS &mp, grid_vars &gv,int i_bb_mode, int nz1)
{

//
//#######################################################################
//
//     Misc. local variables:
//
//#######################################################################
//
      char vfname[200];
//
//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
//
//     Beginning of executable code...
//
//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

      char projection[5];

      int second=0,temp;
      if(mp.mapproj==0)      strcpy(projection , "    ");
      else if(mp.mapproj==1) strcpy(projection , "PS  ");
      else if(mp.mapproj==2) strcpy(projection , "LAMB");
      else if(mp.mapproj==3) strcpy(projection , "MERC");
      else if(mp.mapproj==4) strcpy(projection , "LL  ");

      sprintf(vfname,"%s/%s.gz",dirname,filename);
      char open_mode[3];

      gzFile   fp_gzip;

      sprintf(open_mode,"%s","wb");

      open_mode[2] = '\0';

      if ( (fp_gzip = gzopen(vfname,open_mode) ) == (gzFile) NULL )
      {
         cout<<"++ERROR open "<<vfname;
         exit(0);
      }
      
      gzwrite(fp_gzip,&mp.yr,sizeof(int));

      gzwrite(fp_gzip,&mp.mo,sizeof(int));

      gzwrite(fp_gzip,&mp.day,sizeof(int));

      gzwrite(fp_gzip,&mp.hr,sizeof(int));

      gzwrite(fp_gzip,&mp.min,sizeof(int));

      gzwrite(fp_gzip,&second,sizeof(int));

      gzwrite(fp_gzip,&mp.nx,sizeof(int));

      gzwrite(fp_gzip,&mp.ny,sizeof(int));

      gzwrite(fp_gzip,&nz1,sizeof(int));

      gzwrite(fp_gzip,projection,4*sizeof(char));

      gzwrite(fp_gzip,&map_scale,sizeof(int));

      temp=get_int(mp.trulat1*map_scale);
      gzwrite(fp_gzip,&temp,sizeof(int));

      temp=get_int(mp.trulat2*map_scale);
      gzwrite(fp_gzip,&temp,sizeof(int));

      temp=get_int(mp.trulon*map_scale);
      gzwrite(fp_gzip,&temp,sizeof(int));

      temp=get_int(nw_lon*map_scale);
      gzwrite(fp_gzip,&temp,sizeof(int));

      temp=get_int(nw_lat*map_scale);
      gzwrite(fp_gzip,&temp,sizeof(int));

      gzwrite(fp_gzip,&xy_scale,sizeof(int));

      temp=get_int(mp.dx*dxy_scale);
      gzwrite(fp_gzip,&temp,sizeof(int));

      temp=get_int(mp.dy*dxy_scale);
      gzwrite(fp_gzip,&temp,sizeof(int));

      gzwrite(fp_gzip,&dxy_scale,sizeof(int));

      temp=0;

      for(int k=0; k<nz1; k++){
        temp= (int)(gv.zp[k]*z_scale + 0.50);
        gzwrite(fp_gzip,&temp,sizeof(int));
      }

      gzwrite(fp_gzip,&z_scale,sizeof(int));

      gzwrite(fp_gzip,&i_bb_mode,sizeof(int));


      for(int j = 0; j<9; j++){
        gzwrite(fp_gzip,&temp,sizeof(int));
      }     

      gzwrite(fp_gzip,varname,20*sizeof(char));
        
 
      gzwrite(fp_gzip,varunit,6*sizeof(char));

      gzwrite(fp_gzip,&var_scale,sizeof(int));

      gzwrite(fp_gzip,&imissing,sizeof(int));

      gzwrite(fp_gzip,&nradars,sizeof(int));

      for(int i=0;i<nradars;i++)
      {
        gzwrite(fp_gzip,radarnam[i],4*sizeof(char));
      }

      gzwrite(fp_gzip,i2var,mp.nx*mp.ny*nz1*sizeof(short int));
      gzclose( fp_gzip );

}
