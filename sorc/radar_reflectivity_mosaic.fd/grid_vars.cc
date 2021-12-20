/* ##########################################################################
 *
 *  grid_vars.cc: Implement class function of grid_vars 
 *
 *  Author: Wenwu Xia (CIMMS/NSSL),Jian Zhang (CIMMS/NSSL)
 *  May 10, 2000
 *
 *  Modification History:
 *  3/8/2002  Jian Zhang (CIMMS/NSSL)
 *  Removed the vertical grid streching options.  The exact heights of
 *  each grid levels will be input from the config file.
 *
 * ########################################################################## */

#include <cstring>

#include "grid_vars.h"

using namespace std;

void readtrn_func (const int nx, const int ny,
                  const float dx, const float dy,
                  const char *terrain_file,
                  float ** hterain);


void xytoll_func(float **rlat,float **rlon,float *x,float *y,
     int idim, int jdim,setmapr &m);
 
grid_vars::grid_vars(int nx_in, int ny_in, int nz_in){

      nx = nx_in;
      ny = ny_in;
      nz = nz_in;

      x = new float[nx];
      y = new float[ny];
      zp = new float[nz];

      hterain = new float *[ny];
      for(int j=0; j<ny; j++) hterain[j]=new float[nx];

      gridlat = new float *[nx];
      for(int i=0; i<nx; i++) gridlat[i]=new float[ny];

      gridlon = new float *[nx];
      for(int i=0; i<nx; i++) gridlon[i]=new float[ny];

    }  // end of grid_vars constructor


grid_vars::~grid_vars(){

      delete [] x;
      delete [] y;
      delete [] zp;

      for(int i=0; i<nx; i++) delete [] gridlat[i];
      for(int i=0; i<nx; i++) delete [] gridlon[i];

      delete [] gridlat;
      delete [] gridlon;

      for(int j=0; j<ny; j++) delete [] hterain[j];
      delete [] hterain;
    }


void grid_vars::ini_grd (CONFIG_PARS &pp,setmapr &sm)
{

  int i,j,k;

  float    topomin;
  float    topomax;
      
//#######################################################################
//
//     Define a 2D uniform Cartesian grid.  
//
//#######################################################################

    setgrd s (pp.nx,pp.ny); 
    s.set_grd (sm, pp);

    memcpy(x, s.x, sizeof(float) * pp.nx);
    memcpy(y, s.y, sizeof(float) * pp.ny);

    for(k=0; k<pp.nz; k++) zp[k] = 0;

//#######################################################################
//
//  Initialize terrain...
//
//#######################################################################

    for (j=0;j<pp.ny;j++) for (i=0;i<pp.nx;i++)  hterain[j][i] = 0;

//#######################################################################
//
//  Read in terrain data...
//
//#######################################################################

    if (pp.ternopt==1 )     // Read from terrain data base
    {
      char temp_str[200];
      sprintf(temp_str,"%s/%s",pp.grid_ref_dir,pp.terndta);
      readtrn_func(pp.nx,pp.ny,pp.dx,pp.dy,temp_str,hterain);

      topomin = hterain[0][0];
      topomax = hterain[0][0];
      for(j=0;j<pp.ny;j++)
      {
        for(i=0;i<pp.nx;i++)
        {
          if(hterain[j][i] < -9990) hterain[j][i] = -3.0;
          if(topomin>hterain[j][i]) topomin=hterain[j][i];
          if(topomax<hterain[j][i]) topomax=hterain[j][i];
        }
      }
      cout<<"  topomin/max:: "<<topomin<<"/"<<topomax<<endl;
    }  // end if pp.ternopt==1

//#######################################################################
//
//  Find the lat/lon coordinates of each Cartesian grid points.
//
//#######################################################################
/*      
    float **rlat;
    float **rlon;
    rlat = new float *[pp.nx];
    rlon = new float *[pp.nx];
    cout<<"hello 1"<<endl;
    for(i = 0; i<pp.nx; i++)
    {
      rlat[i] = new float[pp.ny];
      rlon[i] = new float[pp.ny];
    }
    cout<<"hello 2a"<<endl;
    xytoll_func(rlat,rlon,s.x,s.y, pp.nx,  pp.ny,sm);
    cout<<"hello 2b"<<endl;

    for( i=0; i<pp.nx; i++)
      memcpy(gridlat[i], rlat[i], sizeof(float) * ny ); 
    cout<<"hello 3"<<endl;
      
    for( i=0; i<pp.nx; i++)
    {
      cout<<"hello 3.5:: "<< i<<"/"<<rlon[i][0]<<"/"<<rlon[i][pp.ny-1]<<endl;
      memcpy(gridlon[i], rlon[i], sizeof(float) * ny );
    }
    cout<<"hello 4"<<endl;

    for(i=0; i<pp.nx; i++)
    {
      delete [] rlat[i];
      delete [] rlon[i];
    }
    delete [] rlat;
    delete [] rlon; 
*/
    xytoll_func(gridlat,gridlon,s.x,s.y, pp.nx,  pp.ny,sm);
}
