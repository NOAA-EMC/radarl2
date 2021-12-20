//#######################################################################
//  PURPOSE:
//  Read in the real terrain data that has been remapped onto
//  the analysis grid.
/* ##########################################################################
 *  Author: Wenwu Xia (CIMMS/NSSL),Jian Zhang (CIMMS/NSSL)
 *  May 10, 2000
 *
 *  Modification History:
 *  10/22/2001  Jian Zhang
 *  Changed to a simple function.
 *
 * ########################################################################*/

//#######################################################################
//
//  INPUT :
//
//    nx       Number of grid points in the x-direction (east/west)
//    ny       Number of grid points in the y-direction (north/south)
//
//    dx       Grid interval in x-direction
//    dy       Grid interval in y-direction
//
//    terrain_file     Terrain data file name including complete path
//
//  OUTPUT:
//
//    hterain  Terrain height (m)
//
//#######################################################################

#ifndef READTRN_H
#define READTRN_H

#include <fstream>
#include <iostream>
#include <cstdio>
#include <cstdlib>

using namespace std;

void readtrn_func (const int nx, const int ny,
                  const float dx, const float dy,
                  const char *terrain_file,
                  float ** hterain)
{

  int nxin, nyin, idummy;
  float  dxin, dyin, rdummy;

//#######################################################################
//
//     Read in the terrain data.
//
//#######################################################################
  
    std::ifstream in(terrain_file);

    if(!in){ 
      std::cout<< "Error occurred when opening terrain data file "
          <<terrain_file<<" Job stopped in READTRN."<<std::endl;
      exit(0); 
    }

    in.read(reinterpret_cast<char *>(&idummy),sizeof(int));
    in.read(reinterpret_cast<char *>(&nxin),sizeof(int));
    in.read(reinterpret_cast<char *>(&nyin),sizeof(int));
  
 
    if((nx!=nxin)||(ny!=nyin)) {
      std::cout<<" Array size in the terrain data does not match that of the"
        << " model grid. Dimensions in the data were nx="<<nxin
        <<", ny="<<nyin
        <<" the model grid size were nx="<<nx<<" ny= "<<ny<<std::endl;
      cout<< " Job stopped in subroutine READTRN."<<std::endl;
      exit(1);
    }
    for(int i=1;i<=24;i++)in.read(reinterpret_cast<char *>(&idummy),sizeof(int));
    in.read(reinterpret_cast<char *>(&dxin),sizeof(float));
    in.read(reinterpret_cast<char *>(&dyin),sizeof(float));
    for(int i=1;i<=20;i++)in.read(reinterpret_cast<char *>(&idummy),sizeof(int));

    if( abs(int(100*((dx-dxin)/dx)))>1
       ||abs(int(100*((dy-dyin)/dy)))>1)
    {
      std::cout<< "Grid intervals in the terrain data do not match those "
        <<"in the model."<<"In the data  dx="<<dxin<<", dy="<<dyin
        <<"In the model dx="<<dx<<" dy= "<< dy<<std::endl;
      std::cout<< " Job stopped in subroutine READTRN."<<std::endl;
      exit(1);
    }

    for(int jj=0;jj<ny;jj++)
    for(int ii=0;ii<nx;ii++){
      in.read(reinterpret_cast<char *>(&rdummy),sizeof(float));
      hterain[jj][ii]= rdummy;
/*      
      in.read(reinterpret_cast<char *>(&rdummy),sizeof(short));
      hterain[jj][ii]= (float)rdummy;
*/

    }

};

#endif
