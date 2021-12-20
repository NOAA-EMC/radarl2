// ##########################################################################
//
//     radar_sum.cc: Implement fuction of class radar_sum
//
// ##########################################################################
//
//      Author: Jian Zhang (CIMMS/NSSL), Wenwu Xia (CIMMS/NSSL)
//      May 20, 2000
//
//      Modification History:
//
// ##########################################################################



#include "radar_sum.h"

radar_sum::radar_sum(int nx_in, int ny_in, int nz_in){
      nx = nx_in;
      ny = ny_in;
      nz = nz_in;

      gridref=new float **[nx];
      wgtref=new float **[nx];
      for(int i=0;i<nx;i++){
        gridref[i]=new float *[ny];
        wgtref[i]=new float *[ny];
        for(int j=0;j<ny;j++){
          gridref[i][j]=new float[nz];
          wgtref[i][j]=new float[nz];
       }
     }
    }  // end radar_sum

radar_sum::~radar_sum(){
      for(int i=0;i<nx;i++){
        for(int j=0;j<ny;j++) {
          delete [] wgtref[i][j];
          delete [] gridref[i][j];
        }
        delete [] gridref[i];
        delete [] wgtref[i];
      }
       delete [] gridref;
       delete [] wgtref;
    }

