#ifndef RADAR_SUM_H
#define RADAR_SUM_H


//
//     ##################################################################
//     ##################################################################
//     ######                                                      ######
//     ######              radar_sum.h                             ######
//     ######                                                      ######
//     ##################################################################
//     ##################################################################
//
//#######################################################################
//
//     PURPOSE:
//
//     This file defines some common arrays on the Cartesian grid.
//
//     Author:  Jian Zhang
//     09/01/1998
//
//     Modification History:
//     12/01/1999  Leilei Wang
//     Convert from Fortran to C++
//
//     06/10/2000  Jian Zhang
//     Major modifications for memory allocation.  Added destructors.
//
//#######################################################################
//
//#######################################################################
//
//     Summation arrays 
//
//#######################################################################
//
class radar_sum
{ 
  int nx,ny,nz;

  public:

    float ***gridref; //sum of weighted refl. at each grid point
    float ***wgtref;  //sum of weights at each grid point

    radar_sum():gridref(0),wgtref(0){};
   // radar_sum(const radar_sum& src);
   // radar_sum& operator = ( const radar_sum& src);


    radar_sum (int nx_in, int ny_in, int nz_in);
    ~radar_sum ();
};  // end class radar_sum

#endif
