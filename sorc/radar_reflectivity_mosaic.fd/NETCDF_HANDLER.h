#ifndef NETCDF_HANDLER_H
#define NETCDF_HANDLER_H

// ########################################################################
//
//  Class NETCDF_HANDLER.h: header for Reading netcdf file and writting
//                     netcdf file etc
//
// ########################################################################
//
//  Author: Shunxin Wang (CIMMS/NSSL)
//  Nov. 30, 2003
//
//  Modification History:
//##########################################################################
#include <string>
#include <iostream>
#include <cstdlib>
#include <cmath>
#include <cstdio>
#include <string.h>
//#include <netcdf.h>
 
#include "CONFIG_PARS.h"
#include "qpesums_utility.h"

#define NC_NOERR 0
#define NC_CLOBBER 0
#define NC_SHORT 3
#define NC_CHAR 2
#define NC_INT 4
#define NC_GLOBAL -1
#define NC_FLOAT 5
 

using namespace std;

class NETCDF_HANDLER
{

  private:
     char *dir_name;
     char *file_name;
     char *time_str;
     int num_var;
     int index;   
     int last;   
     char **var_name;
     char **var_unit;
     char **netcdf_filen;
     char depictorName[30]; 
     short *var_scale;
     short *imissing;
     int *nz;
     short *mint;
     short *maxt;
     short **i2tem; 
     void check_err(const int stat, const int line, const char *file);
  
  public:
     NETCDF_HANDLER(int numvar,char *dipictor_name,
                    char *dirname, char *filename0,char *timestr );
     virtual ~NETCDF_HANDLER();
     void netcdf_reader();
     void release_space();
     void ith_netcdf_wrt(char *varname,char *varunit, char *filename,
                    short *i2var,  short varscale, short missing,CONFIG_PARS &mp,int nz1,
                    short mint0,short maxt0,int last0);
     void multiVar_netcdf_wrt(CONFIG_PARS &mp);
};

#endif
