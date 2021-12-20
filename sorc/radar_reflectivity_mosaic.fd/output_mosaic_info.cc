//#######################################################################
//  Function: output_mosaic_info()
//     output information for a real-time system monitoring/management tool.
//     The info include the # radars, echo coverage (-30dbz and above), CPU and
//     Clock time used by the each Mosaic run.
// ##########################################################################
//
//   Author: Shunxin Wang (CIMMS/NSSL)
//   April 30, 2004
//
//   Modification History
//   6/2/2004  Jian Zhang
//   Removed calculation of echo coverage.  Instead the coverage is passed
//   in from the main program as an input.  Saved space and computations.
//
// ##########################################################################

#include <iostream>
#include <fstream>
#include <iomanip>
#include <string.h>

#include "CONFIG_PARS.h"

using namespace std;

void output_mosaic_info(double clock_time,double cpu_time,
                  int nradars,float echo_coverage, char* timeStamp,CONFIG_PARS &pp)
{
   char vfname[200];

   //make output file name and open it for reading
   strcpy(vfname,pp.mosaic_info_dir);
   strcat(vfname,"/");
   strncat(vfname,timeStamp,6);
   ifstream inFile(vfname,ios::in);

   if(!inFile)
   {
       ofstream outFile(vfname,ios::out);
       if(!outFile)
       {
           cout<<"+++ERROR+++ Can not output "<<vfname<<endl;
           cout<<"Exiting from output_mosaic_info()"<<endl;
           exit(0);
       }
       outFile<<setw(15)<<setiosflags(ios::left)<<"timeStamp"
              <<setw(15)<<setiosflags(ios::left)<<"cpu time(s)"
              <<setw(15)<<setiosflags(ios::left)<<"clock time(s)"
              <<setw(17)<<setiosflags(ios::left)<<"number of radars"
              <<setw(17)<<setiosflags(ios::left)<<"echo coverage(%)"<<endl;
       outFile<<setw(15)<<setiosflags(ios::left)<<timeStamp
              <<setw(15)<<setiosflags(ios::left)<<setprecision(4)<<cpu_time
              <<setw(15)<<setiosflags(ios::left)<<setprecision(4)<<clock_time
              <<setw(17)<<setiosflags(ios::left)<<nradars
              <<setw(17)<<setiosflags(ios::left)<<setprecision(4)<<echo_coverage<<endl;
       outFile.close();
       return;
   }
   inFile.close();

   ofstream outFile(vfname,ios::app);
   if(!outFile)
   {
       cout<<"+++ERROR+++ Can not output "<<vfname<<endl;
       cout<<"Exiting from output_mosaic_info()"<<endl;
       exit(0);
   }
   outFile<<setw(15)<<setiosflags(ios::left)<<timeStamp
          <<setw(15)<<setiosflags(ios::left)<<setprecision(4)<<cpu_time
          <<setw(15)<<setiosflags(ios::left)<<setprecision(4)<<clock_time
          <<setw(17)<<setiosflags(ios::left)<<nradars
          <<setw(17)<<setiosflags(ios::left)<<setprecision(4)<<echo_coverage<<endl;
   outFile.close();

   return;
}
