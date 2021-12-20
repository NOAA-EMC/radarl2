// ##########################################################################
//
//  apply_bloom_QC.cc: Using a SRC's VCP number and the RUC sfc 
//                     temperature, apply a bloom QC if needed.  
//
// ##########################################################################
//
//  Author: Carrie Langston(CIMMS/NSSL)
//  May 2006
//
//  Inputs : 3D array of SRC reflectivity data, SRC nx/ny/nz, 
//           SRC's time stamp, RUC Tsfc data, and SRC central 
//           lat and lon, 
//  Outputs: QC'd 3D array of floats storing SRC reflectivity
//
//  Modification history:
//  
//######################################################################

#include <stdlib.h> 
#include <ctime>
#include <string>
#include <iostream>
#include <fstream>
#include <cstdio>

using namespace std;

int get_current_vcp(char top_dir[], char radar_name[], 
                    char vcp_product[], long epoch_time)
{
    //Build time stamp of file name
    char time_stamp[20];
    time_t epoch_time2 = epoch_time;
    strftime(time_stamp, 20, "%Y%m%d-%H%M%S", gmtime(&epoch_time2));
    
    //Build file name
    char xml_file[300];
    sprintf(xml_file, "%s/%s/%s/%s.xml", top_dir, radar_name, 
                                         vcp_product, time_stamp);
                                         
    
    //Open file and read in VCP number
    ifstream xmlFile(xml_file);
    
    if(!xmlFile)
    {
      cout<<"++WARNING: Failed to open "<<xml_file
          <<". Exiting get_current_vcp function!"<<endl;
          
      return -1;
    }
    
    cout<<"Reading "<<xml_file<<endl;
    
    string current_string;
    while( !xmlFile.eof() )
    {
      xmlFile >> current_string;

      if(current_string.find("VCP") != string::npos)
      { 
        //cout<<"--> "<<current_string<<endl;  //should be VCP
        xmlFile >> current_string; 
        //cout<<"--> "<<current_string<<endl;  //should be dimensionless
        xmlFile >> current_string;
        //cout<<"--> "<<current_string<<endl;  //should be item
        xmlFile >> current_string;
        //cout<<"--> "<<current_string<<endl;  //should be VCP value
        break;
      }
    }
    xmlFile.close();
    
    int vcp = -999;
    if(current_string.length() > 0)
    {
      const char QUOTE = 34;
      string vcp_str = current_string.substr(current_string.find(QUOTE));
      //cout<<"vcp_str = "<<vcp_str<<endl;
      vcp_str = vcp_str.substr(1, vcp_str.rfind(QUOTE));
      //cout<<"vcp_str = "<<vcp_str<<endl;
   
      vcp = atoi(vcp_str.c_str());
    
      //cout<<"VCP == "<<vcp<<endl;
    }
    
    return vcp;

}//end get_current_vcp function
