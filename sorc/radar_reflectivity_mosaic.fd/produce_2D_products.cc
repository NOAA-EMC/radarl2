// ########################################################################
//  produce_2D_products:
//      A driver for deriving 2D products such as ETP, SHI, POSH, MEHS,
//      VIL,VILD,HSR and HSRH from the 3D reflectivity mosaic grid.
// ########################################################################
//
//  Author: Shunxin Wang (CIMMS/NSSL)
//  Nov. 30, 2003
//
//  Modification History:i
//  Author: Shunxin Wang (CIMMS/NSSL)
//
//  05/27/04  Shunxin Wang
//  rewrite it by using NETCDF_HANDLER class
//
//  06/02/04  Jian Zhang
//  Added a new module for composite reflectivity and the associated height
//
//  04/15/05  Jian Zhang
//  Cleaned up the scale and missing values for the each product output
//
//##########################################################################

#include <iostream>
#include <cmath>
#include <cstdio>
#include <string>
#include <stdlib.h>
 
#include "CONFIG_PARS.h"
#include "mosaic_adapt_pars.h"
#include "func_prototype.h"
#include "NETCDF_HANDLER.h"

//includes for WDSS-II file I/O (3/8/2006 CL)
//#include "ioW2.h"
//#include "W2MultiVarField2D.h"

using namespace std;

void produce_2D_products (char nc_timeString[20], CONFIG_PARS &pp, nids_pars &np,
                          float*** ref, float ***kh_tlvls, int itime0, 
                          int nradar_has_data, grid_vars &gv,
                          char* timeString, 
                          string config_file_name, char radarnam[][5])
{

//###########################################################################
//
//  Initialize the NetCDF output
//
//###########################################################################

//     W2MultiVarField2D w2MVF;

//     NETCDF_HANDLER mosaic2d (10, "mosaic2d",pp.mosaic2d_netcdf_dir, 
//                              nc_timeString,timeString );
   
   
//###########################################################################
//
//  Compute and output each 2D product 
//
//###########################################################################


   //Start timing code
   double clocks_per_second = (double) CLOCKS_PER_SEC;
   time_t start1,end1;

   clock_t cpu_start1,cpu_end1;
   start1 = time(NULL);
   cpu_start1 = clock();   



   short **i2tem;
   i2tem = new short*[3];
   for (int i = 0; i < 3; i++)
      i2tem[i] = new short[pp.nx*pp.ny];
      
   float fractional_time = 0.0;
   float domain_nw_lat, domain_nw_lon;      
   domain_nw_lat = pp.ctrlat + (float)(pp.ny -1)*pp.dy/2;
   domain_nw_lon = pp.ctrlon - (float)(pp.nx -1)*pp.dx/2;

   // compute and output composite reflectivity and height products
   produce_CREF_CREFH (pp, np, config_file_name, ref, i2tem, timeString,
                       gv, nradar_has_data, radarnam);



   //output CREF product
//   if(pp.mosaic2d_wdssii_netcdf_flag)
//     w2MVF.add_LatLonField("cref", "dBZ", "cref", pp.epoch_seconds,
//                           fractional_time, ref_miss, ref_scale,
//                           pp.nx, pp.ny, pp.dx, pp.dy, 
//                           domain_nw_lat, domain_nw_lon, i2tem[0], false, true);
   
//   if (pp.mosaic2d_netcdf_flag==1) 
//      mosaic2d.ith_netcdf_wrt("CREF","dBZ","cref",
//                 i2tem[0], ref_scale, ref_miss*ref_scale,
//                 pp, 1, get_int(min_ref_rang*ref_scale),get_int( max_ref_rang*ref_scale),0);

   //output CREFH product
//   if(pp.mosaic2d_wdssii_netcdf_flag)
//     w2MVF.add_LatLonField("hgt_cref", "kmMSL", "hgt_cref", pp.epoch_seconds,
//                           fractional_time, crefh_miss, crefh_scale,
//                           pp.nx, pp.ny, pp.dx, pp.dy, 
//                           domain_nw_lat, domain_nw_lon, i2tem[1], false, true);


//   if (pp.mosaic2d_netcdf_flag==1) 
//      mosaic2d.ith_netcdf_wrt("CREFH","kmMSL","hgt_cref",
//                 i2tem[1], crefh_scale, crefh_miss*crefh_scale,
//                 pp, 1, get_int(min_crefh_rang*crefh_scale/KM_TO_M),get_int(max_crefh_rang*crefh_scale/KM_TO_M),0);


   //output ETP product
   produce_ETP (pp,ref,itime0,nradar_has_data,gv,timeString,
                 radarnam, i2tem[0]);


//   if(pp.mosaic2d_wdssii_netcdf_flag)
///     w2MVF.add_LatLonField("etp18", "kmMSL", "etp18", pp.epoch_seconds,
//                           fractional_time, etp_miss, etp_scale, 
//                           pp.nx, pp.ny, pp.dx, pp.dy, 
//                           domain_nw_lat, domain_nw_lon, i2tem[0], false, true);

//   if(pp.mosaic2d_netcdf_flag==1) 
//      mosaic2d.ith_netcdf_wrt("ETP18","kmMSL","etp18",
//                 i2tem[0], etp_scale, etp_miss*etp_scale,
//                 pp, 1, get_int(min_etp_rang*etp_scale),get_int(max_etp_rang*etp_scale),0);

   //output SHI product
   produce_HDA (pp, ref, kh_tlvls, itime0, nradar_has_data,gv, timeString,
               config_file_name, radarnam,i2tem);

//   if(pp.mosaic2d_wdssii_netcdf_flag)
//     w2MVF.add_LatLonField("shi", "  ", "shi", pp.epoch_seconds,
//                           fractional_time, shi_miss, shi_scale,
//                           pp.nx, pp.ny, pp.dx, pp.dy, 
//                           domain_nw_lat, domain_nw_lon, i2tem[0], false, true);

//   if(pp.mosaic2d_netcdf_flag==1) 
//      mosaic2d.ith_netcdf_wrt("SHI","  ","shi",
//                 i2tem[0], shi_scale, shi_miss*shi_scale,
//                 pp, 1, get_int(min_shi_rang*shi_scale),get_int(max_shi_rang*shi_scale),0);
   //output POSH product
//   if(pp.mosaic2d_wdssii_netcdf_flag)
//     w2MVF.add_LatLonField("posh", "% ", "posh", pp.epoch_seconds,
//                           fractional_time, posh_miss, posh_scale,
//                           pp.nx, pp.ny, pp.dx, pp.dy, 
//                           domain_nw_lat, domain_nw_lon, i2tem[1], false, true);

//   if(pp.mosaic2d_netcdf_flag==1) 
//      mosaic2d.ith_netcdf_wrt("POSH","% ","posh",
//                 i2tem[1], posh_scale, posh_miss*shi_scale,
//                 pp, 1, get_int(min_posh_rang*posh_scale),get_int(max_posh_rang*posh_scale),0);
 
   //output MEHS product
//   if(pp.mosaic2d_wdssii_netcdf_flag)
//     w2MVF.add_LatLonField("mehs", "mm", "mehs", pp.epoch_seconds,
//                           fractional_time, mehs_miss, mehs_scale,
//                           pp.nx, pp.ny, pp.dx, pp.dy, 
//                           domain_nw_lat, domain_nw_lon, i2tem[2], false, true);

//   if(pp.mosaic2d_netcdf_flag==1) 
//       mosaic2d.ith_netcdf_wrt("MEHS","mm","mehs",
//                 i2tem[2], mehs_scale, mehs_miss*mehs_scale,
//                 pp, 1, get_int(min_mehs_rang*mehs_scale),get_int(max_mehs_rang*mehs_scale),0);

   //output HSR product
   produce_HSR_and_HSRH (pp,ref,itime0,nradar_has_data,gv,timeString,
                  radarnam,i2tem);


//   if(pp.mosaic2d_wdssii_netcdf_flag)
//     w2MVF.add_LatLonField("hsr", "dBZ", "hsr", pp.epoch_seconds,
//                           fractional_time, hsr_miss, hsr_scale, 
//                           pp.nx, pp.ny, pp.dx, pp.dy, 
//                           domain_nw_lat, domain_nw_lon, i2tem[0], false, true);

//   if(pp.mosaic2d_netcdf_flag==1) 
//       mosaic2d.ith_netcdf_wrt("HSR","dBZ","hsr",
//                 i2tem[0], hsr_scale, hsr_miss*hsr_scale,
//                 pp, 1, get_int(min_hsr_rang*hsr_scale),get_int(max_hsr_rang*hsr_scale),0);
//
   //output HSRH product
//   if(pp.mosaic2d_wdssii_netcdf_flag)
//     w2MVF.add_LatLonField("hsrh", "kmAGL", "hsrh", pp.epoch_seconds,
//                           fractional_time, hsrh_miss, hsrh_scale, 
//                           pp.nx, pp.ny, pp.dx, pp.dy, 
//                           domain_nw_lat, domain_nw_lon, i2tem[1], false, true);

//   if(pp.mosaic2d_netcdf_flag==1) 
//       mosaic2d.ith_netcdf_wrt ("HSRH","kmAGL","hsrh",
//                 i2tem[1], hsrh_scale, hsrh_miss*hsrh_scale,
//                 pp, 1, get_int(min_hsrh_rang*hsrh_scale),get_int(max_hsrh_rang*hsrh_scale),0);
//


   //output lcref and lcrefh products
   produce_LCREF_and_LCREFH(pp,ref,itime0,nradar_has_data,gv,timeString,
                  radarnam,i2tem);

   //output LCREF product
//   if(pp.mosaic2d_wdssii_netcdf_flag)
//     w2MVF.add_LatLonField("lcref", "dBZ", "lcref", pp.epoch_seconds,
//                           fractional_time, ref_miss, ref_scale,
//                           pp.nx, pp.ny, pp.dx, pp.dy, 
//                           domain_nw_lat, domain_nw_lon, i2tem[0], false, true);
   //output LCREFH product
//   if(pp.mosaic2d_wdssii_netcdf_flag)
//     w2MVF.add_LatLonField("hgt_lcref", "kmMSL", "hgt_lcref", pp.epoch_seconds,
//                           fractional_time, crefh_miss, crefh_scale,
//                           pp.nx, pp.ny, pp.dx, pp.dy, 
//                           domain_nw_lat, domain_nw_lon, i2tem[1], false, true);
   //outputg VIL product
   produce_VIL_and_VILD (pp,ref,itime0,nradar_has_data,gv,timeString,
                   radarnam,i2tem);

//   if(pp.mosaic2d_wdssii_netcdf_flag)
//     w2MVF.add_LatLonField("vil", "kg/m2", "vil", pp.epoch_seconds,
//                           fractional_time, vil_miss, vil_scale, 
//                           pp.nx, pp.ny, pp.dx, pp.dy, 
//                           domain_nw_lat, domain_nw_lon, i2tem[0], false, true);
//   if (pp.mosaic2d_netcdf_flag==1) 
//       mosaic2d.ith_netcdf_wrt("VIL","kg/m2","vil",
//                 i2tem[0], vil_scale, vil_miss*vil_scale,
//                 pp, 1, get_int(min_vil_rang*vil_scale),get_int(max_vil_rang*vil_scale),0);
//
   //output VILD product
//   if(pp.mosaic2d_wdssii_netcdf_flag)
//     w2MVF.add_LatLonField("vilD", "g/m3", "vilD", pp.epoch_seconds,
//                           fractional_time, vilD_miss, vilD_scale,
//                           pp.nx, pp.ny, pp.dx, pp.dy, 
//                           domain_nw_lat, domain_nw_lon, i2tem[1], false, true);

//   if (pp.mosaic2d_netcdf_flag==1) 
//      mosaic2d.ith_netcdf_wrt ("VILD","g/m3","vilD",
//                 i2tem[1], vilD_scale, vilD_miss*vilD_scale,
//                 pp, 1, get_int(min_vilD_rang*vilD_scale),get_int(max_vilD_rang*vilD_scale),1);

   //compute precip flag
   produce_PCPFLAG(pp, ref, kh_tlvls, itime0, timeString, gv, nradar_has_data,
                   radarnam, config_file_name, i2tem);

   
   //output pcp_flag
//   if(pp.mosaic2d_wdssii_netcdf_flag)
//     w2MVF.add_LatLonField("pcp_flag", "flag", "pcp_flag", pp.epoch_seconds,
//                           fractional_time, pcpflag_miss, pcpflag_scale, 
//                           pp.nx, pp.ny, pp.dx, pp.dy, 
//                           domain_nw_lat, domain_nw_lon, i2tem[0], false, true);


   //Write WDSS-II MV NetCDF file
   if(pp.mosaic2d_wdssii_netcdf_flag)
   {
     if(pp.debug_flag)
     {
       cout<<"Attributes of multi-var file:"<<endl;
//       w2MVF.dumpHeader();
       cout<<endl;
     }
     
     if(pp.debug_flag) cout<<"**Writing WDSS-II MV NetCDF**"<<endl;
     
     //Write out 3D WDSS-II file.
     char wdssii_ncdf_filename[500];
     char wdssii_timestamp[20];
     
     time_t seconds_epoch_temp = pp.epoch_seconds; 
     strftime (wdssii_timestamp, 20, "%Y%m%d-%H%M", gmtime(&seconds_epoch_temp));
     sprintf(wdssii_ncdf_filename, "%s/%s.netcdf", pp.mosaic2d_netcdf_dir, wdssii_timestamp);
      
     if(pp.debug_flag) cout<<"  filename = "<<wdssii_ncdf_filename<<endl;
//     ioW2::write_multivar_wdssii_ncdf(wdssii_ncdf_filename, w2MVF, true, -1, "short");
     
     if( pp.mosaic2d_netcdf_remote_output_opt )
     {
     cout<<"remote option on"<<endl;
       char vfname[200];
       char vfname_remote[200];
       char system_command[200];
       sprintf(vfname,"%s%s",wdssii_ncdf_filename,".gz");
       sprintf(vfname_remote,"%s/%s%s",pp.mosaic2d_netcdf_dir_remote,wdssii_timestamp,".netcdf.gz");
       strcpy(system_command,"cp -f ");
       strcat(system_command,vfname);
       strcat(system_command," ");
       strcat(system_command,vfname_remote);
       int sys_ret = system(system_command);
       if( sys_ret != 0 )
       {
           cout<<"++WARNING: Copy operation failed ("<<sys_ret<<")"<<endl;
           //exit(0);
       }
     }

     //transfer mosaic data to nmqserver by using LDM
     if (pp.ldm_netcdf_opt == 1 )
     {
       char ldm_command[200];
       char fileName[100];
       sprintf(fileName,"%s.gz",wdssii_ncdf_filename);
       sprintf (ldm_command, "%s -v -l %s -q %s -f EXP %s", pp.pqinsert_file,
                   pp.ldmd_log_file,pp.ldm_pq_file,fileName);
       if(pp.debug_flag == 1)
         cout<<"Implement "<<ldm_command<<endl;
       system(ldm_command);
     }

   }//end mosaic2d_wdssii_netcdf_flag if-blk
   

   //For AWIPS MV NetCDF file
   if(pp.mosaic2d_netcdf_flag)
   {
    
     if( pp.mosaic2d_netcdf_remote_output_opt )
     {
       char vfname[200];
       char vfname_remote[200];
       char system_command[200];
       sprintf(vfname,"%s/%s%s",pp.mosaic2d_netcdf_dir,nc_timeString,".gz");
       sprintf(vfname_remote,"%s/%s%s",pp.mosaic2d_netcdf_dir_remote,nc_timeString,".gz");
       strcpy(system_command,"cp -f ");
       strcat(system_command,vfname);
       strcat(system_command," ");
       strcat(system_command,vfname_remote);
       int sys_ret = system(system_command);
       if( sys_ret != 0 )
       {
           cout<<"++WARNING: Copy operation failed ("<<sys_ret<<")"<<endl;
           //exit(0);
       }
     }

     //transfer mosaic data to nmqserver by using LDM
     if (pp.ldm_netcdf_opt == 1 )
     {
       char ldm_command[200];
       char fileName[100];
       sprintf(fileName,"%s/%s.gz",pp.mosaic2d_netcdf_dir,nc_timeString);
       sprintf (ldm_command, "%s -v -l %s -q %s -f EXP %s", pp.pqinsert_file,
                    pp.ldmd_log_file,pp.ldm_pq_file,fileName);
       if(pp.debug_flag == 1)
           cout<<"Implement "<<ldm_command<<endl;
       system(ldm_command);
     }
     
   }//end mosaic2d_netcdf_flag if-blk


   //release space
   for (int i = 0; i < 3; i++) delete [] i2tem[i];
   delete [] i2tem;


   //End timing code
   end1 = time(NULL);
   cpu_end1 = clock();
   double clock_time,cpu_time; 

   clock_time = difftime(end1,start1);
   cpu_time   = (double)(cpu_end1-cpu_start1)/clocks_per_second;
   
   cout<<endl<<"produce_2D_products clock time: "<<clock_time<<endl;
   cout<<"produce_2D_products Total CPU: "<<cpu_time<<endl;


   return;
}


