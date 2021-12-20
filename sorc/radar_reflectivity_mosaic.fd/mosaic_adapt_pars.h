/* ##########################################################################
 *
 * mosaic_adapt_pars.h:  Declaring adaptable parameters for 3D
 *                       MOSAIC algorithm.
 *                       Some constant parameters are declared here, too.
 *
 * Author: Jian Zhang (CIMMS/NSSL)
 *   May 10, 1999.
 *
 * Modification History:
 *   October 22, 2001  Jian Zhang (CIMMS/NSSL)
 *   Renamed from "dataio.h" to "mosaic_adapt_pars.h" because the
 *   parameters defined here are mostly adaptable physical paramemters.
 *
 *   4/1/2004 Jian Zhang
 *   Fixed a couple of incorrect field range values for NetCDF.
 *
 * ########################################################################*/


#ifndef MOSAIC_ADAPT_PARS_H
#define MOSAIC_ADAPT_PARS_H

#include <cmath>
#include "phycst.h"

//#################################################################
//
//  Pre-specified temperature levels (deg C) for outputing level
//  reflectivity products.
//
//#################################################################

    const int n_tlevels = 9;
    const float t_snd[9]={20,10,0,-10,-20,-30,-40,-50,-60};
    const int index_m10C = 3;
    const char t_snd_str[9][30]={"TWENTY_HEIGHT:","TEN_HEIGHT:",
                                  "ZERO_HEIGHT:","MINUSTEN_HEIGHT:",
                                  "MINUSTWENTY_HEIGHT:","MINUSTHIRTY_HEIGHT:",
                                  "MINUSFOURTY_HEIGHT:","MINUSFIFTY_HEIGHT:",
                                  "MINUSSIXTY_HEIGHT:"};

//   float zt_snd[n_tlevels] = { 0, 1500, 3000, 4500, 6000, 7500, 9000, 10500, 12000 };

//#################################################################
//
//  NIDS grid parameters
//
//#################################################################

    const float dx_nids_km = 1.0;  //km

    const float nidsref_low_thresh = 5.0;  //dbz

//#################################################################
//
//  Parameters for fill in data voids near echo tops with a low dbz value
//  representing clear air.
//
//#################################################################
  
    const float radar_cover_flag = -99.0;     // a reflectivity flag indicating radar coverage

    const float clear_air_fill_dbz = -30.0;     // a prespecified clear air dbz value
    const float clear_air_fill_hgt_s = 8000.0;  // a height (meter, MSL) threshold
                             // for stratiform case (when brightband exists)
    const float clear_air_fill_hgt_c = 12000.0;  // a height threshold for convective case


//#################################################################
//
//  Temporal parameters for managing satellite data 
//
//#################################################################

    const int tb_ir_freq = 900;
    const int tb_ir_window = 3600;

//#################################################################
//
//  Temporal parameters for managing METAR surface 
//  temperature data 
//
//#################################################################

    const int ts_metar_freq = 3600;
    const int ts_metar_window = 4500;

//#################################################################
//
//  Temporal parameters for managing model temperature data 
//
//#################################################################

    const int max_model_prods = 10;
    const int model_dat_freq = 3600;
    const int model_dat_window = 32400;

//#################################################################
//
//  QC parameters for clutter removal using satellite 
//  IR temperature and METAR/model surface temperatures.
//
//#################################################################

    const float t_z_miss = -999.0;
    const float dbz_clear_air = -25.0;  //dbz
    const float dT_clear_air_threshold = -8.0; // C degree 

//#################################################################
//
//  Precip flag parameters
//
//#################################################################

    const short stratiform_flag = 1;
    const short conv_flag = 6;

//#################################################################
//
//  VCP's used with bloom QC
//
//#################################################################

    const int VCP_31 = 31;
    const int VCP_32 = 32;

//#######################################################################
//
//  Scale factors used to scale float data before output as integers.
//  This scaling is used for better portability among different
//  platforms.
//
//#######################################################################

    const int latlon_scale = 1000;
    const int dlatdlon_scale = 100000;

    const int meter_scale = 1;
    const int meter_miss = -999;

    const int meter_diff_scale = 1;
    const int meter_diff_miss = -30000;

    const int ref_scale = 10;
    const int ref_miss = -999;

    //for crefh and lcrefh
    const int crefh_scale = 1000;  //was 1   //km MSL
    const int crefh_miss = -1;     //was -999;

    const int tb_K_scale = 10;
    const int tb_K_miss = -999;

    const int temp_C_scale = 10;
    const int temp_C_miss = -999;

    const int etp_scale = 1000;  //was 10;  //km MSL
    const int etp_miss = -1;     //was -999;

    const int shi_scale = 10;
    const int shi_miss = -999;

    const int posh_scale = 10;
    const int posh_miss = -999;

    const int mehs_scale = 10;
    const int mehs_miss = -999;

    const int hsr_scale = 10;
    const int hsr_miss = -999;

    const int hsrh_scale = 1000; //was 1;  //km AGL
    const int hsrh_miss = -1;    //was -999;

    const int vil_scale = 10;
    const int vil_miss = -999;

    const int vilD_scale = 10;
    const int vilD_miss = -999;

    const int pcpflag_scale = 1;
    const int pcpflag_miss = -999;

//#######################################################################
//
//  threshold for netcdf
//
//#######################################################################

    const float min_ref_rang = -20.0;
    const float max_ref_rang = 80.0;

    const float min_ref_diff_rang = -80.0;
    const float max_ref_diff_rang = 80.0;

    const float min_crefh_rang = 0.0;    //km MSL
    const float max_crefh_rang = 20.0;   //was 20000.0 mMSL

    const float min_chgt_diff_rang = -20000.0;   // meters difference (chgt_mosaic - chgt_cref)
    const float max_chgt_diff_rang = 20000.0;

    const float min_phase_rang = 0.0;
    const float max_phase_rang = 6.0;

    const float min_rate_rang = 0.0;
    const float max_rate_rang = 200.0;

    const float min_cloudt_rang = 160.0; 
    const float max_cloudt_rang = 340.0;
    
    const float min_radarcov_rang = 0.0; 
    const float max_radarcov_rang = 100.0;

    const float min_rainacc_rang = 0.0;
    const float max_rainacc_rang = 305.0;

    const float min_etp_rang = 0.0;      // km MSL
    const float max_etp_rang = 20.0;

    const float min_shi_rang = 0.0;
    const float max_shi_rang = 1000.0;

    const float min_posh_rang = 0.0;
    const float max_posh_rang = 1000.0;

    const float min_mehs_rang = 0.0;
    const float max_mehs_rang = 100.0;

    const float min_hsr_rang = -20.0;
    const float max_hsr_rang = 80.0;

    const float min_hsrh_rang = 0.0;   //kmAGL
    const float max_hsrh_rang = 20.0;  //was 20000.0 mAGL

    const float min_vil_rang = 0.0;
    const float max_vil_rang = 100.0;

    const float min_vilD_rang = 0.0;
    const float max_vilD_rang = 50.0;


//#######################################################################
//
//  threshold for Echo Top (ETP)
//
//#######################################################################

   const float ETP_threshold = 18.0;

#endif 
