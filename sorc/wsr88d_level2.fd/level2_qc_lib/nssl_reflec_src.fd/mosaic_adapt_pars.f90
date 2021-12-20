!/* ##########################################################################
! *
! * mosaic_adapt_pars.h:  Declaring adaptable parameters for 3D
! *                       MOSAIC algorithm.
! *                       Some constant parameters are declared here, too.
! *
! * Author: Jian Zhang (CIMMS/NSSL)
! *   May 10, 1999.
! *
! * Modification History:
! *   October 22, 2001  Jian Zhang (CIMMS/NSSL)
! *   Renamed from "dataio.h" to "mosaic_adapt_pars.h" because the
! *   parameters defined here are mostly adaptable physical paramemters.
! *
! *   4/1/2004 Jian Zhang
! *   Fixed a couple of incorrect field range values for NetCDF.
! *
! * ########################################################################*/


!#ifndef MOSAIC_ADAPT_PARS_H
!#define MOSAIC_ADAPT_PARS_H
!
!#include <cmath>
!#include "phycst.h"


      module MOSAIC_ADAPT_PARS
       
       implicit none

!//#################################################################
!//
!//  Pre-specified temperature levels (deg C) for outputing level
!//  reflectivity products.
!//
!//#################################################################

!    const int n_tlevels = 9;
!    const float t_snd[9]={20,10,0,-10,-20,-30,-40,-50,-60};
!    const char t_snd_str[9][30]={"TWENTY_HEIGHT:","TEN_HEIGHT:",
!                                  "ZERO_HEIGHT:","MINUSTEN_HEIGHT:",
!                                  "MINUSTWENTY_HEIGHT:","MINUSTHIRTY_HEIGHT:",
!                                  "MINUSFOURTY_HEIGHT:","MINUSFIFTY_HEIGHT:",
!                                  "MINUSSIXTY_HEIGHT:"};

!//   float zt_snd[n_tlevels] = { 0, 1500, 3000, 4500, 6000, 7500, 9000, 10500, 12000 };

!//#################################################################
!//
!//  NIDS grid parameters
!//
!//#################################################################

       real, parameter ::    dx_nids_km = 1.0           !//km
       real, parameter ::    nidsref_low_thresh = 5.0   ! //dbz

!//#################################################################
!//
!//  Parameters for fill in data voids near echo tops with a low dbz value
!//  representing clear air.
!//
!//#################################################################
  
       real, parameter ::   radar_cover_flag = -99.0            ! a reflectivity flag indicating radar coverage
       real, parameter ::   clear_air_fill_dbz = -30.0          ! a prespecified clear air dbz value
       real, parameter ::   clear_air_fill_hgt_s = 8000.0       ! a height (meter, MSL) threshold for stratiform case (when brightband exists)
       real, parameter ::   clear_air_fill_hgt_c = 12000.0      ! a height threshold for convective case


!//#################################################################
!//
!//  Temporal parameters for managing satellite data 
!//
!//#################################################################

!     const int tb_ir_freq = 900;
!    const int tb_ir_window = 3600;

!//#################################################################
!//
!//  Temporal parameters for managing METAR surface 
!//  temperature data 
!//
!//#################################################################

!       const int ts_metar_freq = 3600;
!    const int ts_metar_window = 22000;   //4500;

!//#################################################################
!//
!//  Temporal parameters for managing model temperature data 
!//
!//#################################################################

!    const int max_model_prods = 10;
!    const int model_dat_freq = 3600;
!    const int model_dat_window = 32400;

!//#################################################################
!//
!//  QC parameters for clutter removal using satellite 
!//  IR temperature and METAR/model surface temperatures.
!/
!//#################################################################

       real, parameter ::  t_z_miss = -999.0
       real, parameter ::  dbz_clear_air = -25.0              !                //dbz
       real, parameter ::  dT_clear_air_threshold = -8.0     !          // C degree 

!//#######################################################################
!//
!//  Scale factors used to scale float data before output as integers.
!//  This scaling is used for better portability among different
!//  platforms.
!//
!//#######################################################################

       integer, parameter ::  latlon_scale = 1000
       integer, parameter ::  dlatdlon_scale = 100000

       integer, parameter ::  meter_scale = 1
       integer, parameter ::  meter_miss = -999

       integer, parameter ::  meter_diff_scale = 1
       integer, parameter ::  meter_diff_miss = -30000

       integer, parameter ::  ref_scale = 10
       integer, parameter ::  ref_miss = -999

       integer, parameter :: tb_K_scale = 10
       integer, parameter :: tb_K_miss = -999

       integer, parameter :: temp_C_scale = 10
       integer, parameter :: temp_C_miss = -999

       integer, parameter :: etp_scale = 10
       integer, parameter :: etp_miss = -999

       integer, parameter ::  shi_scale = 10
       integer, parameter ::  shi_miss = -999

       integer, parameter ::  posh_scale = 10
       integer, parameter ::  posh_miss = -999

       integer, parameter ::  mehs_scale = 10
       integer, parameter ::  mehs_miss = -999

       integer, parameter ::  hsr_scale = 10
       integer, parameter ::  hsr_miss = -999

       integer, parameter ::  hsrh_scale = 1
       integer, parameter ::  hsrh_miss = -999

       integer, parameter ::  vil_scale = 10
       integer, parameter ::  vil_miss = -999

       integer, parameter ::  vilD_scale = 10
       integer, parameter ::  vilD_miss = -999


!//#######################################################################
!//
!//  threshold for netcdf
!//
!//#######################################################################

       real, parameter ::  min_ref_rang = -20.0
       real, parameter ::  max_ref_rang = 80.0

       real, parameter ::  min_ref_diff_rang = -50.0
       real, parameter ::  max_ref_diff_rang = 50.0

       real, parameter ::  min_chgt_rang = 0.0      ! // meter MSL
       real, parameter ::  max_chgt_rang = 20000.0

       real, parameter ::  min_chgt_diff_rang = -20000.0    ! // meters difference (chgt_mosaic - chgt_cref)
       real, parameter ::  max_chgt_diff_rang = 20000.0

       real, parameter ::  min_phase_rang = 0.0
       real, parameter ::  max_phase_rang = 6.0

       real, parameter ::  min_rate_rang = 0.0
       real, parameter ::  max_rate_rang = 200.0

       real, parameter ::  min_cloudt_rang = 160.0 
       real, parameter ::  max_cloudt_rang = 340.0
    
       real, parameter ::  min_radarcov_rang = 0.0 
       real, parameter ::  max_radarcov_rang = 100.0

       real, parameter ::  min_hybscanh_rang = 0.0
       real, parameter ::  max_hybscanh_rang = 20000.0

       real, parameter ::  min_rainacc_rang = 0.0
       real, parameter ::  max_rainacc_rang = 305.0

       real, parameter ::  min_etp_rang = 0.0
       real, parameter ::  max_etp_rang = 20.0

       real, parameter ::  min_shi_rang = 0.0
       real, parameter ::  max_shi_rang = 100.0

       real, parameter ::  min_posh_rang = 0.0
       real, parameter ::  max_posh_rang = 100.0

       real, parameter ::  min_mehs_rang = 0.0
       real, parameter ::  max_mehs_rang = 100.0

       real, parameter ::  min_hsr_rang = -20.0
       real, parameter ::  max_hsr_rang = 80.0

       real, parameter ::  min_hsrh_rang = 0.0   ! // m AGL
       real, parameter ::  max_hsrh_rang = 20000.0

       real, parameter ::  min_vil_rang = 0.0
       real, parameter ::  max_vil_rang = 100.0

       real, parameter ::  min_vilD_rang = 0.0
       real, parameter ::  max_vilD_rang = 15.0


!//  threshold for Echo Top (ETP)

       real, parameter ::  ETP_threshold = 18.0

      end module  MOSAIC_ADAPT_PARS 
