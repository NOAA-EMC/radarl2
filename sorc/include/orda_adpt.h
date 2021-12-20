/*
 * RCS info
 * $Author: ryans $
 * $Locker:  $
 * $Date: 2006/02/28 22:56:01 $
 * $Id: orda_adpt.h,v 1.9 2006/02/28 22:56:01 ryans Exp $
 * $Revision: 1.9 $
 * $State: Exp $
 */
/*****************************************************************
//
//     Contains the structure defintion for ORDA adaptation data
//    
//
****************************************************************/

#ifndef ORDA_ADPT_H
#define ORDA_ADPT_H

#ifdef __cplusplus
extern "C"
{
#endif

typedef struct {

   char adap_file_name[12];		/* Name of in-use adaptation data file. */

   char adap_format[4];			/* Format of in-use adaptation data file. */

   char adap_revision[4];		/* Revision number of in-use adaptation 
					   data file. */
   
   char adap_date[12];			/* Last modified date of in-use adaptation
					   data file. */

   char adap_time[12];			/* Last modified time of in-use adaptation
					   data file. */

   float k1;				/* Azimuth position gain factor (K1), ratio. */

   float az_lat;			/* Latency of DCU Azi Measurement, seconds. */

   float k3;				/* Elevation position gain factor (K3), ratio. */

   float el_lat;			/* Latency of DCU Elev Measurement, seconds. */

   float parkaz;			/* Pedestal park position in azimuth, in degrees. */

   float parkel;			/* Pedestal park position in elevation, in degrees. */

   float a_fuel_conv[11];		/* Generator Fuel level height/capacity conversion, 
					   in percent.  NOTE:  Index 0 corresponds to 0% height, 
					   index 1 to 10% height, etc. */

   float a_min_shelter_temp;		/* Minimum equipment shelter alarm temperature,
					   in degrees Celsius. */

   float a_max_shelter_temp;		/* Maximum equipment shelter alarm temperature, 
					   in degrees Celsius. */
   
   float a_min_shelter_ac_temp_diff;	/* Minimum A/C discharge air temperature differential,
					   in degrees Celsius. */

   float a_max_xmtr_air_temp;		/* Maximum transmitter leaving air alarm, in degrees
					   Celsius. */

   float a_max_rad_temp;		/* Maximum radome alarm temperature, in degrees 
					   Celsius. */

   float a_max_rad_temp_rise;		/* Maximum radome minus ambient temperature difference, 
					   in degrees Celsius. */

   float ped_28V_reg_lim;		/* Pedestal +28 volt power supply tolerance, in
					   percent. */

   float ped_5V_reg_lim;		/* Pedestal +5 volt power supply tolerance, in
					   percent. */

   float ped_15V_reg_lim;		/* Pedestal +/-15 volt power supply tolerance, in
					   percent. */

   float a_min_gen_room_temp;		/* Minimum Generator shelter alarm temperature, in
					   degrees Celsius. */

   float a_max_gen_room_temp;		/* Maximum Generator shelter alarm temperature, in
					   degrees Celsius. */
   
   float dau_5V_reg_lim;		/* DAU +5 volt power supply tolerance, in percent. */

   float dau_15V_reg_lim;		/* DAU +/-15 volt power supply tolerance, in percent. */

   float dau_28V_reg_lim;		/* DAU +28 volt power supply tolerance, in percent. */

   float en_5V_reg_lim;			/* Encoder +5 volt power supply tolerance, in percent. */

   float en_5V_nom_volts;		/* Encoder +5 volt power supply, in volts. */

   char rpg_co_located[4];		/* RPG co-located. */

   char spec_filter_installed[4];	/* Transmitter spectrum filter installed. */

   char tps_installed[4];		/* Transition power source installed. */

   char rms_installed[4];		/* FAA RMS installed. */

   int a_hvdl_tst_int;			/* Reflectivity and clutter suppression test 
					   interval, in hours. */

   int a_rpg_lt_int;			/* RPG loop test interval, in minutes. */

   int a_min_stab_util_pwr_time;	/* Required interval time for stable utility power,
					   in minutes. */

   int a_gen_auto_exer_interval;	/* Maximum generator automatic exercise interval,
					   in hours. */

   int a_util_pwr_sw_req_interval;	/* Recommended switch to utility power time interval,
					   in minutes. */

   float a_low_fuel_level;		/* Low fuel tank warning level, in percent. */

   int config_chan_number;		/* Congiguration channel number. */

   int a_rpg_link_type;			/* RPG wideband link type (0 = direct, 1 = microwave,
					   2 = fiber optic). */

   int redundant_chan_config;		/* Redundant channel configuration (1 = single chan,
					   2 = FAA, 3 = NWS redundant). */

   float atten_table[104];		/* Test signal attenuator insertion losses, in dB.  
					   NOTE:  index 0 corresponds to 0 dB, index 1 to 
					   1 dB, etc. */

   float path_losses[69];		/* Path loss, in dB.  NOTE:  If you think I am going
					   to specify where all these path losses are 
					   measured, you need your head examined. */

   float chan_cal_diff;                 /* Non-controlling channel calibration difference */

   float spare_924;                     /* Spare. */

   float log_amp_factor[2];		/* RF detector log amplifier scale factor/bias for
					   converting receiver test data, in units of V/dBm
					   and volts, respectively. */

   int spare_936;			/* Spare. */

   float rnscale[13];			/* Receiver noise normalization, ratio.  NOTE:  Each
					   index corresponds to an elevation range.  If you
					   need to know what they are, look in the ICD, PAL. */

   float atmos[13];			/* Two-way atmospheric loss/km, in dB/km.  NOTE:  Each
					   index corresponds to an elevation range.  If you
					   need to know what they are, look in the ICD, PAL. */

   float el_index[12];			/* Bypass map generation elevation angles, in degrees. */

   int tfreq_mhz;			/* Transmitter frequency, in Mhz. */

   float base_data_tcn;			/* Point clutter suppression threshold, in dB. */

   float refl_data_tover;		/* Range unfolding overlay threshold (tover), in dB. */
					   
   float tar_dbz0_inc_lp;		/* Target system calibration (dbZ0) increment for long
					   pulse, in dB. */

   float spare_1108; 			/* Spare. */

   float spare_1112;			/* Spare. */

   float spare_1116;			/* Spare. */
  
   float lx_lp;				/* Matched filter loss for long pulse, in dB. */

   float lx_sp;				/* Matched filer loss for short pulse, in dB. */

   float meteor_param;			/* |K|^2 hydrometeor refractivity factor, ratio. */

   float beamwidth;			/* Antenna beamwidth, in degrees. */

   float antenna_gain;			/* Antenna gain including radome, in dB. */

   float spare_1140;		        /* Spare. */

   float vel_maint_limit;		/* Velocity check delta maintenance limit, in m/sec. */

   float wth_maint_limit;		/* Spectrum width check delta maintenance limit, in m/sec. */

   float vel_degrad_limit;		/* Velocity check delta degrade limit, in m/sec. */

   float wth_degrad_limit;		/* Spectrum width check delta degrade limit, in m/sec. */

   float noisetemp_dgrad_limit;		/* System noise temperature degrade limit for 
					   controlling channel, in degrees Kelvin. */

   float noisetemp_maint_limit;		/* System noise temperature maintenance limit for 
					   controlling channel, in degrees Kelvin. */

   int spare1168;	

   int spare1172;

   float kly_degrade_limit;		/* System noise temperature maintenance limit for non-
					   controlling channel, in dB. */

   float ts_coho;			/* COHO power at A10J2, in dBm. */

   float ts_cw;				/* CW test signal at A22J3, in dBm. */

   float ts_rf_sp;			/* RF drive test signal short pulse at 3A5J4, in dBm. */

   float ts_rf_lp;			/* RF drive test signal long pulse at 3A5J4, in dBm. */

   float ts_stalo;			/* STALO power at A5J2, in dBm. */

   float ts_noise;			/* RF noise test signal excess noise ratio at A22J4, in dB. */

   float xmtr_peak_power_high_limit;	/* Maximum transmitter peak power alarm level, in kW. */

   float xmtr_peak_power_low_limit;	/* Minimum transmitter peak power alarm level, in kW. */

   float dbz0_delta_limit;		/* Limit for difference between computed and target system
					   calibration coefficient (dbZ0), in dB. */

   float threshold1;			/* Bypass map generator noise threshold, in dB. */

   float threshold2;			/* Bypass map generator rejection ratio threshold, in dB. */

   float clut_supp_dgrad_lim;		/* Clutter suppression degrade limit, in dB. */

   float clut_supp_maint_lim;		/* Clutter supression maintenance limit, in dB. */

   float range0_value;			/* True range at start of first range bin, in km. */

   float xmtr_pwr_mtr_scale;		/* Scale factor used to convert transmitter power byte
					   data to watts, in watts per LSB. */

   float n_smooth;			/* Receiver noise calibration smoothing coefficient, ratio. */

   float tar_dbz0_sp;			/* Target system calibration (dbZ0) for short pulse, in dB. */

   int spare_1248;			/* Spare. */

   int deltaprf;			/* Site PRF set. */

   int spare_1256;			/* Spare. */

   int spare_1260;			/* Spare. */

   int tau_sp;				/* Pulse width of transmitter output in short pulse, in nsec. */

   int tau_lp;				/* Pulse width of transmitter output in long pulse, in nsec. */

   int nc_dead_value;			/* Number of 1/4 km bins of corrupted data at end of sweep, in bins. */

   int tau_rf_sp;			/* RF drive pulse width in short pulse, in nsec. */

   int tau_rf_lp;			/* RF drive pulse width in long pulse, in nsec. */

   float seg1lim;			/* High/low clutter map boundary elevation, in degrees. */

   float slatsec;			/* Site latitude - seconds, in seconds. */

   float slonsec;			/* Site longitude - seconds, in seconds. */

   int spare_1296;			/* Spare. */

   int slatdeg;				/* Site latitude - degrees, in degrees. */

   int slatmin;				/* Site latitude - minutes, in minutes. */
   
   int slongdeg;			/* Site longitude - degrees, in degrees. */

   int slongmin;			/* Site longitude - minutes, in minutes. */

   char slatdir[4];			/* Site latitude - direction. */

   char slondir[4];			/* Site longitude - direction. */

   int vc_ndx;				/* Index to current volume coverage pattern. */

   char vcpat11[1172];			/* Volume coverage pattern number 11 definition */

   char vcpat21[1172];			/* Volume coverage pattern number 21 definition */

   char vcpat31[1172];			/* Volume coverage pattern number 31 definition */

   char vcpat32[1172];			/* Volume coverage pattern number 32 definition */

   char vcpat300[1172];			/* Volume coverage pattern number 300 definition */

   char vcpat301[1172];			/* Volume coverage pattern number 301 definition */

   float az_correction_factor;		/* Azimuth boresight correction factor, in degrees. */ 

   float el_correction_factor;		/* Elevation boresight correction factor, in degrees. */ 

   char site_name[4];			/* Site name designation. */

   int ant_manual_setup_ielmin;		/* Minimum elevation angle, in degrees. */

   int ant_manual_setup_ielmax;		/* Maximum elevation angle, in degrees. */

   int ant_manual_setup_fazvelmax;	/* Maximum azimuth velocity, in degrees/second. */

   int ant_manual_setup_felvelmax;	/* Maximum elevation velocity, in degrees/second. */

   int ant_manual_setup_ignd_hgt;	/* Site ground height (above sea level), in meters. */

   int ant_manual_setup_irad_hgt;	/* Site radar height (above sea level), in meters. */

   int spare_8396[75];

   int rvp8NV_iwaveguide_length;	/* Waveguide length. */

   int spare_8700[11];			/* Spares. */

   float vel_data_tover;		/* Velocity unfolding overlay threshold, in dB. */

   float width_data_tover;		/* Width unfolding overlay threshold, in dB. */

   int spare_8752[3];			/* Spares. */

   float doppler_range_start;		/* Start range for first Doppler radial, in km. */
    
   int max_el_index;			/* The maximum index for the el_index parameters. */

   float seg2lim;			/* Clutter map boundary elevation between segments
					   2 and 3, in degrees. */

   float seg3lim;			/* Clutter map boundary elevation between segments
					   3 and 4, in degrees. */

   float seg4lim;			/* Clutter map boundary elevation between segments
					   4 and 5, in degrees. */

   int nbr_el_segments;			/* Number of elevation segments in ORDA clutter map. */

   float noise_long;			/* Receiver noise long pulse, in dBm. */

   float ant_noise_temp;		/* Antenna noise temperature, in K. */

   float noise_short;			/* Receiver noise short pulse, in dBm. */

   float noise_tolerance;		/* Receiver noise tolerance, in dB. */

   float min_dyn_range;			/* Minimum dynamic range, in dB. */

   int spare_8808[165];			/* Spares for future use. */

} ORDA_adpt_data_t;


typedef struct {

   RDA_RPG_message_header_t  msg_hdr;   /* ICD message header. */
   ORDA_adpt_data_t          rda_adapt; /* ORDA adaptation data. */

} ORDA_adpt_data_msg_t;

#ifdef __cplusplus
}
#endif

#endif
