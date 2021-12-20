!/* ##########################################################################
! *
! *  CONFIG_PARS.h:  Declaring variables and functions for class "CONFIG_PARS" 
! *                  which store the configuration parameter varibles and functions
! *                  getting parameters from config file based on the idea of 
! *                  class inputpars.
! *
! *###########################################################################
! *  07/10/2004   Shunxin Wang ( NSSL/CIMMS )
! * ########################################################################*/

      module CONFIG_PARS 
        
       implicit none
       integer, parameter :: MAX_NZ = 100, LEN2 = 100

       type CCOUT_CONFIG_PARS

        character*40    :: config_file_name
        character*8     :: radar_name
        character*40    :: ldm_file_name
        character*40    :: tilt_file_names

        ! raw input data type etc 
        integer(2)      :: data_type;
        integer(2)      :: radial_gap_filling_opt
        integer(2)      :: DBZ_correction_opt
        integer(2)      :: unqc_src_opt
        integer(2)      :: qc_src_opt
        integer(2)      :: sync_src_opt
        integer(2)      :: sync_qc_src_opt
        integer(2)      :: alignment_max_opt

        ! parameters for hardware test check
        integer(2)      :: hardware_test_check_opt
        integer(4)      :: hardware_test_check_range
        character*40    :: hardware_test_check_dir
        real            :: diff_threshold_of_echo_coverage
        real            :: threshold_of_echo_value
        integer         :: hardware_test_check_freq
        integer         :: hardware_test_check_window

        ! parameters about the algorithm
        integer(2)      :: run_qpesums_algorithms
        integer(4)      :: qpesums_gate_range
        integer(4)      :: qpesums_azimuth_range
        real            :: qpesums_gate_spacing
        real            :: qpesums_azimuth_spacing
        integer(2)      :: min_number_of_tilts
        integer(2)      :: short_pulse
        integer(2)      :: debug_flag
        integer(2)      :: byteswap_flag
        integer(4)      :: remap_freq

        ! parameters relared to raw level2 data  
        integer(2)      :: num_azimuths
        integer(2)      :: max_num_azimuths
        integer(2)      :: max_num_ref_gates
        integer(2)      :: max_num_doppler_gates
        integer(2)      :: precip_time_lag
        integer(2)      :: clear_air_time_lag
        integer(2)      :: start_time_lag
        integer(2)      :: vrp_low_res
        real            :: low_res_velocity
        real            :: high_res_velocity
        integer(2)      :: level2_pause_time
        character*40    :: file_header_root_name
        character*40    :: file_header_root_name_2

        ! parameters related to gematronik data(ones not same as level2)
        integer(2)      :: max_num_long_tilts
        integer(2)      :: size_short_pulse_gate
        integer(2)      :: size_long_pulse_gate

        ! parameters relared to hybrid tilt file                          
        real            :: hybrid_tilt_scale                   
        integer         :: hybrid_tilt_code
        integer         :: hybrid_tilt_units_code
        character*40    :: hybrid_filename_tag   
        integer(2)      :: hybrid_file_opt
        character*40    :: blockerge_filename_tag   

        ! directories for parameters related to input reference data      
        character*30    :: input_hybrid_tilts
        character*30    :: input_block_luts
        character*30    :: radar_input_file
        character*30    :: input_nids_info

        ! parameters related to sounding
        character*30    :: env_sounding_directory
        character*30    :: env_sounding_file_tag  
        character*30    :: env_tag               
        character(len=LEN2)  :: model_sounding_directory
        character(len=LEN2)  :: model_sounding_file_tag 
        character(len=LEN2)  :: model_tag             
        character(len=LEN2)  :: sounding_usage_directory
        character(len=LEN2)  :: sounding_usage_file_tag 
        character(len=LEN2)  :: climate_sounding_file
        character(len=LEN2)  :: tsfc_model_dir
        real            :: radar_ctr_tsfc_threshold
        real            :: radar_ctr_tsfc
    
        ! Parameters related to conus grid and conus reference data
        integer         :: conus_ref_opt
        integer         :: conus_nx
        integer         :: conus_ny
        integer         :: conus_nz
        real            :: conus_dx
        real            :: conus_dy
        real            :: conus_ctrlat
        real            :: conus_ctrlon
        real            :: startlon
        real            :: startlat

        ! parameters related to output binary file                         
        character(len=LEN2)  :: binary_output_directory
        integer(2)      :: output_bref_raw_binary
        integer(2)      :: output_bref_binary
        integer(2)      :: output_bref_qc_binary
        integer(2)      :: output_da_bvel_binary
        integer(2)      :: output_bvel_binary
        integer(2)      :: output_bvel_raw_binary
        integer(2)      :: output_spwi_binary
        integer(2)      :: output_spwi_raw_binary
        integer(2)      :: output_cref_binary
        integer(2)      :: output_chgt_binary
        integer(2)      :: output_cref_qc_binary
        integer(2)      :: output_chgt_qc_binary
        integer(2)      :: output_lcref_binary
        integer(2)      :: output_lchgt_binary
        integer(2)      :: output_lcref_qc_binary
        integer(2)      :: output_lchgt_qc_binary
        integer(2)      :: output_href_binary
        integer(2)      :: output_hhgt_binary
        integer(2)      :: output_href_qc_binary
        integer(2)      :: output_warm_rate_binary
        integer(2)      :: output_warm_flag_binary
        integer(2)      :: output_cool_rate_binary
        integer(2)      :: output_cool_flag_binary

        ! parameters related to output nids file                           
        character(len=LEN2)  :: nids_output_directory
        integer(2)      :: output_bref_raw_nids
        integer(2)      :: output_bref_nids
        integer(2)      :: output_bref_qc_nids
        integer(2)      :: output_da_bvel_nids
        integer(2)      :: output_bvel_raw_nids
        integer(2)      :: output_bvel_nids
        integer(2)      :: output_spwi_raw_nids
        integer(2)      :: output_spwi_nids
        integer(2)      :: output_cref_nids
        integer(2)      :: output_chgt_nids
        integer(2)      :: output_cref_qc_nids
        integer(2)      :: output_chgt_qc_nids
        integer(2)      :: output_lcref_nids
        integer(2)      :: output_lchgt_nids
        integer(2)      :: output_lcref_qc_nids
        integer(2)      :: output_lchgt_qc_nids
        integer(2)      :: output_href_nids
        integer(2)      :: output_hhgt_nids
        integer(2)      :: output_href_qc_nids
        integer(2)      :: output_warm_rate_nids
        integer(2)      :: output_warm_flag_nids
        integer(2)      :: output_cool_rate_nids
        integer(2)      :: output_cool_flag_nids

        ! parameters related to base reflectivity                          
        integer(2)      :: bref_qpesums_product_code
        integer(2)      :: bref_units_code
        real            :: bref_scale
        integer(2)      :: bref_nids_product_code
        integer(2)      :: bref_nids_hi_index
        integer(2)      :: bref_nids_lo_index
        integer(2)      :: bref_nids_missing_index
        character(len=LEN2)  :: bref_nids_values_tag
        character(len=LEN2)  :: bref_nids_colors_tag
        character(len=LEN2)  :: bref_product_label

        ! parameters related to base velocity
        integer(2)      :: bvel_qpesums_product_code
        integer(2)      :: bvel_da_qpesums_product_code
        integer(2)      :: bvel_units_code            
        real            :: bvel_scale                    
        integer(2)      :: bvel_nids_product_code   
        integer(2)      :: bvel_nids_da_product_code
        integer(2)      :: bvel_nids_hi_index      
        integer(2)      :: bvel_nids_lo_index     
        integer(2)      :: bvel_nids_missing_index 
        integer(2)      :: bvel_nids_range_fold_index
        integer(2)      :: bvel_nids_neg_zero_index 
        integer(2)      :: bvel_nids_pos_zero_index 
        character(len=LEN2)  :: bvel_nids_values_tag       
        character(len=LEN2)  :: bvel_nids_colors_tag       
        integer(2)      :: num_range_fold_elements 
        integer(2)      :: range_fold_text_width  
        character(len=LEN2)  :: bvel_product_label       

        ! parameters related to output base SPEC_WIDTH info  
        integer(2)      :: spwi_qpesums_product_code
        integer(2)      :: spwi_da_qpesums_product_code
        integer(2)      :: spwi_units_code
        real            :: spwi_scale
        integer(2)      :: spwi_nids_product_code
        integer(2)      :: spwi_nids_da_product_code
        integer(2)      :: spwi_nids_hi_index
        integer(2)      :: spwi_nids_lo_index
        integer(2)      :: spwi_nids_missing_index
        integer(2)      :: spwi_nids_range_fold_index
        character(len=LEN2)  :: spwi_nids_values_tag
        character(len=LEN2)  :: spwi_nids_colors_tag
        character(len=LEN2)  :: spwi_product_label

        ! parameters related to output brightband info                     
        character(len=LEN2)  :: brightband_directory
        real            :: brightband_dropoff_ratio_threshold
        real            :: brightband_dbz_threshold
        real            :: brightband_std_dev_threshold
        real            :: brightband_dbz_ratio_threshold
        integer(4)      :: brightband_number_of_trend_records
        real            :: brightband_vcp11_start_point
        real            :: brightband_other_start_point
        real            :: brightband_end_point
        real            :: brightband_default_depth
        integer         :: brightband_past_minutes_to_average
        integer(2)      :: vcp11 
        integer(2)      :: num_sectors
        integer(2)      :: hybrid_tilt_threshold
        integer(2)      :: valid_num_tilts
        real            :: missing_bb
!      double missing_bb_time;
        character(len=LEN2)  :: missing_bb_time                 ! yq
        character(len=LEN2)  :: time_series_tag
!      float default_bb_depth;
        integer(2)      :: default_vols_to_avg
        integer(2)      :: num_bb_info_in_header
        real            :: ground_height

        ! parameters related to output Composite Reflectivity info         
        integer(2)      :: cref_qpesums_product_code
        integer(2)      :: cref_units_code
        real            :: cref_scale
        integer(2)      :: cref_tilt_number
        real            :: cref_tilt_elevation
        integer(2)      :: cref_nids_product_code
        integer(2)      :: cref_nids_hi_index
        integer(2)      :: cref_nids_lo_index
        integer(2)      :: cref_nids_missing_index
        character(len=LEN2)  :: cref_nids_values_tag
        character(len=LEN2)  :: cref_nids_colors_tag
        character(len=LEN2)  :: cref_product_label

        ! parameters related to output COMP_HEIGHT info
        integer(4)      :: comp_height_gate_range
        integer(2)      :: chgt_qpesums_product_code
        integer(2)      :: chgt_units_code
        real            :: chgt_scale
        integer(2)      :: chgt_tilt_number
        real            :: chgt_tilt_elevation
        integer(2)      :: chgt_nids_product_code
        integer(2)      :: chgt_nids_hi_index
        integer(2)      :: chgt_nids_lo_index
        integer(2)      :: chgt_nids_missing_index
        character(len=LEN2)  :: chgt_nids_values_tag
        character(len=LEN2)  :: chgt_nids_colors_tag
        character(len=LEN2)  :: chgt_product_label

        ! parameters related to output lower level Composite Reflectivity info
        integer(2)      :: lcref_max_tilt_num
        integer(2)      :: lcref_qpesums_product_code
        integer(2)      :: lcref_units_code
        real            :: lcref_scale
        integer(2)      :: lcref_tilt_number
        real            :: lcref_tilt_elevation
        integer(2)      :: lcref_nids_product_code
        integer(2)      :: lcref_nids_hi_index
        integer(2)      :: lcref_nids_lo_index
        integer(2)      :: lcref_nids_missing_index
        character(len=LEN2)  :: lcref_nids_values_tag
        character(len=LEN2)  :: lcref_nids_colors_tag
        character(len=LEN2)  :: lcref_product_label

        ! parameters related to output COMP_HEIGHT info
        integer(4)      :: lcomp_height_gate_range
        integer(2)      :: lchgt_qpesums_product_code
        integer(2)      :: lchgt_units_code
        real            :: lchgt_scale
        integer(2)      :: lchgt_tilt_number
        real            :: lchgt_tilt_elevation
        integer(2)      :: lchgt_nids_product_code
        integer(2)      :: lchgt_nids_hi_index
        integer(2)      :: lchgt_nids_lo_index
        integer(2)      :: lchgt_nids_missing_index
        character(len=LEN2)  :: lchgt_nids_values_tag
        character(len=LEN2)  :: lchgt_nids_colors_tag
        character(len=LEN2)  :: lchgt_product_label

        ! parameters related to output HYBRID_REFLECT info                 
        integer(2)      :: href_qpesums_product_code
        integer(2)      :: href_units_code
        real            :: href_scale
        integer(2)      :: href_tilt_number
        real            :: href_tilt_elevation
        integer(2)      :: href_nids_product_code
        integer(2)      :: href_nids_hi_index
        integer(2)      :: href_nids_lo_index
        integer(2)      :: href_nids_missing_index
        character(len=LEN2)  :: href_nids_values_tag
        character(len=LEN2)  :: href_nids_colors_tag
        character(len=LEN2)  :: href_product_label

        ! parameters related to output HYBRID_HEIGHT info
        integer(4)      :: hybrid_height_gate_range 
        integer(2)      :: hhgt_qpesums_product_code
        integer(2)      :: hhgt_units_code
        real            :: hhgt_scale
        integer(2)      :: hhgt_tilt_number
        real            :: hhgt_tilt_elevation
        integer(2)      :: hhgt_nids_product_code
        integer(2)      :: hhgt_nids_hi_index
        integer(2)      :: hhgt_nids_lo_index
        integer(2)      :: hhgt_nids_missing_index
        character(len=LEN2)  :: hhgt_nids_values_tag
        character(len=LEN2)  :: hhgt_nids_colors_tag
        character(len=LEN2)  :: hhgt_product_label

        ! parameters related to subdirectories and file name tag 
        character(len=LEN2)  :: bref_raw_nids_sub_dir
        character(len=LEN2)  :: bref_raw_tag
        character(len=LEN2)  :: bref_nids_sub_dir
        character(len=LEN2)  :: bref_tag
        character(len=LEN2)  :: bref_nf_nids_sub_dir
        character(len=LEN2)  :: bref_nf_tag
        character(len=LEN2)  :: bvel_da_nids_sub_dir
        character(len=LEN2)  :: bvel_da_tag
        character(len=LEN2)  :: bref_qc_nids_sub_dir
        character(len=LEN2)  :: bref_qc_tag
        character(len=LEN2)  :: cref_nids_sub_dir
        character(len=LEN2)  :: cref_tag
        character(len=LEN2)  :: chgt_nids_sub_dir
        character(len=LEN2)  :: chgt_tag
        character(len=LEN2)  :: cref_qc_nids_sub_dir
        character(len=LEN2)  :: cref_qc_tag
        character(len=LEN2)  :: chgt_qc_nids_sub_dir
        character(len=LEN2)  :: chgt_qc_tag
        character(len=LEN2)  :: lcref_nids_sub_dir
        character(len=LEN2)  :: lcref_tag
        character(len=LEN2)  :: lchgt_nids_sub_dir
        character(len=LEN2)  :: lchgt_tag
        character(len=LEN2)  :: lcref_qc_nids_sub_dir
        character(len=LEN2)  :: lcref_qc_tag
        character(len=LEN2)  :: lchgt_qc_nids_sub_dir
        character(len=LEN2)  :: lchgt_qc_tag
        character(len=LEN2)  :: href_nids_sub_dir
        character(len=LEN2)  :: href_tag
        character(len=LEN2)  :: hhgt_nids_sub_dir
        character(len=LEN2)  :: hhgt_tag
        character(len=LEN2)  :: href_qc_nids_sub_dir
        character(len=LEN2)  :: href_qc_tag
        character(len=LEN2)  :: wflag_nids_sub_dir
        character(len=LEN2)  :: wflag_tag
        character(len=LEN2)  :: wrate_nids_sub_dir
        character(len=LEN2)  :: wrate_tag
        character(len=LEN2)  :: flag_cool_nids_sub_dir
        character(len=LEN2)  :: flag_cool_tag
        character(len=LEN2)  :: rate_cool_nids_sub_dir
        character(len=LEN2)  :: rate_cool_tag
        character(len=LEN2)  :: pcp_flag_nids_sub_dir
        character(len=LEN2)  :: pcp_flag_tag
        character(len=LEN2)  :: pcp_rate_nids_sub_dir
        character(len=LEN2)  :: pcp_rate_tag
        character(len=LEN2)  :: bvel_nids_sub_dir
        character(len=LEN2)  :: bvel_tag
        character(len=LEN2)  :: bvel_raw_nids_sub_dir
        character(len=LEN2)  :: bvel_raw_tag
        character(len=LEN2)  :: spwi_nids_sub_dir
        character(len=LEN2)  :: spwi_tag
        character(len=LEN2)  :: spwi_raw_nids_sub_dir
        character(len=LEN2)  :: spwi_raw_tag

        ! parameters related to AP and noise quality control               
        integer(2)      :: qc_opt
        integer(2)      :: zero_out_opt
        real            :: start_hgt
        real            :: winter_start_hgt
        real            :: start_hgt1
        real            :: winter_start_hgt1
        real            :: end_hgt
        real            :: winter_end_hgt
        real            :: gdz_threshold
        real            :: texture_low_threshold
        real            :: texture_high_threshold
        real            :: ref_threshold
        real            :: noise_filter_ratio
        integer         :: noise_filter_size
        real            :: stratiform_search_radius
        real            :: donut_ref_threshold
        integer         :: donut_num_threshold

        ! parameters related to velocity dealiasing                       
        integer(2)      :: velocity_dealiasing_mode

        ! parameters for synchrinization
        integer(2)      :: sync_opt
        integer         :: time_diff
        character(len=LEN2)  :: rucwindfield_dir
        integer         :: rucwindfield_nx
        integer         :: rucwindfield_ny
        integer         :: rucwindfield_freq
        integer         :: rucwindfield_window
        real            :: rucwindfield_nwlat
        real            :: rucwindfield_nwlon
        real            :: rucwindfield_missing
        real            :: rucwindfield_dx
        real            :: rucwindfield_dy

        ! parametrs for PRECIP_FLAG or convective_strtiform                
        real            :: con_strat_low_dbz
        real            :: con_strat_high_dbz
        real            :: depth_of_good_snow
        integer(2)      :: flag_qpesums_product_code
        integer(2)      :: flag_units_code
        real            :: flag_scale        
        integer(2)      :: flag_tilt_number 
        real            :: flag_tilt_elevation 
        integer(2)      :: flag_nids_product_code
        integer(2)      :: flag_nids_hi_index   
        integer(2)      :: flag_nids_lo_index  
        integer(2)      :: flag_nids_missing_index 
        character(len=LEN2)  :: flag_nids_values_tag      
        character(len=LEN2)  :: flag_nids_colors_tag     
        integer(2)      :: good_rain_flag       
        integer(2)      :: bad_rain_flag       
        integer(2)      :: good_snow_flag     
        integer(2)      :: bad_snow_flag     
        integer(2)      :: convective_flag  
        character(len=LEN2)  :: flag_product_label 

        ! parametrs for RAIN_RATE or different rain types
        integer(2)      :: hr_type             
        real            :: rate_aconv_r
        real            :: rate_bconv_r
        real            :: rate_astrat_r
        real            :: rate_bstrat_r
        real            :: rate_asnow_r
        real            :: rate_bsnow_r
        integer(2)      :: rate_qpesums_product_code
        integer(2)      :: rate_units_code         
        real            :: rate_scale                 
        integer(2)      :: rate_tilt_number      
        real            :: rate_tilt_elevation      
        integer(2)      :: rate_nids_product_code
        integer(2)      :: rate_nids_hi_index   
        integer(2)      :: rate_nids_lo_index  
        integer(2)      :: rate_nids_missing_index 
        character(len=LEN2)  :: rate_nids_values_tag      
        character(len=LEN2)  :: rate_nids_colors_tag     
        real            :: rate_missing             
        integer(2)      :: rate_base           
        real            :: rate_fraction          
        character(len=LEN2)  :: rate_product_label   

        ! parameters for climatology                                       
        character(len=LEN2)  :: sea_clutter_par_file
        character(len=LEN2)  :: sea_clutter_par_file_tag
        integer         :: climat_flag
        character(len=LEN2)  :: climat_input_directory
        character(len=LEN2)  :: climat_output_directory
        character(len=LEN2)  :: climat_nids_directory
        character(len=LEN2)  :: climat_binary_directory

        ! parameters for polar monitor file                                
        character(len=LEN2)  :: monitor_directory
        integer(2)      :: polar_monitor_file_flag

   
!      vector<unsigned short> nyquist_velocity;

!      //##################################################################
!      //######       CONFIG PARAMETERS FOR REMAP                    ######
!      //##################################################################
        integer         :: yr
        integer         :: mo
        integer         :: day
        integer         :: hr
        integer         :: minute             !  min
        integer         :: sec
        integer(4)      :: epoch_seconds
        integer(4)      :: UTC_seconds

        ! Parameters related to input reference data
        character(len=LEN2)  :: lut_dir
        character(len=LEN2)  :: g_refdata_dir
        character(len=LEN2)  :: p_refdata_dir

        ! Option for derive reflectivities on pre-specified height levels
        integer         :: nids_lref_flag
        character(len=LEN2)  :: nids_lref_name_tag
        character(len=LEN2)  :: nids_lref_dir
        character(len=LEN2)  :: nids_unqc_lref_name_tag
        character(len=LEN2)  :: nids_qc_lref_name_tag
        character(len=LEN2)  :: nids_sync_lref_name_tag
        character(len=LEN2)  :: nids_sync_qc_lref_name_tag
        character(len=LEN2)  :: nids_unqc_lref_dir
        character(len=LEN2)  :: nids_qc_lref_dir
        character(len=LEN2)  :: nids_sync_lref_dir
        character(len=LEN2)  :: nids_sync_qc_lref_dir

        ! options and directories for binary files output
        integer         :: mosaic3d_binary_flag
        character(len=LEN2)  :: mosaic3d_binary_name_tag
        character(len=LEN2)  :: mosaic3d_binary_dir
        character(len=LEN2)  :: mosaic3d_unqc_binary_name_tag
        character(len=LEN2)  :: mosaic3d_qc_binary_name_tag
        character(len=LEN2)  :: mosaic3d_sync_binary_name_tag
        character(len=LEN2)  :: mosaic3d_sync_qc_binary_name_tag
        character(len=LEN2)  :: mosaic3d_unqc_binary_dir
        character(len=LEN2)  :: mosaic3d_qc_binary_dir
        character(len=LEN2)  :: mosaic3d_sync_binary_dir
        character(len=LEN2)  :: mosaic3d_sync_qc_binary_dir

        ! options and directories for multivariable netcdf files output
        integer         :: mosaic3d_netcdf_flag
        character(len=LEN2)  :: mosaic3d_netcdf_dir
        character(len=LEN2)  :: mosaic3d_unqc_netcdf_dir
        character(len=LEN2)  :: mosaic3d_qc_netcdf_dir
        character(len=LEN2)  :: mosaic3d_sync_netcdf_dir
        character(len=LEN2)  :: mosaic3d_sync_qc_netcdf_dir

        ! Grid configuration parameters.
        integer         :: nx
        integer         :: ny
        integer         :: nz
        real            :: dx
        real            :: dy
        real            :: zp(MAX_NZ)
        real            :: dx_meter
        real            :: dy_meter
        real            :: ctrlat
        real            :: ctrlon

        ! Map projection parameters:
        integer         :: mapproj
        real            :: trulat1
        real            :: trulat2
        real            :: trulon

        ! Options and parameters related to terrain specification.
        integer         :: ternopt;
        character(len=LEN2)  :: terndta;

        ! remap_opt and parameters
        integer         :: remap_opt
        real            :: delv_vcp_margin1
        real            :: delv_vcp_margin2
        real            :: length_scale_km
        real            :: radius_infl_km
        real            :: wgt_cutoff

        ! Swap option for NIDS output
        integer(2)      :: bb_mode
        integer         :: swap_flag

        ! model grid parameters
        integer(2)      :: nx_model
        integer(2)      :: ny_model
        integer(2)      :: nz_model

        ! Minutes back for rerun executable
        integer         :: rerun_back_min

        ! Parameters related to transfering mosaic nodes to nmqserver
        integer(2)      :: ldm_netcdf_opt
        integer(2)      :: ldm_nids_opt
        character(len=LEN2)  :: pqinsert_file
        character(len=LEN2)  :: ldmd_log_file
        character(len=LEN2)  :: ldm_pq_file

        ! parameters for polar monitor file
        integer         :: monitor_file_opt
        character(len=LEN2)  :: monitor_file_directory

       end type CCOUT_CONFIG_PARS
      
 
       type radar

        real            :: latrad
        real            :: lonrad
        real            :: elvrad
        integer(2)      :: vcpmod
!//  char vcpmod[10];
        character*5     :: namrad
        integer         :: ixrad
        integer         :: jyrad
        integer         :: vol
        integer         :: ntilt

       end type radar


       type nids_pars

        real(8)         :: dlon_n
        real(8)         :: dlat_n
        integer         :: nx_n
        integer         :: ny_n
      
       end type nids_pars


       type scan_strategy

        integer(2)      :: scan_mode
        real            :: beam_width
        integer(2)      :: ntilts
        real            :: elv(100)
        integer(2)      :: ngates
        real            :: gate1
        real            :: gate_spc
        integer(2)      :: nrays
        real            :: azm1
        real            :: azm_spc

       end type scan_strategy


       type tilt_moslut_header

        character*5     :: radarname
     !  integer(1)      :: radarname(5)
        real            :: latrad
        real            :: lonrad
        real            :: hgtrad
        real            :: elv_angle
        integer         :: nrays
        integer         :: ngates
        real            :: gate1
        real            :: gate_spc
        real            :: azm1
        real            :: azm_spc
        real            :: ctrlat
        real            :: ctrlon
        integer         :: nx
        integer         :: ny
        integer         :: nz
        real            :: dx
        real            :: dy
        real            :: zp(MAX_NZ)
        integer(2)      :: mapproj
        real            :: trulat1
        real            :: trulat2
        real            :: trulon

       end type tilt_moslut_header


       type tilt_data_header

        character*5     :: radarname
        real            :: latrad
        real            :: lonrad
        real            :: hgtrad
        integer         :: year
        integer         :: month
        integer         :: day
        integer         :: hour
        integer         :: minute
        integer         :: second
        integer(2)      :: vcpmode
        real            :: elv_angle
        integer         :: nrays
        integer         :: ngates
        real            :: gate1
        real            :: gate_spc
        real            :: azm1
        real            :: azm_spc
        integer         :: data_scale
        integer         :: data_missing_flag   ! missing value with the scaling factor
        real            :: missing_refl        ! missing value without the scaling factor

       end type tilt_data_header

! int compare_tilt_lut ()

      end module CONFIG_PARS
