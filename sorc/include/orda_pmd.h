/*
 * RCS info
 * $Author: ryans $
 * $Locker:  $
 * $Date: 2006/06/01 21:20:14 $
 * $Id: orda_pmd.h,v 1.13 2006/06/01 21:20:14 ryans Exp $
 * $Revision: 1.13 $
 * $State: Exp $
 */


#ifndef ORDA_PMD_H
#define ORDA_PMD_H

#include <rda_rpg_message_header.h>

/* Please Look at the table 8.2.4.8.1 in RDA/RPG ICD to get the most updated 
 * information about Range, Format and other information on individual fields
 */

typedef struct
{
  short spare1;
  short loop_back_test_status;
  unsigned int t1_output_frames;
  unsigned int t1_input_frames;
  unsigned int router_mem_used;
  unsigned int router_mem_free;
  short router_mem_util;
  short spare12;
  unsigned int csu_loss_of_signal;
  unsigned int csu_loss_of_frames;
  unsigned int csu_yellow_alarms;
  unsigned int csu_blue_alarms;
  unsigned int csu_24hr_err_scnds;
  unsigned int csu_24hr_sev_err_scnds;
  unsigned int csu_24hr_sev_err_frm_scnds;
  unsigned int csu_24hr_unavail_scnds;
  unsigned int csu_24hr_cntrld_slip_scnds;
  unsigned int csu_24hr_path_cding_vlns;
  unsigned int csu_24hr_line_err_scnds;
  unsigned int csu_24hr_brsty_err_scnds;
  unsigned int csu_24hr_degraded_mins;
  unsigned int lan_switch_mem_used;
  unsigned int lan_switch_mem_free;
  short lan_switch_mem_util;
  short spare44;
  unsigned int ntp_rejected_packets;
  int ntp_est_time_error;
  int gps_satellites;
  int gps_max_sig_strength;
  short ipc_status;
  short cmd_chnl_ctrl;
  short dau_tst_0;
  short dau_tst_1;
  short dau_tst_2;
  short spare58[41];
} comms_t;

typedef struct
{
  unsigned int ups_batt_status;
  unsigned int ups_time_on_batt;
  float ups_batt_temp;
  float ups_output_volt;
  float ups_output_freq;
  float ups_output_current;
  float pwr_admin_load;
  short spare113[24];
} power_t;

typedef struct
{
  short p_5vdc_ps;
  short p_15vdc_ps;
  short p_28vdc_ps;
  short n_15vdc_ps;
  short p_45vdc_ps;
  short flmnt_ps_vlt;
  short vcum_pmp_ps_vlt;
  short fcs_coil_ps_vlt;
  short flmnt_ps;
  short klystron_warmup;
  short trsmttr_avlble;
  short wg_swtch_position;
  short wg_pfn_trsfr_intrlck;
  short mntnce_mode;
  short mntnce_reqd;
  short pfn_swtch_position;
  short modular_ovrld;
  short modulator_inv_crnt;
  short modulator_swtch_fail;
  short main_pwr_vlt;
  short chrg_sys_fail;
  short invrs_diode_crnt;
  short trggr_amp;
  short circulator_temp;
  short spctrm_fltr_pressure;
  short wg_arc_vswr;
  short cbnt_interlock;
  short cbnt_air_temp;
  short cbnt_air_flow;
  short klystron_crnt;
  short klystron_flmnt_crnt;
  short klystron_vacion_crnt;
  short klystron_air_temp;
  short klystron_air_flow;
  short modulator_swtch_mntnce;
  short post_chrg_regulator;
  short wg_pressure_humidity;
  short trsmttr_ovr_vlt;
  short trsmttr_ovr_crnt;
  short fcs_coil_crnt;
  short fcs_coil_air_flow;
  short oil_temp;
  short prf_limit;
  short trsmttr_oil_lvl;
  short trsmttr_batt_chrgng;
  short hv_status;
  short trsmttr_recycling_smmry;
  short trsmttr_inoperable;
  short trsmttr_air_fltr;
  short zero_tst_bit_0;
  short zero_tst_bit_1;
  short zero_tst_bit_2;
  short zero_tst_bit_3;
  short zero_tst_bit_4;
  short zero_tst_bit_5;
  short zero_tst_bit_6;
  short zero_tst_bit_7;
  short one_tst_bit_0;
  short one_tst_bit_1;
  short one_tst_bit_2;
  short one_tst_bit_3;
  short one_tst_bit_4;
  short one_tst_bit_5;
  short one_tst_bit_6;
  short one_tst_bit_7;
  short xmtr_dau_interface;
  short trsmttr_smmry_status;
  short spare204;
  float trsmttr_rf_pwr;
  short spare207[2];
  float xmtr_peak_pwr;
  short spare211[2];
  float xmtr_rf_avg_pwr;
  short xmtr_pwr_mtr_zero;
  short spare216;
  unsigned int xmtr_recycle_cnt;
  short spare219[10];
} transmitter_t;

typedef struct
{
  short ac_1_cmprsr_shut_off;
  short ac_2_cmprsr_shut_off;
  short gnrtr_mntnce_reqd;
  short gnrtr_batt_vlt;
  short gnrtr_engn;
  short gnrtr_vlt_freq;
  short pwr_src;
  short trans_pwr_src;
  short gen_auto_run_off_switch;
  short aircraft_hzrd_lighting;
  short dau_uart;
  short spare240[11];
} tower_utilities_t;

typedef struct
{
  short equip_shltr_fire_smk;
  short gnrtr_shltr_fire_smk;
  short utlty_vlt_freq;
  short site_scrty_alarm;
  short scrty_equip;
  short scrty_sys;
  short rcvr_cnctd_to_antna;
  short radome_hatch;
  short ac_1_fltr_drty;
  short ac_2_fltr_drty;
  float equip_shltr_temp;
  float outside_amb_temp;
  float trsmttr_leaving_air_temp;
  float ac_1_dschrg_air_temp;
  float gnrtr_shltr_temp;
  float radome_air_temp;
  float ac_2_dschrg_air_temp;
  float dau_p_15v_ps;
  float dau_n_15v_ps;
  float dau_p_28v_ps;
  float dau_p_5v_ps;
  short cnvrtd_gnrtr_fuel_lvl;
  short spare284[7];
} equipment_shelter_t;

typedef struct
{
  float pdstl_p_28v_ps;
  float pdstl_p_15v_ps;
  float encdr_p_5v_ps;
  float pdstl_p_5v_ps;
  float pdstl_n_15v_ps;
  short p_150v_ovrvlt;
  short p_150v_undrvlt;
  short elev_srvo_amp_inhbt;
  short elev_srvo_amp_shrt_crct;
  short elev_srvo_amp_ovr_temp;
  short elev_motor_ovr_temp;
  short elev_stow_pin;
  short elev_pcu_parity;
  short elev_dead_lmt;
  short elev_p_nrml_lmt;
  short elev_n_nrml_lmt;
  short elev_encdr_light;
  short elev_grbx_oil;
  short elev_handwheel;
  short elev_amp_ps;
  short azmth_srvo_amp_inhbt;
  short azmth_srvo_amp_shrt_crct;
  short azmth_srvo_amp_ovr_temp;
  short azmth_motor_ovr_temp;
  short azmth_stow_pin;
  short azmth_pcu_parity;
  short azmth_encdr_light;
  short azmth_grbx_oil;
  short azmth_bull_gr_oil;
  short azmth_handwheel;
  short azmth_srvo_amp_ps;
  short srvo;
  short pdstl_intrlock_swtch;
  short azmth_pos_correction;
  short elev_pos_correction;
  short slf_tst_1_status;
  short slf_tst_2_status;
  short slf_tst_2_data;
  short spare334[7];
} antenna_pedestal_t;

typedef struct
{
  short coho_clock;
  short rf_gnrtr_freq_slct_osc;
  short rf_gnrtr_rf_stalo;
  short rf_gnrtr_phase_shft_coho;
  short p_9v_rcvr_ps;
  short p_5v_rcvr_ps;
  short pn_18v_rcvr_ps;
  short n_9v_rcvr_ps;
  short p_5v_rcvr_prtctr_ps;
  short spare350;
  float shrt_pulse_noise;
  float int_pulse_noise;
  float noise_temp;
  short spare357[6];
} rf_gnrtr_rcvr_t;

typedef struct
{
  float linearity;
  float dynamic_range;
  float delta_dbz0;
  float rcv_prot_atten;
  float kd_peak_measured;
  float kd_injct_pnt_diff;
  float shrt_pls_dbz0;
  float int_pls_dbz0;
  short velocity_prcssd;
  short width_prcssd;
  short velocity_rf_gen;
  short width_rf_gen;
  float i_naught;
  short spare385[24];
  float cltr_supp_delta;
  float cltr_supp_ufilt_pwr;
  float cltr_supp_filt_pwr;
  float trsmit_brst_pwr;
  float trsmit_brst_phase;
  short spare419[12];
} calib_t;

typedef struct
{
  short state_file_rd_stat;
  short state_file_wrt_stat;
  short bypass_map_file_rd_stat;
  short bypass_map_file_wrt_stat;
  short spare435;
  short spare436;
  short crnt_adpt_file_rd_stat;
  short crnt_adpt_file_wrt_stat;
  short cnsr_zn_file_rd_stat;
  short cnsr_zn_file_wrt_stat;
  short rmt_vcp_file_rd_stat;
  short rmt_vcp_file_wrt_stat;
  short bl_adpt_file_rd_stat;
  short spare444;
  short cf_map_file_rd_stat;
  short cf_map_file_wrt_stat;
  short gnrl_disk_io_err;
  short spare448[13];
} file_status_t;

typedef struct
{
  short dau_comm_stat;
  short hci_comm_stat;
  short pdstl_comm_stat;
  short sgnl_prcsr_comm_stat;
  short spare465;
  short rms_lnk_stat;
  short rpg_lnk_stat;
  short spare468[13];
} device_status_t;

typedef struct
{
  RDA_RPG_message_header_t msg_hdr;
  comms_t comms;
  power_t power;
  transmitter_t transmitter;
  tower_utilities_t tower_utilities;
  equipment_shelter_t equipment_shelter;
  antenna_pedestal_t antenna_pedestal;
  rf_gnrtr_rcvr_t rf_gnrtr_rcvr;
  calib_t calibration;
  file_status_t file_status;
  device_status_t device_status;
} orda_pmd_t;


#endif
