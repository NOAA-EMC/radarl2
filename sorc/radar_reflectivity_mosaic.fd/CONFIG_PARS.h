// ##########################################################################
//  config_pars.h:
//      Declaring class for storing parameters in config file
//  Author: Shunxin (CIMMS/NSSL)
//  May 5, 2005
// ########################################################################

#ifndef CONFIG_PARS_H
#define CONFIG_PARS_H

#include <string>
#include <fstream>
#include <cstdlib>

#include "qpesums_utility.h"

#define VAR_NUMBER 30
#define DATATYPE_NUMBER 15
#define RADAR_NUMBER 300
#define MAX_NZ 100

using namespace std;

//structure for storing parameters of each tile
struct radar_pars
{
   char radar_name[5];
   char id[3];
   int nx;
   int ny;
   int nz;
   int xmin;
   int xmax;
   int ymin;
   int ymax;
   int ctrx;
   int ctry;
   float dx;
   float dy;
   float ctrlat;
   float ctrlon;
   int node_no1;
   int node_no2;
};

struct radar
{
    float latrad;
    float lonrad;
    float elvrad;

    short int vcpmod;

//  char vcpmod[10];

    char namrad[5];

    int ixrad;
    int jyrad;

    int vol;
    int ntilt;
};

struct nids_pars
{
    double dlon_n;
    double dlat_n;
    int nx_n;
    int ny_n;
};

//class for storing all parameters in config file
class CONFIG_PARS
{
   public:

     //varibles:

     //config file name
     string config_file;
     short arc_opt;     

     //WDSS-II related parameters
     int input_flag;
     char rad_product[100];
     char vcp_product[100];
     
     //pcpflag thresholds and ruc directories
     float strat_thresh;
     float conv_thresh;
     float conv_thresh_m10c;
     
     //bloom QC parameters
     float tsfc_bloom_threshold;

     //Time parameters
     int yr;
     int mo;
     int day;
     int hr;
     int min;
     int sec;
     long epoch_seconds;
     long UTC_seconds;
     char timeStamp[20];

     //dipictor name
     char depictorName[30];

     //data scale,missing value and value range
     short varscale[VAR_NUMBER],missing[VAR_NUMBER];
     short mint[VAR_NUMBER],maxt[VAR_NUMBER];

    //variable name and unit
    char varname[VAR_NUMBER][30];
    char varunit[VAR_NUMBER][30];

    int radar_number;          //# of radars cover the domain
    int variable_index;        //index of the wanted varible
    int variable_number;       //total number of netcdf file in each tile
    char variable_name[VAR_NUMBER][50];
    radar_pars radar_pars_list[RADAR_NUMBER];  //storing parameters of each radar
    int radar_index;
    int nradars;

    int mosaic_window; 
    int mosaic_freq;
    int rucwind_window;
    int rucwind_freq;
    int ruc_tsfc_window;
    int ir_mask_window;
    int ir_mask_freq;
    int eca_mask_window;
    int eca_mask_freq;
    int ctp_mask_window;
    int ctp_mask_freq;
    int prod_timestamp_opt;

    char grid_ref_dir[200];     // reference data on Cartesian grid

    //second data option etc
    int second_data_opt;
    char node_str[20];
    char prod_app[20];
    char src_data_list[30][20];
    char input_dir[200];       // radar data - on polar grid, after AP removal
    char sounding_dir[200];    // sounding directory
    char model_dat_dir[200];   // model T level index data directory
    char ruc_tsfc_dir[200];    //model surface temperature directory
    char rucwind_u_dir[200];
    char rucwind_v_dir[200];
    int sat_mask_qc_opt;
    int ir_mask_opt;
    char ir_mask_dir[200];
    int eca_mask_opt;
    char eca_mask_dir[200];
    int ctp_mask_opt;
    char ctp_mask_dir[200];

    //Parameters related to conus grid and conus reference data
    int conus_ref_opt;
    int conus_nx,conus_ny,conus_nz;
    float conus_dx,conus_dy;
    float conus_ctrlat,conus_ctrlon;
    float startlat,startlon;
    int sat_conus_nx,sat_conus_ny,sat_conus_nz;
    float sat_conus_dx,sat_conus_dy;

    //nids output options and directories.
    int lref_opt;
    int nids_lref_flag;
    char nids_lref_dir[200];
    char nids_mref3d_dir[DATATYPE_NUMBER][200];
    char nids_mref3d_name_tag[DATATYPE_NUMBER][20];

    int tref_opt;
    int nids_tref_flag;
    char nids_tref_dir[200];
    char nids_d3d_tref_dir[DATATYPE_NUMBER][200];
    char nids_d3d_tref_name_tag[DATATYPE_NUMBER][20];

    short product2d_nids_opt;
    int nids_cref_flag;
    char nids_cref_dir[200];
    char nids_d3d_cref_dir[DATATYPE_NUMBER][200];
    char nids_d3d_cref_name_tag[DATATYPE_NUMBER][20];

    int nids_chgt_flag;
    char nids_chgt_dir[200];
    char nids_d3d_chgt_dir[DATATYPE_NUMBER][200];
    char nids_d3d_chgt_name_tag[DATATYPE_NUMBER][20];

    int nids_href_flag;
    char nids_href_dir[200];
    char nids_d3d_href_dir[DATATYPE_NUMBER][200];
    char nids_d3d_href_name_tag[DATATYPE_NUMBER][20];

    int nids_hhgt_flag;
    char nids_hhgt_dir[200];
    char nids_d3d_hhgt_dir[DATATYPE_NUMBER][200];
    char nids_d3d_hhgt_name_tag[DATATYPE_NUMBER][20];

    int nids_vil_flag;
    char nids_vil_dir[200];
    char nids_d3d_vil_dir[DATATYPE_NUMBER][200];
    char nids_d3d_vil_name_tag[DATATYPE_NUMBER][20];

    int nids_vilD_flag;
    char nids_vilD_dir[200];
    char nids_d3d_vilD_dir[DATATYPE_NUMBER][200];
    char nids_d3d_vilD_name_tag[DATATYPE_NUMBER][20];

    int nids_shi_flag;
    char nids_shi_dir[200];
    char nids_d3d_shi_dir[DATATYPE_NUMBER][200];
    char nids_d3d_shi_name_tag[DATATYPE_NUMBER][20];

    int nids_posh_flag;
    char nids_posh_dir[200];
    char nids_d3d_posh_dir[DATATYPE_NUMBER][200];
    char nids_d3d_posh_name_tag[DATATYPE_NUMBER][20];

    int nids_mesh_flag;
    char nids_mesh_dir[200];
    char nids_d3d_mesh_dir[DATATYPE_NUMBER][200];
    char nids_d3d_mesh_name_tag[DATATYPE_NUMBER][20];

    int nids_etp_flag;
    char nids_etp_dir[200];
    char nids_d3d_etp_dir[DATATYPE_NUMBER][200];
    char nids_d3d_etp_name_tag[DATATYPE_NUMBER][20];

    //options and directories for multivariable netcdf files output
    int mosaic2d_netcdf_remote_output_opt;
    int mosaic2d_netcdf_flag;
    char mosaic2d_netcdf_dir[200];
    char mosaic2d_netcdf_dir_remote[200];
    char netcdf_d3d_dir[DATATYPE_NUMBER][20];
    char netcdf_d3d_dir_remote[DATATYPE_NUMBER][20];

    int mosaic3d_netcdf_remote_output_opt;
    int mosaic3d_netcdf_flag;
    char mosaic3d_netcdf_dir[200];
    char mosaic3d_netcdf_dir_remote[200];
    char netcdf_mref3d_dir[DATATYPE_NUMBER][20];
    char netcdf_mref3d_dir_remote[DATATYPE_NUMBER][20];
    
    //For WDSS-II NetCDF output
    int mosaic3d_wdssii_netcdf_flag;
    int tref3d_wdssii_netcdf_flag;
    int mosaic2d_wdssii_netcdf_flag;
    
    int tref3d_netcdf_flag;
    char tref3d_netcdf_dir[200];
    char netcdf_d3d_tref_dir[DATATYPE_NUMBER][20];

    //options and directories for binary products
    int bi_mref_flag;
    char bi_mref_dir[200];
    char binary_mref3d_dir[DATATYPE_NUMBER][20];
    char binary_mref3d_name_tag[DATATYPE_NUMBER][20];

    int bi_tref_flag;
    char bi_tref_dir[200];
    char binary_d3d_tref_dir[DATATYPE_NUMBER][20];
    char binary_d3d_tref_name_tag[DATATYPE_NUMBER][20];

    int bi_cref_flag;
    char bi_cref_dir[200];
    char binary_d3d_cref_dir[DATATYPE_NUMBER][20];
    char binary_d3d_cref_name_tag[DATATYPE_NUMBER][20];

    int bi_chgt_flag;
    char bi_chgt_dir[200];

    int bi_vil_flag;
    char bi_vil_dir[200];

    int bi_vilD_flag;
    char bi_vilD_dir[200];

    int bi_shi_flag;
    char bi_shi_dir[200];

    int bi_posh_flag;
    char bi_posh_dir[200];

    int bi_mehs_flag;
    char bi_mehs_dir[200];

    int bi_etp_flag;
    char bi_etp_dir[200];

    int bi_hsr_flag;
    char bi_hsr_dir[200];

    int bi_hsrh_flag;
    char bi_hsrh_dir[200];

    int bi_lcref_flag;
    char bi_lcref_dir[200];

    int bi_lcrefh_flag;
    char bi_lcrefh_dir[200];

    int bi_pcpflag_flag;
    char bi_pcpflag_dir[200];

    float lcref_max_hgt;

    //grid variables.
    int nx;     //# of grid points in W-E direction
    int ny;     //# of grid points in N-S direction
    int nz;     //# of grid points in horizontal direction
    int xmin;
    int xmax;
    int ymin;
    int ymax;
    float dx;
    float dy;
    float zp[MAX_NZ];
    float dx_meter;
    float dy_meter;
    float ctrlat; 
    float ctrlon;

    int mapproj;
    float trulat1;
    float trulat2;
    float trulon;
               
    int ternopt;
    char terndta[200];

    short debug_flag;
    int swap_flag;

    short bb_mode;

    //Parameters related to transfering mosaic nodes to nmqserver
    short int ldm_netcdf_opt;
    short int ldm_nids_opt;
    short int ldm_binary_opt;
    char pqinsert_file[200];
    char ldmd_log_file[200];
    char ldm_pq_file[200];

    int mosaic_info_flag;
    char mosaic_info_dir[200];

    //methods

    //constructor
    CONFIG_PARS(string configfile);

    //get parameters from config file
    int get_config();

    //get corner coordinate of each radar in domain
    //and corner coordinate of domain
    int get_corner_coordinate();

    ~CONFIG_PARS(){ };
};

#endif
