/* ##########################################################################
 *     func_prototype.h:  Declaring prototypes for MOSAIC algorithm
 *
 *      Author: Wenwu Xia (CIMMS/NSSL),Jian Zhang (CIMMS/NSSL)
 *      July 10, 2000
 *
 *      Modification History:
 *
 * ########################################################################*/

#ifndef FUNC_PROTOTYPE_H
#define FUNC_PROTOTYPE_H

#include <string>
#include <vector>
#include <cstdlib>

#include "CONFIG_PARS.h"
#include "grid_vars.h"
#include "maplib3d.h"
#include "radar_sum.h"
//#include "NETCDF_HANDLER.h"

//#include "LatLonHeightField.h"

using namespace std;

template <class basetype>
void test_pointer( basetype *x)
{
    if(x == NULL)
    {
      cout << "pointer declaring (or deleting) error. Aborting."  << endl;
      exit(1);
    }
}

template <class basetype>
basetype get_max(const basetype &x,const basetype &y)
{
        return(x<y?y:x);
};

template <class basetype>
basetype get_min(const basetype &x,const basetype &y)
{
        return(x>y?y:x);
};

template <class basetype>
int get_sign(const basetype &x)
{
  if(x>=0)  return 1;
  else return -1;
};

template <class basetype>
int get_int(const basetype &x)
{
  if((x-(int)x)>=0.500)  return (int)x+1;
  else return (int)x;
};

void lltoxy_func(float rrlat,float rrlon,float &xloc,float &yloc,setmapr& a);

void read_in_cttemp(int itime0,float **tb, CONFIG_PARS &pp,int const gmt_m_lcl,
        int &cttemp_data_flag,int read_window, int read_freq,
        char *kname);

void i2wrtvar_cart3d(char *dirname, char *filename,
            int map_scale,
            float nw_lon, float nw_lat,
            int xy_scale, int dxy_scale, int z_scale,
            char *varname,char *varunit,
            int nradars, char radarnam[][5],
            short int *i2var, int var_scale, int imissing,
            CONFIG_PARS &mp, grid_vars &gv,int i_bb_mode, int nz1);

int i2readvar_cart3d(char *vfname, int remap_conus_opt, int &map_scale,
            float &nw_lon,  float &nw_lat,
            int &xy_scale, int &dxy_scale, int &z_scale,
            char *varname, char *varunit,
            int &nradars,  char radarnam[][5],
            short int *i2var,int &var_scale,
            int &imissing,  CONFIG_PARS &mp,  int &istatus,
            int &i_bb_mode);
int read_sat_qc_mask(short int *i2var, CONFIG_PARS &mp, int type );

int chktime (char temp_string[20], int itime, int &itime1, int window, short df);

int get_t_z_snd(string radar_name_snd, string config_file, 
            int nt, float *z_snd,short debug_flag);

void get_time_realtime (CONFIG_PARS &pp,int &gmt_m_lcl, int &itime0);

void t_level_deriv(float ***mref_3d, float ***kh_tlvls, const int itime0,
            CONFIG_PARS &pp, char radarnam[][5], grid_vars &gv,nids_pars &np,
            string config_file_name, char nc_timeString[20],char timeString[20]);

void get_attr_realtime (CONFIG_PARS &pp, int &itime0, int gmt_m_lcl,
                        char timeString[], char nc_timeString[]);

float hgt_to_range(float height,float elvang);

int hgt_to_ranges(float height,float elvang, float &range, float &sfcrng);

void beamelv_func(float height,float sfcrng,float &elvang,float &range);

void disthead_func(float lat1,float lon1,float lat2,
            float lon2,float &headng,float &dist);

void gcircle_func (float lat1, float lon1,
            float head, float dist,
            float& lat2, float& lon2);

void beamhgt_func1(float elvang, float range,float &height,float &sfcrng);

void multiVar_netcdf_wrt(char *dirname, char *filename,
                     char *time_str, const int UTC_time_sec,
                     float nw_lon, float nwlat,
                     char **varname,char **varunit, char **file_name,
                     short int **i2var, int *varscale, int *imissing,
                     CONFIG_PARS &mp, grid_vars &gv_tt,int i_bb_mode, int *nz1,
                     const float *mint,const float *maxt,int num_var,
                     char* depictorName);

void output_mosaic_info(double clock_time,double cpu_time,
                  int nradars,float cref, char* timestamp,CONFIG_PARS &pp);


void produce_CREF_CREFH_new (CONFIG_PARS &pp, float *** ref,
                         float** CREF, float** CREFH );
void produce_ETP_new(CONFIG_PARS &pp, float*** ref, float ** ETP);
void produce_HDA_new(CONFIG_PARS &pp, float*** ref, float ***kh_tlvls,
                 float **shi, float **posh, float ** mehs );
void produce_HSR_and_HSRH_new(CONFIG_PARS &pp, float*** ref,
                  float **HSR, float ** HSRH) ;
void produce_LCREF_and_LCREFH_new(CONFIG_PARS &pp, float*** ref,
                  float** LCREF, float** LCREFH);
void produce_PCPFLAG_new (CONFIG_PARS &pp, float*** ref, float ***kh_tlvls,
                      short **PCPFLAG);
void produce_VIL_and_VILD_new(CONFIG_PARS &pp, float*** ref,
                   float **vil, float **vilD, float **terrain);


void produce_CREF_CREFH (CONFIG_PARS &pp, nids_pars &np, string config_file_name,
                         float*** ref,  short **i2tem, char* timeString,
                         grid_vars &gv, int nradar_has_data, char radarnam[][5]);

void produce_ETP(CONFIG_PARS &pp, float*** ref, int itime0, int nradar_has_data,
                  grid_vars &gv, char* timeString, char radarnam[][5],short *tem);

void produce_HDA(CONFIG_PARS &pp, float*** ref, float ***kh_tlvls, int itime0, 
                 int nradar_has_data, grid_vars &gv, char* timeString,
                 string config_file_name, char radarnam[][5],short **tem);

void produce_HSR_and_HSRH(CONFIG_PARS &pp, float*** ref, int itime0, int nradar_has_data,
                 grid_vars &gv, char* timeString,char radarnam[][5],short **tem);

void produce_VIL_and_VILD(CONFIG_PARS &pp, float*** ref, int itime0, int nradar_has_data,
                 grid_vars &gv, char* timeString,char radarnam[][5],short **tem);

void produce_LCREF_and_LCREFH(CONFIG_PARS &pp, float*** ref, int itime0, int nradar_has_data,
                  grid_vars &gv, char* timeString,char radarnam[][5],short **i2tem2d);

void produce_PCPFLAG (CONFIG_PARS &pp, float*** ref, float ***kh_tlvls, int itime0,  
                      char* timeString, grid_vars &gv, 
                      int nradar_has_data, char radarnam[][5], 
                      string config_file_name, short **i2tem2d);

float *** get_index_temp(int itime0, CONFIG_PARS &pp, char radarnam[][5], 
                         grid_vars &gv, string config_file_name, 
                         char timeString[20]);

void produce_2D_products(char nc_timeString[20], CONFIG_PARS &pp, nids_pars &np,
                         float*** ref, float ***kh_tlvls, int itime0,
                         int nradar_has_data, grid_vars &gv, 
                         char* timeString,
                         string config_file_name, char radarnam[][5]);
int read_netcdf(CONFIG_PARS &pp,short*** data,string inputfile);

vector<string> get_radar_file_list(string source_list,
                                   char radar_name[],
                                   string radar_product,
                                   long newest_epoch, long oldest_epoch);
                                   
int get_current_vcp(char top_dir[], char radar_name[], 
                    char vcp_product[], long epoch_time);
                    
//int apply_bloom_QC(CONFIG_PARS pp, LatLonHeightField* llhF, int vcp, 
//                   float** ruc_tsfc, float tsfc_bloom_threshold,
//                   float no_coverage_value);
                   
float ** read_RUC_tsfc(CONFIG_PARS &pp, char data_path[], 
                        long current_epoch, int max_age, int debug_flag);
                        
#endif
