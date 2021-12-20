/*--------------------------------------------------------------------------
	Program:	DefineAndStruc.h
	Function:	DEFINE and Structure of the verification program of 
			QPEs products
	Author:		Fang Zhao (03/28/2001)
	Modification:	
---------------------------------------------------------------------------*/

// Define ---------------------------------------------------------
#define MAX_PREDIC 288

#define MAX_RADARNUM 10
#define MAX_NZ_DEFINE 1000

// Structure Qpesums_Info : Information of QPEs grid domain -------------
typedef struct{
            int    nx;    
            int    ny;
            float  nw_lat;
            float  nw_lon;
            float  se_lat;
            float  se_lon;
            float  dx;
            float  dy;
            float  miss;
}Qpesums_Info;

// Structure Point : Lat and lon of each grid ----------------------------
typedef struct{
            float  lat;
            float  lon;
}Point;

// Structure Cur_Time : Set current time ---------------------------------
typedef struct{
            int  year;
            int  mon;
            int  day;
            int  hour;
            int  min;
}Cur_Time;

// Structure Ver_Config_Info : Info of verification program config file --
typedef struct{
        int nx;
        int ny;
        int nz;
  	float trulat1;
  	float trulat2;
  	float trulon;
        float del_x;
        float del_y;
        float ctrlat;
        float ctrlon;

	int mapproj;

        int nx_sat;
        int ny_sat;
        int nz_sat;
  	float trulat1_sat;
  	float trulat2_sat;
  	float trulon_sat;
        float del_x_sat;
        float del_y_sat;
        float ctrlat_sat;
        float ctrlon_sat;

	char ms_01h_prod_dir[255];
	char rad_01h_prod_dir[255];
	char ms_24h_prod_dir[255];
	char rad_24h_prod_dir[255];

	char inst_type_ms_dir[255];
	char inst_type_rad_dir[255];

	int  aue_proc_flag;
        char aue_raw_prod_dir[255];
	char aue_prod_dir[255];

	int gauge_qc_flag;
	int gauge_qc_step3_flag;
	int gauge_convert_flag;
	char gauge_raw_dir[255];
	char gauge_dir[255];
	int gauge_interval;
	int gauge_time_lag;

        char nids_info_dir[255];
        char ref_dir[255];

	int verification_flag;

        char ver_output_dir[255];
        char ver_score_daily_dir[255];
        char ver_plot_monthly_dir[255];
        char ver_scattergram_daily_dir[255];

        int ref_time_number;
        int ref_time[5];

        int ver_products_num;
        char ver_products_type[8][15];

	int arcview_flag;

	int web_flag;

	int nids_swap_flag;

	int tgroup_number;

	int pgroup_number;

	int valid_time;
}Ver_Config_Info;

// Structure Gz_Info : Header info of binary files ---------------------
typedef struct{

    int yr;
    int mo;
    int dy;
    int hr;
    int min;
    int sec;

    int nx1;
    int ny1;
    int nz1;

    char projection[5];

    int map_scale;
    int trulat1_temp;
    int trulat2_temp;
    int trulon_temp;

    int nw_lon_temp;
    int nw_lat_temp;
    int xy_scale;

    int dx1_temp;
    int dy1_temp;
    int dxy_scale;

    int z_temp[MAX_NZ_DEFINE];
    int z_scale;

    int temp[10];

    char varname[12];
    char varunit[7];

    int var_scale;
    int imissing;

    int nradars;

    char radarnam[MAX_RADARNUM][5];
}Gz_Info;
