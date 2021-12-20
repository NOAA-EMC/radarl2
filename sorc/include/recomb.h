
/******************************************************************

    This is the private header file for recomb - the 
    super-resolution radar data recombination program.
	
******************************************************************/

/* RCS info */
/* $Author: jing $ */
/* $Locker:  $ */
/* $Date: 2007/04/13 20:22:09 $ */
/* $Id: recomb.h,v 1.2 2007/04/13 20:22:09 jing Exp $ */
/* $Revision:  */
/* $State: */
#define NAZIM  760
#define NGATE 1000

#ifndef SPRS2NRM_H
#define SPRS2NRM_H

int CR_combine_radials (char *input, char **output, int *length);
void CR_non_indexed_azimuth ();
void CR_use_nssl_algorithm ();
int MAIN_output_radial (char *output, int length);
int ORPGVCP_get_num_elevations( int vcp_num );

/* structure for output the recombined data, added by Guan July, 2008 */
typedef struct {
   char radar_name[8];
   int vcpnum[NAZIM];
   float year[NAZIM];
   float month[NAZIM];
   float day[NAZIM];

   float hour[NAZIM];
   float minute[NAZIM];
   float second[NAZIM];

   float radlat[NAZIM];
   float radlon[NAZIM];
   float radhgt[NAZIM];
   float elev_angle[NAZIM];
   float fstgatdis[NAZIM];
   float nyq_vel[NAZIM];
   int num_beam[NAZIM];
   int num_gate[NAZIM];
   float gateWidth[NAZIM];
   float azim[NAZIM];
   float field[NAZIM][NGATE];
   float gate_dis[NAZIM][NGATE];
} Cout_strct_ncep;

typedef struct {
   char radar_name[8];
   int vcpnum;
   int year;
   int month;
   int day;
   int hour;
   int minute;
   int second;

   float radlat;
   float radlon;
   float radhgt;
   float elev_angle;
   float fstgatdis;
   float nyq_vel;
   int num_beam;
   int num_gate;
   float gateWidth[NAZIM];
   float elev[NAZIM];
   float azim[NAZIM];
   float field[NAZIM][NGATE];
} ccout_strct_ncep_tmp;

int RCDP_get_recombined_dp_fields (char *rad);
int RCDP_dp_recomb (char *rad1, char *rad2);
#endif
int reverse_int4(int n);
void swp_int4(int k, int *n);
short reverse_int2(short n);
void swp_int2(int k, short *n);
float reverse_float(float value);
    union fp_bit_twiddler {
      float f;
      unsigned int i;
    } q;

void swp_float(int k, float *n);
