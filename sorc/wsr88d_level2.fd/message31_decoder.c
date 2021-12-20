#include "stdio.h"
#include <orpggdr.h>
#include <string.h>
#include <infr.h>
#include <generic_basedata.h>
#include <bzlib.h>
#include <vcp.h>
#include <orpgevt.h>
#include <pbd.h>
#include "recomb.h"
/* Value for undefined azimuth and elevation. */
#define UNDEFINED_AZI           1.e20
#define UNDEFINED_ELE           1.e20

/* Azimuth incremant tolerance. */
#define DELTA_AZI_TOLERANCE     (2.0f)
#define CENTER_AZI              (0.5f)

/* Elevation Delta tolerance */
#define DELTA_ELV_TOLERANCE     (0.5f)

/* Degrees to Radians conversion. */
#define DEG_TO_RAD              (0.0174533f)

/* Used in the height equation:  (Index of refraction)*(Earth's Radius) */
#define IRRE                    (1.21*6371.0)

/* Used in the height equation: 70,000 ft in km. */
#define TOPHGT_KM               (21.21)

/* Radial count in current elevation. */
static int Radial_cnt = 0;

/* Set in Find_start_and_delta_angles; used for delta azimuth checking */
static float Delta_azimuth;

/* Starting time and date of current volume. */
static int Volume_time;
static int Volume_date;

/* Number of elevations in current volume; must be set before a radial
   is processed; must be less than ECUTMAX. */
static int Number_elevations;

/* Target elevation table. Values are in degrees. */
static float Target_elevation [ECUTMAX];

/* Waveform type of this elevation.  Valid waveforms defined in the
   RDA/RPG ICD. */
static int Waveform_type;
/* The RPG elevation index of this elevation. */
static int Rpg_elevation_index;

/* Used for recording the previous azimuth */
static float Old_azi = UNDEFINED_AZI;

/* Local functions. */
static int Find_start_and_delta_angles( float azi, Base_data_header *rpg_hd,
                                        float half_azimuth_spacing );
static void Read_and_update_current_vcp_table( Vol_stat_gsm_t *vs_gsm );
static void Process_angle_data( Generic_basedata_t *gbd,
                                Base_data_header *rpg_hd );
static void Process_block_headers( Generic_basedata_t *gbd,
                                   Base_data_header *rpg_hd );
static int Process_moment_block_headers( Generic_basedata_t *gbd,
                                         Base_data_header *rpg_hd );

#define BLOCK_SIZE              1208
#define HALF_WORD               2
#define CMAXBEAM                368     /* Maximum beam number after combination */
#define INPUT_DATA_SIZE         76000   /* Maximum size of RDA radial, in bytes */
#define OUT_SIZE                460000
#define LOCAL_SIZE              16920 
#define MAX_RPG_RADIAL_SIZE     16920   /* This size is based on the following
                                           assumptions:

                                           Radial Header - (sizeof(Base_data_header))
                                           8 possible data moments, 3 standard + 5 additional
                                           2 bytes/datum for REF, VEL, SPW, PHI and RHO
                                           1 byte/datum for ZDR, SNR and RFR
                                           460 km range, 250 m gates for SNR and REF = 1840 bins
                                           300 km range, 250 m gates for ZDR, PHI, RHO, VEL and SW = 1200 bins
                                           60 km range, 250 m gates for 240 bins
                                           5 additional data headers - (sizeof(Generic_moment_t)) = 32 bytes

                                             200 - Radial header
                                           16560 - moment data
                                             160 - additional headers
                                           -----
                                           16920

                                        */

#define LOCAL_OFFSET            13
#define ABS(a)  ((a)>0?(a):(-1.0*a))

#ifdef UNDERSCORE
   #define get_radar_data get_radar_data_
#endif

static Vol_stat_gsm_t Vs_gsm;           /* Volume Status Data. */
/* Target elevation table. Values are in degrees. 
static float Target_elevation[ECUTMAX]; */


/* Local Function Prototypes. */
void read_msg31(char *buf, int size, int *pnum_elevation, int wsr88d_type);
void swp_db(Generic_moment_t *db);
static int Process_a_radial( int msg_type, char *rda_msg, char *rpg_basedata );
static int Check_rda_commands();
static void Commanded_restart( int parameter_1 );
static void Init_events();
static void Init_lb_notification();
static void Init_alarms();
static void Init_vsnum_wxmode( );
static void Check_input_buffer_load( int data_id, LB_id_t msg_id, int read_returned );
static int Cleanup_fxn( int signal, int status );
static void Open_lb();
static int Update_current_vcp_table( int vcp_num, Vcp_struct *vcp );
static char* Legacy_to_generic( char *rda_msg );
void convert_time( long timevalue, int *hrs, int *mins, int *secs );
void calendar_date( short date, int *dd, int *dm, int *dy );
void out_to_refvel(Cout_strct_ncep * refvel, Base_data_header * bh, int *dualindex);
void out_missing_value(Cout_strct_ncep * refvel, Base_data_header * bh, int *pscan, int *dualindex, int *pnum_elevation, int *sailsindex);
static void writeout(Cout_strct_ncep *refvel );
void get_radar_data(Cout_strct_ncep *tmp1,Cout_strct_ncep *tmp2, Cout_strct_ncep *tmp3, Cout_strct_ncep *tmp4,Cout_strct_ncep *tmp5, Cout_strct_ncep *tmp6,int * num_elevation, int *dualindex, int *sailsindex);

/*****************************************************************

   Description:
      Decode messgae 31, converts incoming Digital Radar Data format data (Refer to RDA/RPG ICD) 
      to internal RPG base data format, perform the super-resolution radial data recombination.

   Inputs:
      buf - raw data.
      size - size of raw data.

   Returns:
      Structure array - recombined radar data.
      *pnum_elevation - number of elevation in the radar data

******************************************************************/
void read_msg31(char *buf, int size, int *pnum_elevation, int wsr88d_type)
{
   Generic_basedata_t *rec;
   RDA_RPG_message_header_t *rrmsg;
   int msg_type, i, j, k, lscan, lscan2;
   int *pscan;
   int read_size, total_byte, seek_size;
   Generic_any_t *data_block;
   static char rpg_basedata[MAX_RPG_RADIAL_SIZE];
   char str_type[5];
   Base_data_header *bh;
   Generic_moment_t *mom = NULL;
   unsigned char *ref_src_ptr = NULL;
   char *output;
   int length, radial_status; 
   FILE *out2;
   Moment_t *mt;
   int *dualindex, dualv, save, dualscan, *sailsindex, sailsv;
   int ind;
   int sails; 
   static Cout_strct_ncep refvel[6], ref, rho, phi, zdr; 
   static float previous_elev_angle = 0.5;
   float dualangle;
   int year, month, day, hour, minute, second;
   Generic_rad_t *rt;
   Generic_vol_t *vt;
   Generic_elev_t *et;
   Generic_moment_t *db;

#ifdef LITTLE_ENDIAN
   total_byte = 12;
   while (size > total_byte)
   {
/* Read message header */
      rrmsg = (RDA_RPG_message_header_t *) (buf + total_byte);
      rrmsg->size = reverse_int2( rrmsg->size);
      rrmsg->sequence_num = reverse_int2( rrmsg->sequence_num);
      rrmsg->julian_date = reverse_int2( rrmsg->julian_date);
      rrmsg->milliseconds = reverse_int4( rrmsg->milliseconds);
      rrmsg->num_segs = reverse_int2( rrmsg->num_segs);
      rrmsg->seg_num = reverse_int2( rrmsg->seg_num);

      switch (rrmsg->type)
      {
         case 0:
         case 2:
         case 3:
         case 4:
         case 5:
         case 11:
         case 13:
         case 15:
         case 18:
            read_size = rrmsg->num_segs *(BLOCK_SIZE * HALF_WORD + sizeof(RDA_RPG_message_header_t));
            read_size = 2432;
            break;
         case 31:
            read_size = rrmsg->num_segs * rrmsg->size * HALF_WORD;
            break;
         default:
            printf(" Error: No message type %d\n", msg_type);
            exit (0);
      }

      if (rrmsg->type == 31)
      {
         rec = (Generic_basedata_t *) (buf + total_byte);
         rec->base.time = reverse_int4( rec->base.time);
         rec->base.date = reverse_int2( rec->base.date);
         rec->base.azi_num = reverse_int2( rec->base.azi_num);
         rec->base.azimuth = reverse_float( rec->base.azimuth);
         rec->base.radial_length = reverse_int2( rec->base.radial_length);
         rec->base.elevation = reverse_float( rec->base.elevation);
         rec->base.no_of_datum = reverse_int2( rec->base.no_of_datum);
         swp_int4( rec->base.no_of_datum, rec->base.data);
   /*     Find the data block of interest. */
         for( i = 0; i < rec->base.no_of_datum; i++ )
         {

            data_block = (Generic_any_t *)
                       (buf + total_byte + sizeof(RDA_RPG_message_header_t) + rec->base.data[i]);
      /* Convert the name to a string so we can do string compares. */
            memset( str_type, 0, 5 );
            memcpy( str_type, data_block->name, 4 );
            if (strcmp(str_type,"RVOL") == 0)
            {
               vt = ( Generic_vol_t *) data_block;
               vt->len = reverse_int2( vt->len);
               vt->lat = reverse_float( vt->lat);
               vt->lon = reverse_float( vt->lon);
               vt->height = reverse_int2( vt->height);
               vt->feedhorn_height = reverse_int2( vt->feedhorn_height);
               vt->calib_const = reverse_float( vt->calib_const);
               vt->horiz_shv_tx_power = reverse_float( vt->horiz_shv_tx_power);
               vt->vert_shv_tx_power = reverse_float( vt->vert_shv_tx_power);
               vt->sys_diff_refl = reverse_float( vt->sys_diff_refl);
               vt->sys_diff_phase = reverse_float( vt->sys_diff_phase);
               vt->vcp_num = reverse_int2( vt->vcp_num);
            }

            if( strcmp( str_type, "RELV" ) == 0 )
            {
               et = (Generic_elev_t *) data_block;
               et->len = reverse_int2( et->len);
               et->atmos = reverse_int2( et->atmos);
               et->calib_const = reverse_float( et->calib_const);
            }
            if (strcmp(str_type,"RRAD") == 0)
            {
               rt = ( Generic_rad_t *) data_block;
               rt->len = reverse_int2( rt->len);
               rt->unamb_range = reverse_int2( rt->unamb_range);
               rt->horiz_noise = reverse_float( rt->horiz_noise);
               rt->vert_noise = reverse_float( rt->vert_noise);
               rt->nyquist_vel = reverse_int2( rt->nyquist_vel);
            }
            if (strcmp(str_type,"DREF") == 0)
            {
               db = ( Generic_moment_t *) (buf + total_byte + sizeof(RDA_RPG_message_header_t) + rec->base.data[i]);
               swp_db(db);
            }
            if (strcmp(str_type,"DVEL") == 0)
            {
               db = ( Generic_moment_t *) (buf + total_byte + sizeof(RDA_RPG_message_header_t) + rec->base.data[i]);
               swp_db(db);
            }
            if (strcmp(str_type,"DSW ") == 0)
            {
               db = ( Generic_moment_t *) (buf + total_byte + sizeof(RDA_RPG_message_header_t) + rec->base.data[i]);
               swp_db(db);
            }
            if (strcmp(str_type,"DZDR") == 0)
            {
               db = ( Generic_moment_t *) (buf + total_byte + sizeof(RDA_RPG_message_header_t) + rec->base.data[i]);
               swp_db(db);
            }
            if (strcmp(str_type,"DPHI") == 0)
            {
               db = ( Generic_moment_t *) (buf + total_byte + sizeof(RDA_RPG_message_header_t) + rec->base.data[i]);
               swp_db(db);
            }
            if (strcmp(str_type,"DRHO") == 0)
            {
               db = ( Generic_moment_t *) (buf + total_byte + sizeof(RDA_RPG_message_header_t) + rec->base.data[i]);
               swp_db(db);
            }
         }
         total_byte+=LOCAL_OFFSET -1;
      }
      total_byte += read_size;
   }
#endif
    printf("Total %d bytes have been read  %d\n", total_byte, size);

   dualindex = &dualv;
   dualscan = 0;
   sailsv = 0;
   sailsindex = &sailsv;
   save = 0;
   lscan = -1;
   lscan2 = 0;
   pscan = &lscan;
   total_byte = 12;
   while (size > total_byte)
   {
/* Read message header */
/*
      if (buf[total_byte+3] == NULL)
      {
         printf("Total %d bytes have been read \n", total_byte);
         return;
         exit (0);
      }
*/
      rrmsg = (RDA_RPG_message_header_t *) (buf + total_byte);
     
      switch (rrmsg->type)
      {
         case 0:
         case 2:
         case 3:         
         case 4:
         case 5:
         case 11:
         case 13:
         case 15:
         case 18:
            read_size = rrmsg->num_segs *(BLOCK_SIZE * HALF_WORD + sizeof(RDA_RPG_message_header_t));
            read_size = 2432;
            break; 
         case 31:
            read_size = rrmsg->num_segs * rrmsg->size * HALF_WORD;
            break;
         default:
            printf(" Error: No message type %d\n", msg_type);
            exit (0);
      }

      if (rrmsg->type == 31)
      {
         Process_a_radial( rrmsg->type, buf+total_byte, rpg_basedata);
/*         printf("total_byte = %d \n", total_byte);  */
         radial_status = CR_combine_radials( (char *)rpg_basedata , (char **)&output, &length);

         bh = (Base_data_header *) output;
         rec = (Generic_basedata_t *) (buf + total_byte);
         sails = 0;
         if ( length > 0 )
         {
/*    Test whether it is SAILS   */ 
          if ( (bh->vcp_num == 12) || (bh->vcp_num == 212) || (bh->vcp_num == 215) || (bh->vcp_num == 35) )  
          {
            if (abs(bh->elevation-0.5)<0.1 && (Target_elevation[lscan2] - bh->elevation ) > 0.3)
             {
              sails = 14;
              *sailsindex =1;
              //printf("This is SAILS \n");
             }
          }
          if (  bh->azi_num > CMAXBEAM)  bh->azi_num = CMAXBEAM; 
   /*     Find whether it is dual pol */
         *dualindex = 0;
         for( i = 0; i < rec->base.no_of_datum; i++ )
         {

            data_block = (Generic_any_t *)
                       (buf + total_byte + sizeof(RDA_RPG_message_header_t) + rec->base.data[i]);

      /* Convert the name to a string so we can do string compares. */
            memset( str_type, 0, 5 );
            memcpy( str_type, data_block->name, 4 );
           if (strcmp(str_type,"DZDR") == 0)
           {
              *dualindex = 1;
              dualscan = 1;
           }
         }

/* 
           if (wsr88d_type == 4 || wsr88d_type == 7)
           {
              bh->height /= FT_TO_M;
              bh->feedhorn_height /= FT_TO_M;
           } 
*/
         if (bh->status == 3 )
         {
/*          Beginning of Volume Scan. Initial refvel[6] */
            memset( refvel, 0, 6 * sizeof(Cout_strct_ncep));
            for (  i = 0; i < 6; i++)
            {
               for (j = 0; j < NAZIM; j++)
               {
                  refvel[i].year[j] = -999;
                  refvel[i].elev_angle[j] = Target_elevation[lscan2];
               }
            }
/*          
            while [0]: reflectivity; [1]: velocity; [2]: spectrum width   
                  [3]: Rho;          [4]: Phi;      [5]: Zdr */
            *pnum_elevation = 0;
            out_to_refvel(refvel, bh, dualindex);
         }
         else if (bh->status == 0 || bh->status == 1 ||  bh->status == 5)
         {
            out_to_refvel(refvel, bh, dualindex);
         }
         else if (bh->status == 2 )
         {
/*          End of Elevation     */   
            if (sails == 0 ) lscan++;
            out_to_refvel(refvel, bh, dualindex);
/*            printf("test bh->elevation = %f, %f, %d\n",bh->elevation, Target_elevation[lscan], lscan); */
            while ( (bh->elevation - Target_elevation[lscan]) > 0.4)
            {
                out_missing_value(refvel, bh, pscan, &dualscan, pnum_elevation, sailsindex);
                *dualindex = 0;
            }
            if ( ABS(bh->elevation - previous_elev_angle ) > 0.2 )
            {
               printf(" Error in elevation angle -- exit");
               exit (0);
            }
            
            if ((bh->n_dop_bins == 0) && (*dualindex == 1))
            {
/*              save the dual pol data */
                dualangle = bh->elevation;
                memcpy( &ref, &refvel[0], sizeof(Cout_strct_ncep) );
                memcpy( &rho, &refvel[3], sizeof(Cout_strct_ncep) );
                memcpy( &phi, &refvel[4], sizeof(Cout_strct_ncep) );
                memcpy( &zdr, &refvel[5], sizeof(Cout_strct_ncep) );
                save = 1;
                *dualindex = 0;
            }
            /* consider case, bh->n_dop_bins =0, need to ger_radar_data */
            lscan2 = lscan + 1;
            if (lscan2 > Get_num_elevations( bh->vcp_num )) lscan2 = lscan;
            if ((((save == 1) || (bh->n_surv_bins > 0)) && (bh->n_dop_bins > 0)) || 
                ( (sails == 0 ) &&(Target_elevation[lscan2] - Target_elevation[lscan] > 0.4))) 
            {
               if ( (*dualindex == 0) &&  ABS(bh->elevation - dualangle ) < 0.3 )
               {
                   memcpy( &refvel[0], &ref, sizeof(Cout_strct_ncep) );
                   memcpy( &refvel[3], &rho, sizeof(Cout_strct_ncep) );
                   memcpy( &refvel[4], &phi, sizeof(Cout_strct_ncep) );
                   memcpy( &refvel[5], &zdr, sizeof(Cout_strct_ncep) );
                   *dualindex = 1;
                   save = 0;
               }
               if ( ((*dualindex == 1) || (dualscan == 0 ) ) && (((Target_elevation[lscan] -bh->elevation ) < 0.4) || (sails != 0 ))) 
               {
                  printf("Output data at %f elevation at %d level \n", bh->elevation, *pnum_elevation + 1);
                if ( (sails != 0 ))
                {
                  get_radar_data(&refvel[0], &refvel[1], &refvel[2], &refvel[3], &refvel[4], &refvel[5], &sails, &dualscan, sailsindex);
                }
                else
                {
                  get_radar_data(&refvel[0], &refvel[1], &refvel[2], &refvel[3], &refvel[4], &refvel[5], pnum_elevation, &dualscan, sailsindex);
                }
                  *dualindex = 0;
                  memset( refvel, 0, 6 * sizeof(Cout_strct_ncep));
                  for (  i = 0; i < 6; i++)
                  {
                     for (j = 0; j < NAZIM; j++)
                     {
                        refvel[i].year[j] = -999;
                        refvel[i].elev_angle[j] = Target_elevation[lscan2];
                     }
                  }
                  if (sails == 0 ) *pnum_elevation = *pnum_elevation + 1;
               }
             }
             if (((Target_elevation[lscan] -bh->elevation ) > 0.4) && (sails == 0 ) ) lscan--;

/*
                if (bh->elevation < 0.90 && bh->elevation > 0.7 )
                {
                   printf("check %f, %d \n", refvel[0].elev_angle[0], *pnum_elevation);
                    writeout(refvel );
                }
*/
         }
         else if (bh->status == 4 )
         {
/*          End of Volume Scan    */
/*          Check data blocks    */
/*          output the refvel    */
            lscan++;
            out_to_refvel(refvel, bh, dualindex);
            printf("Output data at %f elevation at %d level \n", bh->elevation, *pnum_elevation + 1);
            get_radar_data(&refvel[0], &refvel[1], &refvel[2], &refvel[3], &refvel[4], &refvel[5], pnum_elevation, &dualscan, sailsindex);
            printf("pnum_elevation2 = %d\n", *pnum_elevation);
            *pnum_elevation =  *pnum_elevation +1;
            lscan++;
            while (*pnum_elevation < Get_num_elevations( bh->vcp_num ))
            {
                out_missing_value(refvel, bh, pscan, &dualscan, pnum_elevation, sailsindex);
            }
            return;
         }

         previous_elev_angle = bh->elevation;


/*
         if (rec->base.elev_num == 2 && rec->base.azi_num == 1)
         if (bh->elev_num == 2 && bh->azi_num == 1)
         {
            out2 = fopen("test.txt","w") ;
            mom = (Generic_moment_t *) ORPGGDR_get_data_block( (char *) rec, ORPGGDR_DREF );
            if (mom == NULL) fprintf(out2, "error\n");
             fprintf(out2, "mom->no_of_gates = %d\n",mom->no_of_gates);
            ref_src_ptr = (unsigned char *) &mom->gate.b[0];
            for( i = 0; i < mom->no_of_gates; i++ )
            for( i = 0; i < 1840; i++ )
            {
               mt = (Moment_t * ) (rpg_basedata + bh->ref_offset + sizeof (Moment_t) * i); 
               fprintf(out2, "i = %d, val = %hd\n", i, *mt);
            }
            fclose(out2);
         }
         printf("rec->base.status %hd\n", bh->status);
         printf("rec->base.elev_num %hd\n", bh->elev_num);
         printf("rec->base.azimuth_index %hd\n", bh->azm_index);
      
         printf(" rec->base.azi_num= %d\n" , bh->azi_num);
         printf(" length= %d\n" ,length);
         printf(" bh->vel_offset= %d\n" , bh->vel_offset);
         printf(" bh->n_dop_bins= %d\n" , bh->n_dop_bins);
         printf(" bh->ref_offset= %d\n" , bh->ref_offset);
         printf(" bh->n_surv_bins= %d\n" , bh->n_surv_bins);
         printf(" bh->spw_offset= %d\n" , bh->spw_offset);
         printf(" bh->surv_bin_size= %d\n" , bh->surv_bin_size);
         printf("rec->base.elevation %f\n", bh->elevation);
         printf("pnum_elevation = %d\n", *pnum_elevation);
*/

   /* Find the data block of interest. 
         for( i = 0; i < rec->base.no_of_datum; i++ )
         {

            printf("rec->base.data[i]=%d, %d\n", rec->base.data[i],i);
            data_block = (Generic_any_t *)
                       (buf + total_byte + sizeof(RDA_RPG_message_header_t) + rec->base.data[i]);
   */

      /* Convert the name to a string so we can do string compares. 
            memset( str_type, 0, 5 );
            memcpy( str_type, data_block->name, 4 );
            printf(" %s \n",str_type);
         }
      */
        }
         total_byte+=LOCAL_OFFSET -1;
      }      
      total_byte += read_size;
   }
   if (*pnum_elevation < Get_num_elevations( bh->vcp_num ))
   {
       printf(" Error: input data is incomplete -- exit");
       exit (0);
   }

   return ;
}

/**
**/
long read_contine(char *buf, long *beg_size, long *end_size, FILE *in)
{
   static char buf1[LOCAL_SIZE]; 
   long beg_long, end_long, read_size, i;
   
   beg_long = ftell(in);
   
   if (fgets (buf1,*end_size, in) != NULL)
   {
      end_long = ftell(in);
      read_size = end_long - beg_long;
/*      printf(" endc ftell = %ld\n", read_size); */

      for (i=0; i<*end_size;i++)
         buf[*beg_size+i] = buf1[i]; 
      *beg_size += read_size;
      return i; 
   }
   return -1;
}


/*******************************************************************

   Description:   
      This function is the controlling module for the generation of 
      RPG base data structure from the RDA radial data. 

      This function also controls the generation of scan summary 
      data and the ORPGEVT_SCAN_INFO event.

      This function posts the ORPGEVT_START_OF_VOLUME event when 
      detected.  It delays sending radials through the RPG whenever 
      the weather mode changes or the VCP changes.  This allows the 
      routine product generator time to build the master product 
      generation list for the upcoming volume scan.

      This function also posts the ORPGEVT_END_OF_VOLUME event when 
      detected and when there is an unexpected start of volume.

   Inputs:        
      msg_type - type of message.
      rda_msg - RDA digital radar data.

   Outputs:       
      rpg_basedata - RPG base data message.

   Returns:       
      It returns 0 on success or -1 on failure.

*********************************************************************/
static int Process_a_radial( int msg_type, char *rda_msg, 
                             char *rpg_basedata ){

    int i, waveform_type, return_value;
    int vol_aborted, unexpected_bov, rpg_num_elev_cuts;
    time_t current_time = 0, previous_time = 0, wait_time;

    static orpgevt_scan_info_t scan_data;

    Generic_basedata_t *gbd = (Generic_basedata_t *) rda_msg;
    Base_data_header *rpg_hd = (Base_data_header *) rpg_basedata;

    if (gbd->base.status == 5) gbd->base.status = 0;

    /* Initialize the azimuth resolution to 1 deg radials (the default
       value) and the maximum number of radials.  If the azimuth resolution 
       is different than the default, it and the maximum number of radials
       will be specified in the Generic Radial Header (Message 31). */
    PBD_max_num_radials = LR_MAXN_RADIALS;
    if( gbd->base.azimuth_res == HALF_DEG_RADIALS )
       PBD_max_num_radials = HR_MAXN_RADIALS;

    /* Construct the RPG base data header. */
    if( PH_process_header( gbd, rpg_basedata ) < 0 )
	return (-1);

    /* Move digital radar data to the RPG basedata message. */
    if( PD_move_data( gbd, rpg_basedata ) < 0 )
	return (-1);

    return (0);

/* End of Process_a_radial() */
}

/****************************************************************************/

void convert_time( long timevalue,
                   int *hrs,
                   int *mins,
                   int *secs ){

   /*
     Extract the number of hours.
   */
   *hrs = timevalue/3600000;

   /*
     Extract the number of minutes.
   */
   timevalue = timevalue - *hrs*3600000;
   *mins = timevalue/60000;

   /*
     Extract the number of seconds.
   */
   *secs = timevalue - *mins*60000;
   *secs = *secs/1000;

   return;
}

void calendar_date( short date, int *dd, int *dm, int *dy ){

   int l,n, julian;

   /* Convert modified julian to type integer */
   julian = date;

   /* Convert modified julian to year/month/day */
   julian += 2440587;
   l = julian + 68569;
   n = 4*l/146097;
   l = l -  (146097*n + 3)/4;
   *dy = 4000*(l+1)/1461001;
   l = l - 1461*(*dy)/4 + 31;
   *dm = 80*l/2447;
   *dd= l -2447*(*dm)/80;
   l = *dm/11;
   *dm = *dm+ 2 - 12*l;
   *dy = 100*(n - 49) + *dy + l;

   return;
}

void out_to_refvel(Cout_strct_ncep *refvel, Base_data_header * bh, int *dualindex)
{
   int i, j, k;
   int year, month, day, hour, minute, second;
   Moment_t *mt;
   char * pc;
   Generic_moment_t *gt;
   float vmult, vadd;

   i = bh->azi_num - 1;
   if ( bh->n_surv_bins > 0) 
   {
/*   Reflectivity field exits and it is not the additional reflectivity fields for Doppler recombination. */ 
      strcpy(refvel[0].radar_name, bh->radar_name); 
      refvel[0].vcpnum[i] = bh->vcp_num;

      calendar_date( bh->date, &day, &month, &year );
      refvel[0].year[i] = (float) year;
      refvel[0].month[i] = (float) month;
      refvel[0].day[i] = (float) day;

      convert_time( bh->time, &hour, &minute, &second); 
      refvel[0].hour[i] = (float) hour; 
      refvel[0].minute[i] = (float) minute;
      refvel[0].second[i] = (float) second;

      refvel[0].radlat[i] = bh->latitude;
      refvel[0].radlon[i] = bh->longitude;
      refvel[0].radhgt[i] = bh->height + bh->feedhorn_height;
      refvel[0].elev_angle[i] = bh->elevation;
      refvel[0].fstgatdis[i] = bh->range_beg_surv + bh->surv_bin_size/2;
      refvel[0].nyq_vel[i] = 0.01 * bh->nyquist_vel;
      refvel[0].num_beam[i] = bh->azi_num;
      if (bh->n_surv_bins > NGATE) bh->n_surv_bins = NGATE;
      refvel[0].num_gate[i] = bh->n_surv_bins;
      refvel[0].gateWidth[i] = bh->surv_bin_size;
      refvel[0].azim[i] = bh->azimuth;
      if ( refvel[0].azim[i] < 0 ) refvel[0].azim[i] += 360;
      if ( refvel[0].azim[i] > 360 ) refvel[0].azim[i] -= 360;
      pc = (char *) bh;
      mt = (Moment_t * ) (pc + bh->ref_offset );
      for (j = 0; j < bh->n_surv_bins; j++)
      {
         refvel[0].field[i][j] = 999.0;
         refvel[0].gate_dis[i][j] = 0.0;
         if (*mt == 0 || *mt == 1)
         {
         if ( j == 0 )
         {
            refvel[0].field[i][j] = 999.0;
            refvel[0].gate_dis[i][j] = 0.0;
         }
         }
         else
         {
            refvel[0].field[i][j] = (*mt - 2.0)/2.0 - 32.0; 
            refvel[0].gate_dis[i][j] = j * 1000.0;
         }
         mt++;
      }
   }
   if ( bh->n_dop_bins > 0 )
   {
/*   Velocity field exits */
      strcpy(refvel[1].radar_name, bh->radar_name);
      refvel[1].vcpnum[i] = bh->vcp_num;

      calendar_date( bh->date, &day, &month, &year );
      refvel[1].year[i] = (float) year;
      refvel[1].month[i] = (float) month;
      refvel[1].day[i] = (float) day;

      convert_time( bh->time, &hour, &minute, &second);
      refvel[1].hour[i] = (float) hour;
      refvel[1].minute[i] = (float) minute;
      refvel[1].second[i] = (float) second;

      refvel[1].radlat[i] = bh->latitude;
      refvel[1].radlon[i] = bh->longitude;
      refvel[1].radhgt[i] = bh->height + bh->feedhorn_height;
      refvel[1].elev_angle[i] = bh->elevation;
      refvel[1].fstgatdis[i] = bh->range_beg_dop + bh->dop_bin_size/2;
      refvel[1].nyq_vel[i] = 0.01 * bh->nyquist_vel;
      refvel[1].num_beam[i] = bh->azi_num;
      if (bh->n_dop_bins > NGATE) bh->n_dop_bins = NGATE;
      refvel[1].num_gate[i] = bh->n_dop_bins;
      refvel[1].gateWidth[i] = bh->dop_bin_size;
      refvel[1].azim[i] = bh->azimuth;
      if ( refvel[1].azim[i] < 0 ) refvel[1].azim[i] += 360;
      if ( refvel[1].azim[i] > 360 ) refvel[1].azim[i] -= 360;
      vmult = 0.;
      vadd = 999.0;
      if (bh->dop_resolution == 1)
      {
         vmult = 0.5;
         vadd = -63.5;
      }
      if (bh->dop_resolution == 2)
      {
         vmult = 1.0;
         vadd = -127;
      }
      pc = (char *) bh;
      mt = (Moment_t * ) (pc + bh->vel_offset );
      if (bh->n_dop_bins > 1000) bh->n_dop_bins = 1000;
      for (j = 0; j < bh->n_dop_bins; j++)
      {
         refvel[1].field[i][j] = 999.0;
         refvel[1].gate_dis[i][j] = 0.0;
         if (*mt == 0 || *mt == 1)
         {
         if ( j <= 1 )
         {
            refvel[1].field[i][j] = 999.0;
            refvel[1].gate_dis[i][j] = 0.0;
         }
         }
         else
         {
            refvel[1].field[i][j] = (*mt - 2.0)*vmult + vadd;
            refvel[1].gate_dis[i][j] = (float) (2*j - 4);
         }
         mt++;
      }
   }
   if ( bh->n_dop_bins > 0 )
   {
/*    Doppler spectrum width next */
      strcpy(refvel[2].radar_name, bh->radar_name);
      refvel[2].vcpnum[i] = bh->vcp_num;

      calendar_date( bh->date, &day, &month, &year );
      refvel[2].year[i] = (float) year;
      refvel[2].month[i] = (float) month;
      refvel[2].day[i] = (float) day;

      convert_time( bh->time, &hour, &minute, &second);
      refvel[2].hour[i] = (float) hour;
      refvel[2].minute[i] = (float) minute;
      refvel[2].second[i] = (float) second;

      refvel[2].radlat[i] = bh->latitude;
      refvel[2].radlon[i] = bh->longitude;
      refvel[2].radhgt[i] = bh->height + bh->feedhorn_height;
      refvel[2].elev_angle[i] = bh->elevation;
      refvel[2].fstgatdis[i] = bh->range_beg_dop + bh->dop_bin_size/2;
      refvel[2].nyq_vel[i] = 0.01 * bh->nyquist_vel;
      refvel[2].num_beam[i] = bh->azi_num;
      if (bh->n_dop_bins > NGATE) bh->n_dop_bins = NGATE;
      refvel[2].num_gate[i] = bh->n_dop_bins;
      refvel[2].gateWidth[i] = bh->dop_bin_size;
      refvel[2].azim[i] = bh->azimuth;
      if ( refvel[2].azim[i] < 0 ) refvel[2].azim[i] += 360;
      if ( refvel[2].azim[i] > 360 ) refvel[2].azim[i] -= 360;

      pc = (char *) bh;
      mt = (Moment_t * ) (pc + bh->spw_offset );
      if (bh->n_dop_bins > 1000) bh->n_dop_bins = 1000;
      for (j = 0; j < bh->n_dop_bins; j++)
      {
         refvel[2].field[i][j] = 999.0;
         refvel[2].gate_dis[i][j] = 0.0;
         if (*mt == 0 || *mt == 1)
         {
         if ( j <= 1 )
         {
            refvel[2].field[i][j] = 999.0;
            refvel[2].gate_dis[i][j] = 0.0;
         }
         }
         else
         {
            refvel[2].field[i][j] = (*mt - 2.0)/2.0 -63.5;
            refvel[2].gate_dis[i][j] = (float) (2*j - 4);
         }
         mt++;
      }
   }
   if (*dualindex == 1) 
   {
   for (k = 3; k < 6; k++)
   {
      pc = (char *) bh;
      gt = (Generic_moment_t * ) (pc + bh->offsets[k-3] );
      if (gt->no_of_gates > 0)
      {
         if (gt->no_of_gates > 920) gt->no_of_gates = 920;
         strcpy(refvel[k].radar_name, bh->radar_name);
         refvel[k].vcpnum[i] = bh->vcp_num;

         calendar_date( bh->date, &day, &month, &year );
         refvel[k].year[i] = (float) year;
         refvel[k].month[i] = (float) month;
         refvel[k].day[i] = (float) day;

         convert_time( bh->time, &hour, &minute, &second);
         refvel[k].hour[i] = (float) hour;
         refvel[k].minute[i] = (float) minute;
         refvel[k].second[i] = (float) second;

         refvel[k].radlat[i] = bh->latitude;
         refvel[k].radlon[i] = bh->longitude;
         refvel[k].radhgt[i] = bh->height + bh->feedhorn_height;
         refvel[k].elev_angle[i] = bh->elevation;
         refvel[k].fstgatdis[i] = gt->first_gate_range ;
         refvel[k].nyq_vel[i] = 0.01 * bh->nyquist_vel;
         refvel[k].num_beam[i] = bh->azi_num;
         if (gt->no_of_gates > NGATE) gt->no_of_gates = NGATE;
         refvel[k].num_gate[i] = gt->no_of_gates;
         refvel[k].gateWidth[i] = gt->bin_size;
         refvel[k].azim[i] = bh->azimuth;
         if ( refvel[k].azim[i] < 0 ) refvel[k].azim[i] += 360;
         if ( refvel[k].azim[i] > 360 ) refvel[k].azim[i] -= 360;


         if ((gt->data_word_size == 8) && (gt->scale != 0.0))
         {
            for (j = 0; j < gt->no_of_gates; j++)
            {
                if (gt->gate.b[j] == 0 || gt->gate.b[j] == 1)
                {
                    refvel[k].field[i][j] = 999.0;
                    refvel[k].gate_dis[i][j] = 0.0;
                }
                else
                { 
                    refvel[k].field[i][j] = ( gt->gate.b[j] -  gt->offset)/gt->scale;
                    refvel[k].gate_dis[i][j] = j * gt->bin_size + gt->first_gate_range;
                }
            }
         }
         if ((gt->data_word_size == 16) && (gt->scale != 0.0))
         {
            for (j = 0; j < gt->no_of_gates; j++)
            {
                if (gt->gate.u_s[j] == 0 || gt->gate.u_s[j] == 1)
                {
                    refvel[k].field[i][j] = 999.0;
                    refvel[k].gate_dis[i][j] = 0.0;
                }
                else
                {
                    refvel[k].field[i][j] = ( gt->gate.u_s[j] -  gt->offset)/gt->scale;
                    refvel[k].gate_dis[i][j] = j * gt->bin_size + gt->first_gate_range;
                }
            }
         }


      }
   }
   }

   return;
} 

/****************************************************************************/
static void writeout(Cout_strct_ncep *refvel )
{
   FILE *outr, *outv;
   Cout_strct_ncep *bh;
   float *pf;
   int na, i, j;
   bh = &refvel[0];


   outr = fopen ("reflectivity", "w");
   na = 0;

   fprintf(outr,"%s \n", bh->radar_name);
   fprintf(outr,"%8d \n", bh->vcpnum[0]);
   fprintf(outr,"%8.0f%8.0f%8.0f%8.0f%8.0f%8.0f \n",bh->year[0], bh->month[0], bh->day[0], bh->hour[0], bh->minute[0] , bh->second[0]);
/*
   fprintf(outr,"%8d%8d%8d%8d%8d%8d \n",bh->year[0], bh->month[0], bh->day[0], bh->hour[0], bh->minute[0] , bh->second[0]);
*/
   fprintf(outr,"%10.3f%10.3f%10.1f \n",bh->radlat[0], bh->radlon[0], bh->radhgt[0]);
   fprintf(outr,"%8.1f%8.1f \n", bh->fstgatdis[100], bh->gateWidth[100]);
   fprintf(outr,"%8.3f \n", bh->elev_angle[0]);
   for( i = 0; i < NAZIM; i++ )
   {
      if (bh->num_beam[i] > na ) na = bh->num_beam[i];
   }
   fprintf(outr,"%8d%8d \n", na, bh->num_gate[0]);
   fprintf(outr,"%6.1f \n", bh->nyq_vel[0]);
   pf = & bh->azim[0];
   for (i =0; i < na; i+=15, pf+=15)
   {
      fprintf(outr,"%6.1f%6.1f%6.1f%6.1f%6.1f%6.1f%6.1f%6.1f%6.1f%6.1f%6.1f%6.1f%6.1f%6.1f%6.1f\n",*pf, pf[1],pf[2],pf[3],pf[4],pf[5],pf[6],pf[7],pf[8],pf[9],pf[10],pf[11],pf[12],pf[13],pf[14]);
   }
   for (i =0; i < na; i++)
   {
      pf = & bh->field[i][0];
      for (j =0; j < bh->num_gate[0]; j+=20, pf+=20)
        fprintf(outr,"%6.1f%6.1f%6.1f%6.1f%6.1f%6.1f%6.1f%6.1f%6.1f%6.1f%6.1f%6.1f%6.1f%6.1f%6.1f%6.1f%6.1f%6.1f%6.1f%6.1f\n",*pf, pf[1],pf[2],pf[3],pf[4],pf[5],pf[6],pf[7],pf[8],pf[9],pf[10],pf[11],pf[12],pf[13],pf[14],pf[15],pf[16],pf[17],pf[18],pf[19]);
   }
   fclose(outr);
   return;
}

int ORPGVCP_get_num_elevations( int vcp_num ){
   int Number_elevations = -1;
   switch (vcp_num)
   {
      case 11:
      case 211:
         Number_elevations = 16;
         break;
      case 21:
      case 221:
         Number_elevations = 11;
         break;
      case 31:
         Number_elevations = 8;
         break;
      case 32:
         Number_elevations = 7;
         break;
      case 12:
      case 212:
         Number_elevations = 17;
         break;
      case 121:
         Number_elevations = 20;
         break;
      case 35:
         Number_elevations = 14;
      case 215:
         Number_elevations = 14;
         break;
   }
   return Number_elevations; 
}

/* eleminate the scan with the same elevation angle */
int Get_num_elevations( int vcp_num ){
   int Number_elevations = -1;
   switch (vcp_num)
   {
      case 11:
      case 211:
         Number_elevations = 14;
         break;
      case 21:
      case 221:
         Number_elevations = 9;
         break;
      case 31:
      case 32:
         Number_elevations = 5;
         break;
      case 12:
      case 212:
         Number_elevations = 14;
         break;
      case 121:
         Number_elevations = 9;
         break;
      case 35:
         Number_elevations = 14;
         break;
      case 215:
         Number_elevations = 14;
         break;
   }
   return Number_elevations;
}

/*******************************************************************

      Description:
	Function setting up the RPG base data header. 

      Inputs:
        gbd - RDA radial data message.
 
      Outputs:
        rpg_basedata - RPG base data buffer.

      Returns:
	This function returns 0 on success or -1 on failure.

*******************************************************************/
int PH_process_header( Generic_basedata_t *gbd, char *rpg_basedata ){        

   Base_data_header *rpg_hd;
   Generic_vol_t *vol_hdr = NULL;
   float ftmp;

   rpg_hd = (Base_data_header *) rpg_basedata;

   /* Find VCP */
   vol_hdr = (Generic_vol_t *) ORPGGDR_get_data_block( (char *) gbd, ORPGGDR_RVOL );

  /* VCP = 32 */
   Number_elevations = 7;
   Target_elevation[0] =  0.5;
   Target_elevation[1] =  0.5;
   Target_elevation[2] =  1.5;
   Target_elevation[3] =  1.5;
   Target_elevation[4] =  2.5;
   Target_elevation[5] =  3.5;
   Target_elevation[6] =  4.5;   
   rpg_hd->weather_mode = 1;

   if (vol_hdr->vcp_num == 31 )
   {
      Number_elevations = 8;
      Target_elevation[0] =  0.5;
      Target_elevation[1] =  0.5;
      Target_elevation[2] =  1.5;
      Target_elevation[3] =  1.5;
      Target_elevation[4] =  2.5;
      Target_elevation[5] =  2.5;
      Target_elevation[6] =  3.5;
      Target_elevation[7] =  4.5;
      rpg_hd->weather_mode = 1;
   }

   if (vol_hdr->vcp_num == 11 || vol_hdr->vcp_num == 211)
   {
      Number_elevations = 16;
      Target_elevation[2] =  1.45;
      Target_elevation[3] =  1.45;
      Target_elevation[4] =  2.4;
      Target_elevation[5] =  3.35;
      Target_elevation[6] =  4.3;
      Target_elevation[7] =  5.25;
      Target_elevation[8] =  6.2;
      Target_elevation[9] =  7.5;
      Target_elevation[10] =  8.7;
      Target_elevation[11] =  10.0;
      Target_elevation[12] =  12.0;
      Target_elevation[13] =  14.0;
      Target_elevation[14] =  16.7;
      Target_elevation[15] =  19.5;
      rpg_hd->weather_mode = 2;
   }
   
   if (vol_hdr->vcp_num == 21 || vol_hdr->vcp_num == 221 )
   {
      Number_elevations = 11;
      Target_elevation[2] =  1.45;
      Target_elevation[3] =  1.45;
      Target_elevation[4] =  2.4;
      Target_elevation[5] =  3.35;
      Target_elevation[6] =  4.3;
      Target_elevation[7] =  6.0;
      Target_elevation[8] =  9.9;
      Target_elevation[9] =  14.6;
      Target_elevation[10] = 19.5;
      rpg_hd->weather_mode = 2;
   }

   if (vol_hdr->vcp_num == 12 || vol_hdr->vcp_num == 212)
   {
      Number_elevations = 17;
      Target_elevation[2] =  0.9;
      Target_elevation[3] =  0.9;
      Target_elevation[4] =  1.3;
      Target_elevation[5] =  1.3;
      Target_elevation[6] =  1.8;
      Target_elevation[7] =  2.4;
      Target_elevation[8] =  3.1;
      Target_elevation[9] =  4.0;
      Target_elevation[10] =  5.1;
      Target_elevation[11] =  6.4;
      Target_elevation[12] =  8.0;
      Target_elevation[13] =  10.0;
      Target_elevation[14] =  12.5;
      Target_elevation[15] =  15.6;
      Target_elevation[16] =  19.5;
      rpg_hd->weather_mode = 2;
   }

   if (vol_hdr->vcp_num == 121)
   {
      Number_elevations = 20;
      Target_elevation[2] =  0.5;
      Target_elevation[3] =  0.5;
      Target_elevation[4] =  1.45;
      Target_elevation[5] =  1.45;
      Target_elevation[6] =  1.45;
      Target_elevation[7] =  1.45;
      Target_elevation[8] =  2.4;
      Target_elevation[9] =  2.4;
      Target_elevation[10] =  2.4;
      Target_elevation[11] =  3.35;
      Target_elevation[12] =  3.35;
      Target_elevation[13] =  3.35;
      Target_elevation[14] =  4.3;
      Target_elevation[15] =  4.3;
      Target_elevation[16] =  6.0;
      Target_elevation[17] =  9.9;
      Target_elevation[18] =  14.6;
      Target_elevation[19] =  19.5;
      rpg_hd->weather_mode = 2;
   }

   if (vol_hdr->vcp_num == 35)
   {  
      Number_elevations = 17;
      Target_elevation[2] =  0.9;
      Target_elevation[3] =  0.9;
      Target_elevation[4] =  1.3;
      Target_elevation[5] =  1.3;
      Target_elevation[6] =  1.8;
      Target_elevation[7] =  2.4;
      Target_elevation[8] =  3.1;
      Target_elevation[9] =  4.0;
      Target_elevation[10] =  5.1;
      Target_elevation[11] =  6.4;
      Target_elevation[12] =  8.0;
      Target_elevation[13] =  10.0;
      Target_elevation[14] =  12.0;
      Target_elevation[15] =  14.0;
      Target_elevation[16] =  19.5;
      rpg_hd->weather_mode = 2;
   }

   if (vol_hdr->vcp_num == 215)
   {
      Number_elevations = 17;
      Target_elevation[2] =  0.9;
      Target_elevation[3] =  0.9;
      Target_elevation[4] =  1.3;
      Target_elevation[5] =  1.3;
      Target_elevation[6] =  1.8;
      Target_elevation[7] =  2.4;
      Target_elevation[8] =  3.1;
      Target_elevation[9] =  4.0;
      Target_elevation[10] =  5.1;
      Target_elevation[11] =  6.4;
      Target_elevation[12] =  8.0;
      Target_elevation[13] =  10.0;
      Target_elevation[14] =  12.0;
      Target_elevation[15] =  14.0;
      Target_elevation[16] =  19.5;
      rpg_hd->weather_mode = 2;
   }
   switch (vol_hdr->vcp_num)
   {
      case 11:
      case 21:
      case 31:
      case 32:
      case 12:
      case 121:
      case 211:
      case 212:
      case 221:
      case 35:
      case 215:
         break;
      default:
         printf(" Error: No VCP %d\n", vol_hdr->vcp_num);
         exit (0);
   }

   Info.rpg_surv_bin_off = 0;
   Info.rda_surv_bin_off = 0;
   Info.rpg_dop_bin_off = 0;
   Info.rda_dop_bin_off = 0;

   /* Setup the radial count and reset azimuth number at beginning of 
      elevation/volume. */
   if( (gbd->base.status == GOODBEL) || (gbd->base.status == GOODBVOL) ){

      Radial_cnt = 0;
      rpg_hd->sc_azi_num = gbd->base.azi_num;
      rpg_hd->azi_num = 0;

   }

   /* Increment the radial count. */
   Radial_cnt++;

   /* A new volume starts */
   if( (gbd->base.status == GOODBVOL) && (gbd->base.elev_num == 1) ){
      
      /* Set volume scan start time/date.  */
      Volume_time = gbd->base.time;
      Volume_date = gbd->base.date;
   }  

   /* Set the radar name. */
   memcpy( rpg_hd->radar_name, gbd->base.radar_id, 4 );

   /* Increment the azimuth number. This number is used for data
      sequencing checks. */
   rpg_hd->sc_azi_num = gbd->base.azi_num;
   rpg_hd->azi_num++;

   /* Set the azimuth resolution and azimuth index value. */
   rpg_hd->azm_reso = gbd->base.azimuth_res;
   rpg_hd->azm_index = gbd->base.azimuth_index;

   /* Initialize the message type field and initialize the message size. */
   rpg_hd->msg_type = 0;
   rpg_hd->msg_len = (sizeof(Base_data_header) + 1)/sizeof(short);

   /* Radial collection date/time. */
   rpg_hd->time = gbd->base.time;
   rpg_hd->date = gbd->base.date;

   /* Volume begin time (sec past midnight) and volume begin date
      (Modified Julian) .*/
   rpg_hd->begin_vol_time = Volume_time;
   rpg_hd->begin_vol_date = (unsigned short) Volume_date; 

   /* Radial status; must be assigned earlier to be used later. */
   rpg_hd->status = gbd->base.status;

   /* Assign RDA elevation number.  This is the cut sequence number
      of the VCP. */
   rpg_hd->elev_num = gbd->base.elev_num;

   /* Set the number of moments (additional items other than the standard
      three moments) to 0.  This value will be set when the data is moved. */
   rpg_hd->no_moments = 0;

   /* Process elevation/azimuth data. */
   Process_angle_data( gbd, rpg_hd );

   /* The elevation number must be greater than 0 and less than or equal the
      number expected. */
   if( (rpg_hd->elev_num == 0)
                || 
       (rpg_hd->elev_num > Number_elevations) ){

      /* Send status information.  Do not process this radial. */
      LE_send_msg (GL_STATUS | LE_RPG_WARN_STATUS, "Unexpected Number Of Elevations (%d)\n", 
                   rpg_hd->elev_num);
      return (-1);

   }

   /* Set last elevation flag if last elevation cut of the VCP. */
   if( rpg_hd->elev_num == Number_elevations)
      rpg_hd->last_ele_flag = 1;

   else
      rpg_hd->last_ele_flag = 0;

   /* Set the RPG elevation index and target elevation.  Note the target
      elevation is a scaled integer (i.e., ang*10).  The RPG elevation
      index treats split cuts as same elevation cut. */
   rpg_hd->rpg_elev_ind = Rpg_elevation_index;
   ftmp = 10. * Target_elevation [rpg_hd->elev_num - 1];
   rpg_hd->target_elev = Round (ftmp);

   /* Volume scan sequence number, 1-PBD_MAX_SCANS and quotient of the volume 
      sequence number (monotonically increasing sequence number). */
   rpg_hd->volume_scan_num = PBD_volume_scan_number;
   rpg_hd->vol_num_quotient = PBD_volume_seq_number/PBD_MAX_SCANS;

   /* Weather mode. Either Precipitation mode, Clear Air mode, or
      Maintenance mode.  */

   /* PRF sector number and calibration constant. */
   rpg_hd->sector_num = gbd->base.sector_num;

   /* Populate fields in the RPG radial header taken from the Generic_rad_t 
      data structure, the Generic_elev_t data structure and the Generic_vol_t
      data structure. */
   Process_block_headers( gbd, rpg_hd );

   /* Process moment block headers. */
   if( Process_moment_block_headers( gbd, rpg_hd ) < 0 )
      return (-1);

   /* Spot blank flag and bitmap. */
   rpg_hd->spot_blank_flag = gbd->base.spot_blank_flag;
   if ((rpg_hd->spot_blank_flag & SPOT_BLANK_RADIAL) != 0)
      rpg_hd->n_surv_bins = rpg_hd->n_dop_bins = 0;

   /* Bit ordering is from left to right .... bit 0 is LSB.  Bit 1
      corresponds to elevation index 1, bit 2 elevation index 2, etc. */
   if ((rpg_hd->spot_blank_flag & 
       (SPOT_BLANK_RADIAL | SPOT_BLANK_ELEVATION)) != 0)
      PBD_spot_blank_bitmap |= (1 << (31 - Rpg_elevation_index));

   /* If radial count is the maximum, set the radial status to end 
      of elevation or volume. */
   if( Radial_cnt == PBD_max_num_radials ){

      LE_send_msg( GL_STATUS | LE_RPG_WARN_STATUS, 
                   "Number Of Radials In RDA Cut %d >= %d --> Forced End Of El/Vol\n",
                   rpg_hd->elev_num, PBD_max_num_radials );  

      if( rpg_hd->last_ele_flag ){

         rpg_hd->status = GENDVOL;

         PBD_start_volume_required = (PBD_DONT_SEND_RADIAL | 1);
         LE_send_msg( GL_INFO, "---> Start Of Volume Required .... Do Not Send Radial\n" );

      }
      else{

         rpg_hd->status = GENDEL;

         PBD_start_elevation_required = (PBD_DONT_SEND_RADIAL | 1);
         PBD_expected_elev_num = ++rpg_hd->elev_num;
         LE_send_msg( GL_INFO, "---> Start Of Elevation Required .... Do Not Send Radial\n" );

      }

      Old_azi = UNDEFINED_AZI;

   }

   /* Set the type of data this radial is ... i.e., BASEDATA, REFLDATA,
      or COMBBASE. */
   if( Waveform_type == VCP_WAVEFORM_CD )
      rpg_hd->msg_type |= COMBBASE_TYPE;

   else if( (Waveform_type == VCP_WAVEFORM_BATCH)
                       ||
            (Waveform_type == VCP_WAVEFORM_CDBATCH)
                       ||
            (Waveform_type == VCP_WAVEFORM_STP) )
      rpg_hd->msg_type |= BASEDATA_TYPE;

   else if( Waveform_type == VCP_WAVEFORM_CS )
      rpg_hd->msg_type |= REFLDATA_TYPE;

   /* Finish specifying the data type.  Check to see if the data is 
      considered "Super Resolution". */
   if( (rpg_hd->azm_reso == BASEDATA_HALF_DEGREE) 
                     || 
       (rpg_hd->surv_bin_size == 250)
                     ||
       (rpg_hd->n_dop_bins > BASEDATA_VEL_SIZE) )
      rpg_hd->msg_type |= SUPERRES_TYPE;

   return (0);

/* End of PH_process_header() */
}

/*********************************************************************

      Description:
	This function sets up the starting and delta angles for this
	radial. It also checks the azimuth increment of this radial
	and puts it in Delta_azimuth for later use. It finally resets
	rpg_hd->status field to be psuedo end of either elevation or 
	volume, if the accumulated scan angle in this elevation reaches
	360 degrees. rpg_hd->status is reset only if it is GOODINT
	(good internal radial). We don't want to overwrite any other
	status info.

      Inputs:
        azi - azimuth in degrees.
        rpg_hd - RPG base data 
        half_azimuth_spacing - 1/2 the radial azimuth spacing.

      Outputs:
        rpg_hd - RPG base data 

      Returns:
	This function returns 0 on success or -1 on failure.

*********************************************************************/
static int Find_start_and_delta_angles ( float azi, Base_data_header *rpg_hd,
                                         float half_azimuth_spacing ){

    static float st_ang, end_ang;	/* angles of starting and ending 
					   edges of this radial */
    static float scan_angle = 0.0f;	/* accumulate azimuth scan angle 
					   in this elevation */
    float delta;
    int idelta;

    /* First radial in an elevation */
    if (Radial_cnt == 1)
	st_ang = azi - half_azimuth_spacing;

    else
	st_ang = end_ang;

    /* Check for wrap around 360 degrees */
    if (st_ang < 0.)
	st_ang += 360.;

    else if (st_ang >= 360.)
        st_ang -= 360.;

    rpg_hd->start_angle = Round( st_ang*10.0 );

    end_ang = azi + half_azimuth_spacing;
    delta = end_ang - st_ang;
    idelta = Round( end_ang*10.0 ) - rpg_hd->start_angle;

    /* Check for warp around 360 degrees */
    if( delta < 0.0 ){	

	delta += 360.;
	idelta += 3600;

    }

    rpg_hd->delta_angle = idelta;

    /* Save delta for future azimuth change check */
    Delta_azimuth = delta;

    /* Check if this elevation scan reaches 360 degrees */
    if (Radial_cnt == 1)	/* reset scaned angle */
	scan_angle = delta;

    else{

        /* Check if this is a fat radial.  If this is a fat radial, we do not want
           to include in the computation for determining psuedo end of elevation or
           volume since this radial status is also passed to downstream consumers. */
        if ( Delta_azimuth <= DELTA_AZI_TOLERANCE ) {

   	   scan_angle += delta;

	   /* If almost more than 360 degrees are scaned, set pseudo end of 
	      elevation or volume */
	   if (scan_angle >= 360.){

	       if (rpg_hd->status == GOODINT) {

	      	   if (rpg_hd->elev_num == Number_elevations){

		       /* The last elevation */
		       rpg_hd->status = PGENDVOL;
                       LE_send_msg( GL_INFO, "Marking Azi#/El# %d Pseudo EOV\n",
                                    rpg_hd->azi_num, rpg_hd->elev_num );

                   }
		   else{

		       rpg_hd->status = PGENDEL;
                       LE_send_msg( GL_INFO, "Marking Azi#/El# %d/%d Pseudo EOE\n",
                                    rpg_hd->azi_num, rpg_hd->elev_num );

                   }
		   scan_angle = 0.;

	       }

	   }

       }

    }

    return (0);

/* End of Find_start_and_delta_angles() */
}

/*********************************************************************

      Description:
	This function returns the round-off integer of a float point
	number.

      Inputs: 
        r - a floating number.

      Outputs:

      Returns: 
	This function returns the round-off integer of a float point
	number.

**********************************************************************/
int Round ( float r	/* a float number */) {

#ifdef LINUX
    return( (int) roundf( r ) );
#else
    if ((float)r >= 0.)
	return ((int)(r + .5));
    else 
	return (-(int)((-r) + .5));
#endif

/* End of Round() */
}

/**********************************************************************************

   Description:
      This function sets fields in the RPG radial header relating to azimuth
      and elevation.

   Inputs:
      gbd - Generic basedata radial.

   Outputs:
      rpg_hd - RPG radial header.

   Returns:
      There is no return value defined for this function.

**********************************************************************************/
static void Process_angle_data( Generic_basedata_t *gbd, Base_data_header *rpg_hd ){


   /* Needed for delta azimuth and delta elevation tolerance checks. */
   static float az_tolerance = DELTA_AZI_TOLERANCE;
   static float elv_tolerance_warning = DELTA_ELV_TOLERANCE;
   static float elv_tolerance_alarm = DELTA_ELV_TOLERANCE;
   static float half_azimuth_spacing = 0.5f;

   float diff;
   float azi;

   /* Setup the radial count and reset azimuth number at beginning of
      elevation/volume. */
   if( (gbd->base.status == GOODBEL) || (gbd->base.status == GOODBVOL) ){

      double dtemp;

      /* Set the value for half of the azimuth spacing.  This value is used in
         Find_start_and_delta_angles(). */
      if( gbd->base.azimuth_res == BASEDATA_ONE_DEGREE )
         half_azimuth_spacing = 0.5f;

      else
         half_azimuth_spacing = 0.25f;

      az_tolerance = DELTA_AZI_TOLERANCE;
      elv_tolerance_warning = DELTA_ELV_TOLERANCE;
      elv_tolerance_alarm = DELTA_ELV_TOLERANCE;

      /* Make sure the warning level is less than or equal to alarm level. */
      if( elv_tolerance_warning > elv_tolerance_alarm )
         elv_tolerance_warning = elv_tolerance_alarm;

   }
   else if( (gbd->base.status == GENDEL) || (gbd->base.status == GENDVOL) ){

      /* Reset Old_azi at end of elevation or volume. */
      Old_azi = UNDEFINED_AZI;

   }

   /* Get the radial azimuth. */
   azi = gbd->base.azimuth;

   /* Check for duplicates. */
   if( Old_azi != UNDEFINED_AZI ){

      diff = azi - Old_azi;

      if (diff < 0.)
         diff = -diff;

      if (diff < .001)
         azi = Old_azi + .001;
   }

   Old_azi = azi;

   /* Assign the azimuth angle. */
   rpg_hd->azimuth = azi;

   /* Set sine and cosine of the azimuth angle. */
   rpg_hd->sin_azi = sinf(azi * DEG_TO_RAD);
   rpg_hd->cos_azi = cosf(azi * DEG_TO_RAD);

   /* Set start and delta angles of this azimuth. */
   Find_start_and_delta_angles( azi, rpg_hd, half_azimuth_spacing );

   /* Get the radial elevation. */
   rpg_hd->elevation = gbd->base.elevation;

   if( rpg_hd->elevation > 180.0f )
      rpg_hd->elevation -= 360.0f;

   /* Compare with expected elevation. */
   diff = (float) rpg_hd->elevation - Target_elevation [rpg_hd->elev_num - 1];

   if (diff < 0.)
      diff = -diff;

  /* If difference greater than threshold, try sails  */
   if( diff >= elv_tolerance_warning ){
      diff = (float) rpg_hd->elevation - Target_elevation [0];
      if (diff < 0.) diff = -diff;
   } 
   if( diff >= elv_tolerance_warning ){
      rpg_hd->elev_num = rpg_hd->elev_num - 2;
      diff = (float) rpg_hd->elevation - Target_elevation [rpg_hd->elev_num - 1];
      if (diff < 0.) diff = -diff;
      /* If there is no sails */
      if( diff >= elv_tolerance_warning ) rpg_hd->elev_num = rpg_hd->elev_num + 2;
   }
   /* If difference greater than threshold, report error. */
   if( diff >= elv_tolerance_warning ){

      /* Send status message - not implemented. */
      /* SHUN comment out
      LE_send_msg( GL_STATUS | LE_RPG_WARN_STATUS,
                   "Elevation Tolerance EXCEEDED (AZ/EL/PEL):  %6.1f/%5.1f/%5.1f",
                   rpg_hd->azimuth, rpg_hd->elevation,
                   Target_elevation [rpg_hd->elev_num - 1] );
      */

      /* We assume anything greater than the alarm level is a problem with
         the encoder and therefore isn't a true elevation reading. */
      if( diff >= elv_tolerance_alarm ){

         /* Reset elevation angle to the target. */
         //rpg_hd->elevation = Target_elevation [rpg_hd->elev_num - 1];
        printf("\n");

      }

   }

   /* Set the sine and cosine of the elevation angle. */
   rpg_hd->sin_ele = sinf( rpg_hd->elevation * DEG_TO_RAD );
   rpg_hd->cos_ele = cosf( rpg_hd->elevation * DEG_TO_RAD );

   /* Has a FAT RADIAL been detected? */
   if ( Delta_azimuth > az_tolerance ) {

      /* Azimuth difference greater than tolerance. */
      LE_send_msg( GL_STATUS | LE_RPG_WARN_STATUS, 
                   "Azimuth Tolerance EXCEEDED (EL/AZ/DELTA):  %4.1f/%6.1f/%6.1f",
                   rpg_hd->elevation, rpg_hd->azimuth, Delta_azimuth );

      /* If the elevation to restart is the lowest, issue volume
         restart rda control command. */

   }

/* End of Process_angle_data(). */
}

/**********************************************************************************

   Description:
      This function sets fields in the RPG radial header from information in
      the radial block header, the elevation block header and the volume block
      header.

   Inputs:
      gbd - Generic basedata radial.

   Outputs:
      rpg_hd - RPG radial header.

   Returns:
      There is no return value defined for this function.

**********************************************************************************/
static void Process_block_headers( Generic_basedata_t *gbd, Base_data_header *rpg_hd ){

   Generic_rad_t *rad_hdr = NULL;
   Generic_vol_t *vol_hdr = NULL;
   Generic_elev_t *elev_hdr = NULL;

   /* Transfer data from radial block header. */
   rad_hdr = (Generic_rad_t *) ORPGGDR_get_data_block( (char *) gbd, ORPGGDR_RRAD );
   if( rad_hdr != NULL ){

      rpg_hd->unamb_range = rad_hdr->unamb_range;
      rpg_hd->nyquist_vel = rad_hdr->nyquist_vel;
      rpg_hd->horiz_noise = rad_hdr->horiz_noise;
      rpg_hd->vert_noise = rad_hdr->vert_noise;

   }
   else{

      /* This should never happen ..... */
      LE_send_msg( GL_INFO, "Radial Does Not Contain Radial Data Block\n" );

      rpg_hd->unamb_range = 0;
      rpg_hd->nyquist_vel = 0;
      rpg_hd->horiz_noise = 0.0f;
      rpg_hd->vert_noise = 0.0f;

   }

   /* Transfer data from elevation block header. */
   elev_hdr = (Generic_elev_t *) ORPGGDR_get_data_block( (char *) gbd, ORPGGDR_RELV );
   if( elev_hdr != NULL ){

      rpg_hd->calib_const = elev_hdr->calib_const;
      rpg_hd->atmos_atten = elev_hdr->atmos;

   }
   else{

      /* This should never happen ..... */
      LE_send_msg( GL_INFO, "Radial Does Not Contain Elevation Data Block\n" );
      rpg_hd->calib_const = 0.0f;
      rpg_hd->atmos_atten = 0;

   }

   /* Transfer data from volume block header. */
   vol_hdr = (Generic_vol_t *) ORPGGDR_get_data_block( (char *) gbd, ORPGGDR_RVOL );
   if( vol_hdr != NULL ){

      rpg_hd->vcp_num = vol_hdr->vcp_num;
      rpg_hd->version = (vol_hdr->major_version << 8) + vol_hdr->minor_version;
      rpg_hd->latitude = vol_hdr->lat;
      rpg_hd->longitude = vol_hdr->lon;
      rpg_hd->height = (unsigned short) ((float) vol_hdr->height);
      rpg_hd->feedhorn_height = (unsigned short) ((float) vol_hdr->feedhorn_height);
      rpg_hd->horiz_shv_tx_power = vol_hdr->horiz_shv_tx_power;
      rpg_hd->vert_shv_tx_power = vol_hdr->vert_shv_tx_power;
      rpg_hd->sys_diff_refl = vol_hdr->sys_diff_refl;
      rpg_hd->sys_diff_phase = vol_hdr->sys_diff_phase;

   }
   else{

      /* This should never happen ..... */
      LE_send_msg( GL_INFO, "Radial Does Not Contain Volume Data Block\n" );
      rpg_hd->vcp_num = 0;
      rpg_hd->version = 0;
      rpg_hd->latitude = PBD_latitude;
      rpg_hd->longitude = PBD_longitude;
      rpg_hd->height = PBD_rda_height;
      rpg_hd->feedhorn_height = 20.0f;
      rpg_hd->horiz_shv_tx_power = 0.0f;
      rpg_hd->vert_shv_tx_power = 0.0f;
      rpg_hd->sys_diff_refl = 0.0f;
      rpg_hd->sys_diff_phase = 0.0f;

   }

/* End of Process_block_headers(). */
}

/**********************************************************************************

   Description:
      This function sets fields in the RPG radial header from information in
      the moment block headers. 

   Inputs:
      gbd - Generic basedata radial.

   Outputs:
      rpg_hd - RPG radial header.

   Returns:
      -1 on error, 0 otherwise.

**********************************************************************************/
static int Process_moment_block_headers( Generic_basedata_t *gbd, 
                                         Base_data_header *rpg_hd ){

   Generic_moment_t *mom = NULL;
   int leading_edge_range, diff, first_bin;

   /* Only set a beginning of elevation/ volume. */
   static int max_surv_bins, max_dop_bins, srto70;

   /* Initialize Info data structure. */
   memset( &Info, 0, sizeof(Moment_info_t) );

   /* Get the Reflectivity moment block header. */
      mom = (Generic_moment_t *) ORPGGDR_get_data_block( (char *) gbd, ORPGGDR_DREF );

   if( mom != NULL ){

      /* Bin size should always be set.  If not, set default bin size. */
      rpg_hd->surv_bin_size = mom->bin_size;
      if( rpg_hd->surv_bin_size <= 0 )
         rpg_hd->surv_bin_size = 1000;
 
      rpg_hd->n_surv_bins = mom->no_of_gates;
      rpg_hd->msg_type |= REF_ENABLED_BIT;

      /* Initialize range to first bin and the bin number to the first bin.  These
         will be set based on "Info". */
      rpg_hd->surv_range = 1;
      rpg_hd->range_beg_surv = 0;

      /* Set the SNR thresholds for reflectivity. */ 
      rpg_hd->surv_snr_thresh = mom->SNR_threshold;

      /* Is there any surveillance data in this radial? */
      if( rpg_hd->n_surv_bins > 0 ){

         /* If first bin is behind radar, adjust the first bin and the number
            of bins;  The first bin must be positive.  If the first bin is in
            front radar, then the number of RDA bins remains the same.  We must
            account for this when we move the data and we pad the front with 0's. */

         /* The leading_edge_range is the range to the beginning of the first
            range bin.  Dividing by the bin size, gives the number of bins
            between 0 range and the start of the data. */
         leading_edge_range = mom->first_gate_range - rpg_hd->surv_bin_size / 2;
         first_bin = Round ((float) mom->first_gate_range / (float) rpg_hd->surv_bin_size);

         /* Process accordingly based on whether the first bin is in front, at, or
            behind the radar. */
         if( first_bin < 1 ) {

            rpg_hd->n_surv_bins += first_bin;
            Info.rda_surv_bin_off = -first_bin;
            Info.num_surv_bins = rpg_hd->n_surv_bins;

            /* Set the range to the start of the first surveillance bin, in meters. */
            rpg_hd->range_beg_surv =
                (short) (leading_edge_range + (Info.rda_surv_bin_off*rpg_hd->surv_bin_size));

         }
         else {

            rpg_hd->n_surv_bins += (first_bin - 1);
            Info.rpg_surv_bin_off = (first_bin - 1);
            Info.num_surv_bins = rpg_hd->n_surv_bins;

            /* Set the range to the start of the first surveillance bin, in meters. */
            rpg_hd->range_beg_surv =
                (short) (leading_edge_range - (Info.rpg_surv_bin_off*rpg_hd->surv_bin_size));

         }

         /* Set the range to the start of the first surveillance bin, in bins. */
         rpg_hd->surv_range = 1;

      }

   }
   else{

      rpg_hd->surv_bin_size = 1000;
      rpg_hd->n_surv_bins = 0;
      rpg_hd->surv_range = 1;
      rpg_hd->range_beg_surv = 0;

   }

   /* Get the Velocity moment block header. */
   mom = (Generic_moment_t *) ORPGGDR_get_data_block( (char *) gbd, ORPGGDR_DVEL );
   if( mom != NULL ){

      rpg_hd->dop_bin_size = mom->bin_size;
      if( rpg_hd->dop_bin_size <= 0 )
         rpg_hd->dop_bin_size = 250;

      rpg_hd->n_dop_bins = mom->no_of_gates;
      rpg_hd->msg_type |= VEL_ENABLED_BIT;
      rpg_hd->vel_tover = mom->tover;
      rpg_hd->vel_snr_thresh = mom->SNR_threshold;

      rpg_hd->dop_range = 1;
      rpg_hd->range_beg_dop = 0; 

      /* Is there any Doppler data in this radial? */
      if( rpg_hd->n_dop_bins > 0 ){

         /* Range to first bin and number of bins for Doppler. */

         /* The leading_edge_range is the range to the beginning of the first range
            bin.  Dividing by the bin size, gives the number of bins between 0 range
            and the start of the data. */
         leading_edge_range = mom->first_gate_range - rpg_hd->dop_bin_size / 2;
         first_bin = Round ((float) leading_edge_range / (float) rpg_hd->dop_bin_size);

         rpg_hd->n_dop_bins += first_bin;

         /* Process accordingly based on whether the first bin is in front, at, or
            behind the radar. */
         if (first_bin < 1) {

            Info.rda_dop_bin_off = -first_bin;
            Info.num_dop_bins = rpg_hd->n_dop_bins;

         }
         else {

            Info.rpg_dop_bin_off = first_bin;
            Info.num_dop_bins = rpg_hd->n_dop_bins;

         }

         /* Set the range to the start of the first Doppler bin, in meters. */
         rpg_hd->range_beg_dop =
             (short) (leading_edge_range - (first_bin*rpg_hd->dop_bin_size));

         /* Set the range to the start of the first Doppler bin, in bins. */
         rpg_hd->dop_range = 1;

         /* We assume the Doppler resolution is either 0.5 or 1.0 m/s. */
         if( mom->scale == 2.0 )
            rpg_hd->dop_resolution = 1;

         else
            rpg_hd->dop_resolution = 2;

      }

   }
   else{

      rpg_hd->dop_bin_size = 250;
      rpg_hd->n_dop_bins = 0;
      rpg_hd->dop_range = 1;
      rpg_hd->dop_resolution = 1;
      rpg_hd->vel_tover = 0;


   }

   /* Get the Spectrum Width moment block header. */
   mom = (Generic_moment_t *) ORPGGDR_get_data_block( (char *) gbd, ORPGGDR_DSW );
   if( mom != NULL ){

      rpg_hd->msg_type |= WID_ENABLED_BIT;
      rpg_hd->spw_tover = mom->tover;
      rpg_hd->spw_snr_thresh = mom->SNR_threshold;

   }
   else{

      rpg_hd->spw_tover = 0;

   }

   /* Find the height to 70,000 ft.   We want to clip the data along a radial
      to the minimum of 70,000 ft and either 230 km or the unambiguous range
      depending on the waveform.  We also need to clip the data to array
      bounds limit. */
   if( (gbd->base.status == GOODBEL) || (gbd->base.status == GOODBVOL) ){

      float sin_ele;
      float rda_height = PBD_rda_height/1000.0f;

      sin_ele = sin ((double) Target_elevation [rpg_hd->elev_num - 1] * DEG_TO_RAD );
      srto70 = (int) (IRRE*(sqrt( (sin_ele*sin_ele) +
                      2.0*(rda_height + TOPHGT_KM)/IRRE ) - sin_ele));

      max_surv_bins = ((srto70*1000) / rpg_hd->surv_bin_size);

      /* Since we don't know how many reflectivity bins there can be, we
         check the number there are.  If the number there are are less
         than or equal to BASEDATA_REF_SIZE, then this is the limit. */
      if( rpg_hd->n_surv_bins <= BASEDATA_REF_SIZE ){

         if( max_surv_bins > BASEDATA_REF_SIZE )
            max_surv_bins = BASEDATA_REF_SIZE;

      }
      else if( max_surv_bins > MAX_BASEDATA_REF_SIZE )
         max_surv_bins = MAX_BASEDATA_REF_SIZE;

      /* For Doppler, the maximum number of bins is limited by BASEDATA_DOP_SIZE. */
      max_dop_bins = ((srto70*1000) / rpg_hd->dop_bin_size);
      if( max_dop_bins > BASEDATA_DOP_SIZE )
         max_dop_bins = BASEDATA_DOP_SIZE;

   }

   /* Is there any surveillance data in this radial? */
   if( rpg_hd->n_surv_bins > 0 ){

      /* Clip the data a 70,000 ft.  Also make sure there isn't more data
         that we have space allocated for. */
      if( (diff = (rpg_hd->n_surv_bins - max_surv_bins)) > 0 ){

         rpg_hd->n_surv_bins -= diff;
         Info.num_surv_bins -= diff;

      }

   }

   /* Is there any Doppler data in this radial? */
   if( rpg_hd->n_dop_bins > 0 ){

      /* Clip the data a 70,000 ft.  Also make sure there isn't more data
         that we have space allocated for. */
      if( (diff = (rpg_hd->n_dop_bins - max_dop_bins)) > 0 ){

         rpg_hd->n_dop_bins -= diff;
         Info.num_dop_bins -= diff;

      }

   }

   /* If the number of surveillance bins or Doppler bins is negative, set
      the number to 0 with the corresponding beginning range and bin number. */
   if (rpg_hd->n_surv_bins < 0){

      rpg_hd->n_surv_bins = 0;
      rpg_hd->surv_range = 1;
      rpg_hd->range_beg_surv = 0;
      Info.num_surv_bins = 0;

   }

   if (rpg_hd->n_dop_bins < 0){

      rpg_hd->n_dop_bins = 0;
      rpg_hd->dop_range = 1;
      rpg_hd->range_beg_dop = 0;
      Info.num_dop_bins = 0;

   }

   /* Make sure that the space in RPG base data buffer is large enough. */
   if( (rpg_hd->n_surv_bins > (int) MAX_BASEDATA_REF_SIZE)
                           ||
       (rpg_hd->n_dop_bins > (int) BASEDATA_DOP_SIZE) ){
      LE_send_msg( GL_STATUS | LE_RPG_WARN_STATUS,
                   "Too Many Bins Found In RDA Radial Message (%d %d )\n",
                   rpg_hd->n_surv_bins, rpg_hd->n_dop_bins );
      return (-1);

   }

   return 0;

/* End of Process_moment_block_headers(). */
}

void swp_db(Generic_moment_t *db)
{
   int i, j;
   unsigned short *usp;
      db->info = reverse_int4( db->info);
      db->no_of_gates = reverse_int2( db->no_of_gates);
      db->first_gate_range = reverse_int2( db->first_gate_range);
      db->bin_size = reverse_int2( db->bin_size);
      db->tover = reverse_int2( db->tover);
      db->SNR_threshold = reverse_int2( db->SNR_threshold);
      db->scale  = reverse_float( db->scale);
      db->offset = reverse_float( db->offset);
      if (db->data_word_size == 16) {
          usp = &db->gate.u_s[0];
          swp_int2(db->no_of_gates, ( short *) &db->gate.u_s[0]);
      }
      if (db->data_word_size == 32 && db->offset != 0.0) swp_int4(db->no_of_gates, ( int *) &db->gate.u_i[0]);
      if (db->data_word_size == 32 && db->offset == 0.0) swp_float(db->no_of_gates, (float *) &db->gate.f[0]);
      return;
}

void out_missing_value(Cout_strct_ncep * refvel, Base_data_header * bh, int *pscan, int *dualscan, int *pnum_elevation, int *sailsindex)
{
    int i, j, k;
    int year, month, day, hour, minute, second;

    for (  i = 0; i < 6; i++)
    {
        for (j = 0; j < NAZIM; j++)
        {
            strcpy(refvel[i].radar_name, bh->radar_name);
            refvel[i].vcpnum[i] = bh->vcp_num;
            calendar_date( bh->date, &day, &month, &year );

            refvel[i].elev_angle[j] = Target_elevation[*pscan];
            refvel[i].year[j] = (float) year;
            refvel[i].month[j] = (float) month;
            refvel[i].day[j] = (float) day;

            convert_time( bh->time, &hour, &minute, &second);
            refvel[i].hour[j] = (float) hour;
            refvel[i].minute[j] = (float) minute;
            refvel[i].second[j] = (float) second;

            refvel[i].radlat[j] = bh->latitude;
            refvel[i].radlon[j] = bh->longitude;
            refvel[i].radhgt[j] = bh->height + bh->feedhorn_height;
            refvel[i].fstgatdis[j] = bh->range_beg_surv + bh->surv_bin_size/2;
            refvel[i].nyq_vel[j] = 0.01 * bh->nyquist_vel;
            refvel[i].num_beam[j] = bh->azi_num;
            if (bh->n_surv_bins > NGATE) bh->n_surv_bins = NGATE;
            refvel[i].num_gate[j] = bh->n_surv_bins;
            refvel[i].gateWidth[j] = bh->surv_bin_size;
            refvel[i].azim[j] = bh->azimuth;

            for (k = 0; k < NGATE; k++)
            {
                refvel[i].field[j][k] = 999.0;
                refvel[i].gate_dis[j][k] = 0.0;
            }
        }
    }
    printf("Output MISSING data at %f elevation at %d level \n",  Target_elevation[*pscan], *pnum_elevation + 1);
    get_radar_data(&refvel[0], &refvel[1], &refvel[2], &refvel[3], &refvel[4], &refvel[5], pnum_elevation, dualscan, sailsindex);
    *pnum_elevation =  *pnum_elevation +1;
    (*pscan)++;
    memset( refvel, 0, 6 * sizeof(Cout_strct_ncep));
}
