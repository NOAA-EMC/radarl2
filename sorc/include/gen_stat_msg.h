/*
 * RCS info
 * $Author: steves $
 * $Locker:  $
 * $Date: 2006/09/28 17:17:58 $
 * $Id: gen_stat_msg.h,v 1.57 2006/09/28 17:17:58 steves Exp $
 * $Revision: 1.57 $
 * $State: Exp $
 */
/*****************************************************************
//
//     Contains the structure defintions for the volume-base
//     general status message information as well as the RPG
//     status of the general status message.  The rda_status.h
//     header file contains the RDA status portion of the general
//     status message.
//    
//     The Linear Buffer data store ID is ORPGDAT_GSM_DATA
//
//     The task table is also defined here and stored in the same data 
//     store.
//
//     Scan summary data used by legacy and new algorithms
//     is stored here.
//
//     The RDA status portion is updated by the CONTROL RDA 
//     function whenever new status data is received from the 
//     RDA.   Whenever the status changes, CONTROL RDA posts
//     the event ORPGEVT_RDA_STATUS_CHANGE. 
//     
//     The volume-based status is updated at beginning of volume
//     by PROCESS BASE DATA.  PROCESS BASE DATA posts the event
//     ORPGEVT_START_OF_VOLUME whenever this data has been 
//     updated.
//
//     The RPG status portion is TBD.
//
****************************************************************/

#ifndef GEN_STAT_MSG_H
#define GEN_STAT_MSG_H


#ifdef __cplusplus
extern "C"
{
#endif

#include<rda_status.h>
#include<orpgctype.h>
#include<lb.h>
#include<vcp.h>
#include<mode_select.h>


/* Message IDs in the ORPGDAT_GSM_DATA LB */
enum { RDA_STATUS_ID, VOL_STAT_GSM_ID, PREV_RDA_STAT_ID,
       SPARE_ID, ARCHIII_STATUS_ID, WX_STATUS_ID };

#define MAX_CUTS           VCP_MAXN_CUTS 


/*    Volume Scan Status            */
/*
    Note:   This structure is also defined in a309.inc, a3cd06.  If 
            Vol_stat_gsm_t changes, the include file also needs to 
            change. */
typedef struct volume_status {

   unsigned long volume_number;     /* Current volume scan sequence 
                                       number.  Monotonically 
                                       increases.  Initial value 0. */

   unsigned long cv_time;           /* Current volume scan time in
                                       milliseconds past midnight. */

   int cv_julian_date;              /* Current volume scan Julian date */

   int initial_vol;                 /* Flag, if set, indicates the 
                                       volume associated with volume
                                       volume is the initial volume.  
                                       It is assumed for an initial
                                       volume, no radar data-derived
                                       products will be available. */ 

   int pv_status;                   /* Previous volume scan status:
                                          1 - completed successfully,
                                          0 - aborted. */

   int expected_vol_dur;            /* Expected volume scan duration,
                                       in seconds. */

   int volume_scan;                 /* The volume scan number [0, 80]. */

   int mode_operation;              /* Mode of operation:
                                          0 - Maintenance Mode
                                          1 - Clear Air Mode        
                                          2 - Precipitation Mode */

   int vol_cov_patt;                /* Volume coverage pattern */

   int rpgvcpid;                    /* slot in vcp_table containing VCP
                                       data associated with vol_cov_patt. */

   int num_elev_cuts;               /* Number of elevations in VCP. */

   short elevations[MAX_CUTS];      /* Elevation angles (deg*10). */
   
   short elev_index[MAX_CUTS];      /* RPG elev index associated with 
                                       each elevation angle. */
   
   int super_res_cuts;		    /* Bit map indicating which RPG cuts
                                       are expected to have super res data. */

   Vcp_struct current_vcp_table;    /* The current VCP data. */
         
} Vol_stat_gsm_t;


/* RDA-RPG communications status */
typedef struct {
   
   int wblnstat;    /* Wideband line status */
   int rda_display_blanking;    /* Status fields to NOT blank. 
                                      0 = None Blanked.
             >0 = Field to Blank. */
   int wb_failed;    /* If set (non 0) then the WB *
           is considered failed.  If 0,
           then not considered failed. */


} RDA_RPG_comms_status_t;

/* RDA status structure ... include comms status and RDA Status Message.
   The RDA_status_msg_t structure is defined in rda_status.h. */
typedef struct {

   RDA_RPG_comms_status_t wb_comms;
   RDA_status_msg_t       status_msg;

} RDA_status_t;
   

/* ORDA status structure ... include comms status and ORDA Status Message.
   The ORDA_status_msg_t structure is defined in rda_status.h. */
typedef struct {

   RDA_RPG_comms_status_t wb_comms;
   ORDA_status_msg_t       status_msg;

} ORDA_status_t;


/*
  Stores RDA state variables to be used to return the RDA to a previous
  state after wideband failures, restarts, etc.
*/
typedef struct previous_state {

   short rda_status;
   unsigned short data_trans_enbld;
   short rda_control_auth;
   short int_suppr_unit;
   short op_mode;
   short spot_blanking_status;
   short current_vcp_number;
   short channel_control;
   
}  Previous_state_t;


#define MAX_VSCANS          81
#define MAX_SCAN_SUM_VOLS   (MAX_VSCANS-1)

typedef struct
{
  unsigned short status;    /* archiveIII status bits defined in archive_III.h */
  short          pcnt_used; /* the percentage of archive device currently used */
  int            code;      /* a message is associated with each code */
  LB_id_t        last_read; /* LB_id_t taken from lb.h. last_read keeps track
                               of the status log message that was last read by 
                               archiveIII status log */
} ArchIII_status_t;


#define WX_STATUS_UNDEFINED   -1

/* Mode Selection Supplemental Data. */
typedef struct{

   time_t curr_time;       /* The UNIX time the detection algorithm 
                              was last executed. */
   time_t last_time;       /* UNIX time precipitation was last 
                              detected. */
   time_t time_to_cla;     /* End time for category 1 precipitation
                              detected. */
   int pcpctgry;           /* The current precipitation 
                              category.
                                 0 - No Precipitation
                                 1 - Precipitation */
   int prectgry;           /* The previous precipitation  
                              category. */
} A3052t;


typedef struct{

   int current_wxstatus;
   int current_vcp;
   int recommended_wxstatus;
   int wxstatus_deselect;
   time_t recommended_wxstatus_start_time;
   int recommended_wxstatus_default_vcp;
   time_t conflict_start_time;
   time_t current_wxstatus_time;
   float precip_area;
   Mode_select_entry_t mode_select_adapt;
   A3052t a3052t;

} Wx_status_t;


#ifdef __cplusplus
}
#endif

#endif
