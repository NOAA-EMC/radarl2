/*
 * RCS info
 * $Author: steves $
 * $Locker:  $
 * $Date: 2006/09/26 20:43:28 $
 * $Id: orpgsum.h,v 1.3 2006/09/26 20:43:28 steves Exp $
 * $Revision: 1.3 $
 * $State: Exp $
 */

#ifndef ORPGSUM_H
#define ORPGSUM_H

#include <orpg.h>
#include <gen_stat_msg.h>

#define MAX_SCAN_SUM_VOLS	(MAX_VSCANS-1)
#define SCAN_SUMMARY_ID		1

typedef struct {

   int volume_start_date;	/* Modified Julian start date */

   int volume_start_time;	/* From midnight, in seconds */

   int weather_mode;		/* 2 = convective, 1 = clear air */

   int vcp_number;		/* Pattern number:
                     		   Maintenance/Test: > 255
                     		   Operational: <= 255
                     		   Constant Elevation Types: 1 - 99 */

   short rpg_elev_cuts;		/* Number of RPG elevation cuts in VCP */

   short rda_elev_cuts;		/* Number of RDA elevation cuts in VCP */

   int spot_blank_status;	/* Bitmap indicating whether elevation
				   cut has spot blanking enabled */

   unsigned char super_res[ECUT_UNIQ_MAX];	
				/* Bitmap indicating whether elevation
				   cut is Super Resolution.   Each byte 
                                   corresponds to an RPG elevation cut. 
				   Bits are defined in vcp.h */

   int spare;			/* Reserved for future use. */

} Scan_Summary;

/* Note:  Scan Summary Data is defined as 81 volume scans.  Index 0
   (volume scan number 0 is reserved for the initial volume scan. */
typedef struct {

   Scan_Summary scan_summary[MAX_VSCANS];

} Summary_Data;

Scan_Summary* ORPGSUM_get_scan_summary(int vol_num );
Summary_Data* ORPGSUM_get_summary_data( );
int ORPGSUM_set_summary_data( Summary_Data* summary_data );
int ORPGSUM_read_summary_data( void *buf );

#endif /* #ifndef ORPGSUM_H */
