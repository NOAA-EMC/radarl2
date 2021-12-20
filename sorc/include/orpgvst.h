/*
 * RCS info 
 * $Author: steves $
 * $Locker:  $
 * $Date: 2006/11/29 19:27:09 $
 * $Id: orpgvst.h,v 1.7 2006/11/29 19:27:09 steves Exp $
 * $Revision: 1.7 $
 * $State: Exp $
 * $Log: orpgvst.h,v $
 * Revision 1.7  2006/11/29 19:27:09  steves
 * issue 3-046
 *
 * Revision 1.6  2006/01/12 15:51:29  steves
 * issue 2-744
 *
 * Revision 1.5  2005/10/05 14:10:29  steves
 * issue 2-873
 *
 * Revision 1.4  2002/12/11 22:10:53  nolitam
 * Add RCS header information
 *
 * Revision 1.3  2002/08/30 23:05:23  eddief
 * Add a function that will allow prod_extract to work correctly
 *
 * Revision 1.2  1999/05/07 20:21:48  priegni
 * Add update time functions
 *
*/
/****************************************************************
 *								*
 *  Module: orpgvst.h						*
 *								*
 *   Description: This is the header file associated with the	*
 *		 Volume status function group in libORPG.	*
 *								*
 ****************************************************************/



#ifndef ORPGVST_STATUS_H
#define ORPGVST_STATUS_H

#ifdef __cplusplus
extern "C"
{
#endif

#include <a309.h>
#include <gen_stat_msg.h>
#include <rda_control.h>
#include <rss_replace.h>

/*	Various return status values for Vol status functions	*/

#define	ORPGVST_DATA_NOT_FOUND		-9999

/*	Macro definitions for previous volume status		*/

#define	ORPGVST_ABORTED				0
#define	ORPGVST_SUCCESS				1

/*	Macro definitions for mode of operation			*/

#define	ORPGVST_MAINTENANCE_MODE	MAINTENANCE_MODE
#define ORPGVST_PRECIPITATION_MODE	PRECIPITATION_MODE
#define	ORPGVST_CLEAR_AIR_MODE		CLEAR_AIR_MODE

/*	Prototypes for Volume status functions.			*/

char*	ORPGVST_read ( char *vol_stat );
int	ORPGVST_io_status ();
int	ORPGVST_status_update_flag ();
time_t	ORPGVST_status_update_time ();
void	ORPGVST_en_status_callback (EN_id_t evtcd, char *msg, int msglen, void *arg);
void	ORPGVST_lb_notify_callback (int fd, LB_id_t msgid, int msginfo, void *arg);
unsigned long	ORPGVST_get_volume_number ();
unsigned long	ORPGVST_get_volume_time   ();
int	ORPGVST_get_volume_flag           ();
int	ORPGVST_get_volume_duration       ();
int	ORPGVST_get_volume_date           ();
int	ORPGVST_get_previous_status       ();
int	ORPGVST_get_mode                  ();
int	ORPGVST_get_vcp                   ();
int     ORPGVST_get_volume_scan           ();
int     ORPGVST_get_vcp_id                ();
int	ORPGVST_get_number_elevations     ();
int	ORPGVST_get_elevation             (int indx);
int	ORPGVST_get_index                 (int indx);
int 	ORPGVST_get_rda_index		  (int rpg_elev_indx);
int     ORPGVST_get_current_vcp           (Vcp_struct *vcp);
int	ORPGVST_get_superres_bitmap	  ();
int	ORPGVST_is_rpg_elev_superres	  (int rpg_elev_num);


#ifdef __cplusplus
}
#endif

#endif

