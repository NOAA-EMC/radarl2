/*
 * RCS info
 * $Author: steves $
 * $Locker:  $
 * $Date: 2005/11/08 23:35:04 $
 * $Id: rdacnt.h,v 1.12 2005/11/08 23:35:04 steves Exp $
 * $Revision: 1.12 $
 * $State: Exp $
 */
#ifndef RDACNT_H
#define RDACNT_H

/*
  Define Adaptation Data Block RDACNT.
*/
#define RDACNT_SIZE       66000

/*
  Define VCP data constants and size information.
*/
#define WXVCPMAX	     21
#define WXMAX                 2
#define COMPMAX               2
#define VCPMAX               20
#define FILES_MAX            VCPMAX
#define ECUTMAX              25
#define ECUT_UNIQ_MAX        20
#define VOL_ATTR             11
#define EL_ATTR              23
#define VCPSIZ               ((ECUTMAX*EL_ATTR)+VOL_ATTR)
#define VCP_RDA_DEFINED       0
#define VCP_RPG_DEFINED       1
#define VCP_EXPERIMENTAL      2

/*
  Define PRF data constants and sizes.
*/
#define PRFMAX		8
#define NAPRFVCP	(PRFMAX+2)
#define NAPRFELV	(PRFMAX+1)
#define MXALWPRF	(NAPRFVCP+(ECUTMAX*NAPRFELV))
#define DELPRI_MAX	5

/*
  RDACNT Adaptation Data Structure
*/
typedef struct {

   int   rdacnt_start;			/* Structure start identifier.	*/

   short rdcwxvcp[WXMAX][WXVCPMAX]; 	/* Weather mode table.        	*/

   short rdc_where_defined[COMPMAX][VCPMAX]; 	/* VCP defined table.  	*/

   short rdcvcpta[VCPMAX][VCPSIZ];	/* Table of VCPs.		*/

   short rdccon[VCPMAX][ECUTMAX];	/* Table of RPG Elev Indices.	*/

   short alwblprf[VCPMAX][MXALWPRF];	/* Table of Allowable PRFs 	*/
  
   float prfvalue[PRFMAX];		/* Table of PRF's, in Hertz. 	*/

   short vcp_times[VCPMAX];	   	/* Table of VCP times, in seconds. */

   int   unambigr[DELPRI_MAX][PRFMAX];	/* Table of Unambiguous Ranges. */

   int   delta_pri;			/* Index into PRF table.        */

   int   rdacnt_last;			/* Stucture end identifier.	*/

} Rdacnt;

/*  Define a structure for the allowable prf table	*/

typedef struct {

    short	vcp_num;
			/* VCP number					*/
    short	num_alwbl_prf;
			/* Number of allowable prfs for this VCP	*/
    short	prf_num [PRFMAX];
			/* Allowable prf numbers for this VCP		*/
    short	pulse_cnt [ECUTMAX][NAPRFELV];
			/* Pulse counts for each allowable prf and	*
			 * elevation in this VCP.  Element NAPRFELV	*
			 * is the defalut prf number.			*/

} Vcp_alwblprf_t;

/* Offsets into the pulse_cnt array of Vcp_alwblprf_t structure. */

#define NUM1PULS       0         /* Pulse count for PRF # = 1 */
#define NUM2PULS       1         /* Pulse count for PRF # = 2 */
#define NUM3PULS       2         /* Pulse count for PRF # = 3 */
#define NUM4PULS       3         /* Pulse count for PRF # = 4 */
#define NUM5PULS       4         /* Pulse count for PRF # = 5 */
#define NUM6PULS       5         /* Pulse count for PRF # = 6 */
#define NUM7PULS       6         /* Pulse count for PRF # = 7 */
#define NUM8PULS       7         /* Pulse count for PRF # = 8 */
#define DEFLTPRF       8         /* Default PRF number */


#endif
