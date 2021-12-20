/*
 * RCS info
 * $Author: steves $
 * $Locker:  $
 * $Date: 2004/06/14 22:43:41 $
 * $Id: orpgrda_adpt.h,v 1.5 2004/06/14 22:43:41 steves Exp $
 * $Revision: 1.5 $
 * $State: Exp $
 */

#ifndef ORPGRDA_ADPT_H
#define ORPGRDA_ADPT_H


/* System Include Files/Local Include Files */
#include <orda_adpt.h>      

/* Macro definitions for RPG commonly used adaptation data items.
   NOTE: If adding new items, need to add support to ORPGRDA_APDT_get_values() */
#define ORPGRDA_ADPT_DELTA_PRI          1
#define ORPGRDA_ADPT_NBR_EL_SEGMENTS    2
#define ORPGRDA_ADPT_SEG1LIM            3
#define ORPGRDA_ADPT_SEG2LIM            4
#define ORPGRDA_ADPT_SEG3LIM            5
#define ORPGRDA_ADPT_SEG4LIM            6

/* Function prototypes. */
void* ORPGRDA_ADPT_read();
void* ORPGRDA_ADPT_get_data();
int ORPGRDA_ADPT_get_data_value( int item, void *value );

#endif /*DO NOT REMOVE!*/
