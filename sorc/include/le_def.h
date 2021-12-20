
/*******************************************************************

    Module: le_def.h

    Description: The private header file for the LE module.

*******************************************************************/

/* 
 * RCS info
 * $Author: jing $
 * $Locker:  $
 * $Date: 2006/07/05 16:25:36 $
 * $Id: le_def.h,v 1.9 2006/07/05 16:25:36 jing Exp $
 * $Revision: 1.9 $ 
 * $State: Exp $
 */  

#ifndef LE_DEF_H
#define LE_DEF_H

#define DEFAULT_LB_SIZE 300
#define DEFAULT_LB_TYPE 0

#define UNDEFINED_LB_SIZE -1
#define UNDEFINED_LB_TYPE -1

/* private functions */
void LE_save_message (char *msg);
char *LE_get_saved_msg ();
int LE_get_instance ();
char *LE_get_label ();
int LE_check_and_open_lb (char *arg0);
int LE_set_disable (int yes);
void LE_set_print_src_mask (unsigned int mask);
void LE_set_output_fd (void *fl);


#endif 		/* #ifndef LE_DEF_H */
