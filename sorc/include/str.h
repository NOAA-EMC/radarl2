
/*******************************************************************

    Description: Public header file for STR string manipulation 
    functions

*******************************************************************/

/* 
 * RCS info
 * $Author: eforren
 * $Locker:  $
 * $Date: 2005/02/28 16:33:31 $
 * $Id: str.h,v 1.5 2005/02/28 16:33:31 jing Exp $
 * $Revision: 1.5 $
 * $State: Exp $
 */  

#ifndef STR_H
#define STR_H

#ifdef __cplusplus
extern "C"
{
#endif


char *STR_grow (char *str, int length);
char *STR_create (const char *orig_str);
char *STR_copy (char *dest_str, const char *src_str);
char *STR_cat (char *dest_str, const char *src_str);
void STR_free (char *str);
char *STR_append (char *dest_str, const char *src_str, int size);
char *STR_replace (char *str, int offset, int len, char *c_str, int size);
int STR_size (char *str);
char *STR_reset (char *str, int length);
char *STR_gen (char *str, ...);


#ifdef __cplusplus
}
#endif

#endif		
