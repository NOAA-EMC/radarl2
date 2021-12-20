/*
 * RCS info
 * $Author: steves $
 * $Locker:  $
 * $Date: 2005/03/21 23:26:55 $
 * $Id: orpgcmp.h,v 1.3 2005/03/21 23:26:55 steves Exp $
 * $Revision: 1.3 $
 * $State: Exp $
 */

/************************************************************************
 *									*
 *	Module:  orpgcmp.h						*
 *									*
 * 	Description:							*
 *	   This is the global include file for ORPGPAT.			*
 *									*
 ************************************************************************/




#ifndef ORPGCMP_H

#define ORPGCMP_H

typedef struct{

   int code;
   int orig_len;
   int comp_len;
   int magic_num;
   int spare1;
   int spare2;

} ORPGCMP_hdr_t;
   
#define ORPGCMP_MAGIC_NUM       192837465

/* Data compression type macro definitions. */
#define COMPRESSION_NONE        0
#define COMPRESSION_BZIP2       1

/* Macro definitions for bzip2 compression */
#define BZIP2_MIN_BYTES_TO_COMPRESS    1000  /* minimum bytes to compress */
#define BZIP2_MIN_BLOCK_SIZE_BYTES   100000  /* corresponds to 100 Kbytes */
#define BZIP2_MIN_BLOCK_SIZE              1  /* corresponds to 100 Kbytes */
#define BZIP2_MAX_BLOCK_SIZE              5  /* corresponds to 500 Kbytes */
#define BZIP2_WORK_FACTOR                30  /* the recommended default */
#define BZIP2_NOT_VERBOSE                 0  /* turns off verbosity */
#define BZIP2_NOT_SMALL                   0  /* does not use small version */


#define ORPGCMP_ERROR	       		-800 
#define ORPGCMP_MALLOC_FAILED		-801
#define ORPGCMP_BAD_COMPRESSION_CODE	-802
#define ORPGCMP_BZIP2_INTERNAL_ERROR	-803
#define ORPGCMP_CODE_MISMATCH       	-804
#define ORPGCMP_ZLIB_INTERNAL_ERROR	-805

/*	Functions prototypes */

int ORPGCMP_compress( int code, char *src, int src_len, 
                      char **dest );
int ORPGCMP_decompress( int code, char *src, int src_len, 
                        char **dest );


#endif
