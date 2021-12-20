
/*******************************************************************

    Module: net.h

    Description: Public header file for the net library.

*******************************************************************/

/* 
 * RCS info
 * $Author: jing $
 * $Locker:  $
 * $Date: 2006/02/10 16:36:06 $
 * $Id: net.h,v 1.6 2006/02/10 16:36:06 jing Exp $
 * $Revision: 1.6 $
 * $State: Exp $
 */  

#ifndef NET_H
#define NET_H

#include <poll.h>


/* net_misc.c return values */
#define NET_DISCONNECTED	-10
#define NET_READ_FAILED		-11
#define NET_WRITE_FAILED	-12
#define NET_MALLOC_FAILED	-13
#define NET_SETSOCKOPT_FAILED	-16
#define NET_FCNTL_FAILED	-17


#ifndef INADDR_NONE
#define INADDR_NONE  0xffffffff		/* for unavailable inet address */
#endif

#ifdef LINUX
#define POLL_IN_FLAGS (POLLIN | POLLPRI)
#define POLL_IN_RFLAGS (POLLIN | POLLPRI | POLLHUP)
#else
#define POLL_IN_FLAGS (POLLIN | POLLPRI | POLLRDBAND | POLLRDNORM)
					/* flags for socket input poll */
#define POLL_IN_RFLAGS (POLL_IN_FLAGS | POLLHUP)
#endif
/* we directly use POLLOUT for output bit */

#ifdef __cplusplus
extern "C"
{
#endif


/*
 * socket/networking routines
 */
char *NET_err_msg ();
int NET_write_socket (int fd, char *data, int len);
int NET_read_socket (int fd, char *data, int len) ;
int NET_find_local_ip_address (unsigned int **add);
unsigned int NET_get_ip_by_name (char *hname);
int NET_get_name_by_ip(unsigned int ip_address, char* name_buf, 
						int name_buf_len);
int NET_set_TCP_NODELAY (int fd);
int NET_set_SO_REUSEADDR (int fd);
int NET_set_non_block (int fd);
int NET_set_linger_off (int fd);
int NET_set_linger_on (int fd);
int NET_set_keepalive_on (int fd);
void NET_select_local_ip_address (char *ip);
char *NET_string_IP (unsigned int ip, int need_byte_swap, char *buf);


#ifdef __cplusplus
}
#endif


#endif			/* #ifndef NET_H */
