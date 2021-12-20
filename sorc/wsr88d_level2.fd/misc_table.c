#include <config.h>
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/time.h>
#include <time.h>

#include <misc.h>
#include <en.h>

static int Malloc_retry = 0;
/************************************************************************

    Malloc function with error checking. The caller of this function does
    not need to perform exception handing. When the requested memory
    resource is not available, this function either tries or terminates
    the process depending on "Malloc_retry" set by MISC_malloc_retry.
    This function prints the size to be allocated and the process stack
    before terminating the process. This function is event-safe.

************************************************************************/

void *MISC_malloc (size_t size) {
    char *p;
    char out_buf[512];

    while ((p = (char *)malloc (size)) == NULL) {
        if (Malloc_retry &&  (int) size >= 0)
            sleep (1);
        else
            break;
    }
    if (p == NULL) {
        printf("Error - MISC: Malloc %d bytes failed - stack:\n%s", (int) size, out_buf);
/*  Modified by Guan
        MISC_proc_printstack (getpid (), 512, out_buf);
        MISC_log ("MISC: Malloc %d bytes failed - stack:\n%s", size, out_buf);
*/
        exit (1);
    }
    return ((void *)p);
}

/************************************************************************

    Event-safe free.

************************************************************************/

void MISC_free (void *p) {

    free (p);
}


