#include <config.h>
#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <time.h>
#include <stdarg.h>
#include <unistd.h>
#include <sys/utsname.h>
#include <string.h>
#include <signal.h>
#include <errno.h>

#include <infr.h>
#include <le_def.h>

static int Line_num = 0;        /* source line number; 0 indicates that the
                                   source file info is not available. */
static char *Src_name = "";     /* source file name */
static int Current_VL = 0;              /* current process verbose level */
static int In_callback = 0;     /* in LE callback function */
static int Process_saved_from_cb = 0;
                                /* processing saved msg send from callback */

/********************************************************************

    Description: This function accepts the source file name and the
                line number.

    Inputs:     file - source file name.
                line_num - line number where LE_send_msg is called.

    Return:     This function returns LE_CRITICAL_BIT.

********************************************************************/

unsigned int LE_file_line (char *file, int line_num)
{

    Src_name = file;
    Line_num = line_num;
    return (LE_CRITICAL_BIT);
}

/********************************************************************

    Description: See LE manpage. This function calls Send_msg_vargs
                to perform the work such that the latter can be called
                directly.

    Input:      code - an integer message code.
                format - a format string used for generating the
                        message. The format is used as the same way
                        as that used in printf.
                ... - the variable list.

********************************************************************/

void LE_send_msg (int code, const char *format, ...)
{
    static int call_level = 0;
    va_list args;
    int mvl;
    
    printf("%s\n",format);
/* modified by Guan
    if (Le_disabled && EN_control (EN_GET_IN_CALLBACK) == 0)
        return;
*/

    /* check verbose level */
    mvl = LE_GET_MVL (code);
    if (mvl > Current_VL && !(code & LE_CRITICAL_BIT))
        return;

    if (format != NULL &&
        strcmp (format, "%ms") == 0 &&
        call_level == 0) {      /* process a multiple line string */
        char *str, *p, buf[LE_MAX_MSG_LENGTH];
        va_start (args, format);
        str = va_arg (args, char *);
        va_end (args);
        while (1) {
            int len;
            p = strstr (str, "\n");
            if (p == NULL)
                p = str + strlen (str);
            len = p - str;
            if (len > 0) {
                if (len >= LE_MAX_MSG_LENGTH)
                    len = LE_MAX_MSG_LENGTH;
                strncpy (buf, str, len);
                buf[len] = '\0';
                call_level++;
                LE_send_msg (code, "%s", buf);
                printf("%s", buf);  /* modified by Guan */
                call_level--;
            }
            if (*p == '\0')
                break;
            str = p + 1;
        }
        return;
    }

    if (In_callback && Process_saved_from_cb)
        return;

    va_start (args, format);
/* Modified by Guan
    Send_msg_vargs (code, format, args);
*/
    va_end (args);
    return;
}


