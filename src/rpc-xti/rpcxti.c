/* $Id: rpcxti.c 258 2004-05-25 16:49:11Z gerd $
 * ----------------------------------------------------------------------
 *
 */


#include "caml/mlvalues.h"
#include "caml/alloc.h"
#include "caml/memory.h"
#include "caml/fail.h"
#include <unistd.h>
#include <sys/types.h>
#include <string.h>
#include <stdio.h>
#include <xti.h>
#include <fcntl.h>
#include <stropts.h>
#include <errno.h>


/**********************************************************************/
/* From unixsupport.h                                                 */
/**********************************************************************/

#define Nothing ((value) 0)

extern void unix_error (int errcode, char * cmdname, value arg) Noreturn;
extern void uerror (char * cmdname, value arg) Noreturn;

/**********************************************************************/


#define ABUFLEN 1024

void xti_error(int fd, char *cmdname) {
    char error[100];
    int n;

    snprintf(error, 100, "XTI error: %s", t_strerror(t_errno));
    n = t_errno == TSYSERR ? errno : 0;

    if (fd >= 0) t_close(fd);

    unix_error(n, cmdname, copy_string(error));
}


value xti_cots_connect (value device, value addr) {
    CAMLparam2(device,addr);
    int fd;
    char *dev;
    struct t_call sndcall;
    char abuf[ABUFLEN];
    int k;

    dev = String_val(device);

    if ((fd = t_open(dev, O_RDWR, (struct t_info *) NULL))
	== -1) {
	xti_error(-1, "t_open");
    }

    if (t_bind(fd, (struct t_bind *) NULL, (struct t_bind *) NULL)
	== -1) {
	xti_error(fd, "t_bind");
    }

    sndcall.opt.buf = NULL;
    sndcall.opt.len = 0;
    sndcall.opt.maxlen = 0;
    sndcall.udata.buf = NULL;
    sndcall.udata.len = 0;
    sndcall.udata.maxlen = 0;
    sndcall.addr.buf = abuf;
    sndcall.addr.len = 0;
    sndcall.addr.maxlen = ABUFLEN;
    sndcall.sequence = 0;

    if (string_length(addr) > sndcall.addr.maxlen) {
	t_close(fd);
	invalid_argument("cots_connect: address too long");
    };

    sndcall.addr.len = string_length(addr);
    for (k=0; k<string_length(addr); k++) {
	sndcall.addr.buf[k] = Byte(addr,k);
    };
    if (t_connect( fd, &sndcall, (struct t_call *) NULL) == -1 ) {
	xti_error(fd, "t_connect");
    }

    if (ioctl(fd, I_PUSH, "tirdwr") == -1) {
	int e = errno;
	t_close(fd);
	unix_error(e, "ioctl(I_PUSH)", Nothing);
    }

    CAMLreturn(Val_int(fd));
}
