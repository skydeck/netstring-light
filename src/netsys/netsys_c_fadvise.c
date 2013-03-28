/* $Id: netsys_c_fadvise.c 1691 2012-02-05 18:29:36Z gerd $ */

#include "netsys_c.h"

#ifdef HAVE_POSIX_FADVISE
#include <sys/fcntl.h>
#endif

/**********************************************************************/
/* POSIX fadvise                                                      */
/**********************************************************************/

/* A lately added POSIX function */

CAMLprim value netsys_have_posix_fadvise(value dummy) {
#ifdef HAVE_POSIX_FADVISE
    return Val_bool(1);
#else
    return Val_bool(0);
#endif
}

CAMLprim value netsys_fadvise(value fd, value start, value len, value adv) {
#ifdef HAVE_POSIX_FADVISE
    int adv_int, r;
    int64 start_int, len_int;
    off_t start_off, len_off;
    /* Att: off_t might be 64 bit even on 32 bit systems! */

    adv_int = 0;
    switch (Int_val(adv)) {
    case 0: case 6: adv_int = POSIX_FADV_NORMAL; break;
    case 1: case 7: adv_int = POSIX_FADV_SEQUENTIAL; break;
    case 2: case 8: adv_int = POSIX_FADV_RANDOM; break;
    case 3: case 9: adv_int = POSIX_FADV_NOREUSE; break;
    case 4: case 10: adv_int = POSIX_FADV_WILLNEED; break;
    case 5: case 11: adv_int = POSIX_FADV_DONTNEED; break;
    default: invalid_argument("Netsys.fadvise");
    };

    start_int = Int64_val(start);
    len_int = Int64_val(len);

    if ( ((int64) ((off_t) start_int)) != start_int )
	failwith("Netsys.fadvise: large files not supported on this OS");
    if ( ((int64) ((off_t) len_int)) != len_int )
	failwith("Netsys.fadvise: large files not supported on this OS");

    start_off = start_int;
    len_off = len_int;

    r = posix_fadvise(Int_val(fd), start_off, len_off, adv_int);
    if (r == -1) 
	uerror("posix_fadvise64", Nothing);
    return Val_unit;
#else
    invalid_argument("Netsys.fadvise not available");
#endif
}

