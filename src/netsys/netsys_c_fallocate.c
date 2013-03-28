/* $Id: netsys_c_fallocate.c 1691 2012-02-05 18:29:36Z gerd $ */

#include "netsys_c.h"

/**********************************************************************/
/* POSIX fallocate                                                    */
/**********************************************************************/

/* A lately added POSIX function */

CAMLprim value netsys_have_posix_fallocate(value dummy) {
#ifdef HAVE_POSIX_FALLOCATE
    return Val_bool(1);
#else
    return Val_bool(0);
#endif
}


CAMLprim value netsys_fallocate(value fd, value start, value len) {
#ifdef HAVE_POSIX_FALLOCATE
    int r;
    int64 start_int, len_int;
    off_t start_off, len_off;
    /* Att: off_t might be 64 bit even on 32 bit systems! */

    start_int = Int64_val(start);
    len_int = Int64_val(len);

    if ( ((int64) ((off_t) start_int)) != start_int )
	failwith("Netsys.fadvise: large files not supported on this OS");
    if ( ((int64) ((off_t) len_int)) != len_int )
	failwith("Netsys.fadvise: large files not supported on this OS");

    start_off = start_int;
    len_off = len_int;

    r = posix_fallocate(Int_val(fd), start_off, len_off);
    /* does not set errno! */
    if (r != 0) 
	unix_error(r, "posix_fallocate64", Nothing);
    return Val_unit;
#else
    invalid_argument("Netsys.fallocate not available");
#endif
}

