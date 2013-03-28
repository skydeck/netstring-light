#define _GNU_SOURCE

#include <unistd.h>
#include <time.h>
#include <signal.h>

#include "caml/mlvalues.h"
#include "caml/alloc.h"

/* reminder: we return the exit code, and 0 means success */

value check(value dummy) {
    struct sigevent evp;
    timer_t tm;
    int code;

    evp.sigev_notify = SIGEV_NONE;
    code = timer_create(CLOCK_REALTIME, &evp, &tm);
    return Val_int(code == 0 ? 0 : 1);
}
