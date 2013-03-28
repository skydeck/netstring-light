#define _GNU_SOURCE

#include <unistd.h>
#include <stdlib.h>
#include <limits.h>
#include <sys/timerfd.h>
#include <time.h>

#include "caml/mlvalues.h"
#include "caml/alloc.h"

/* reminder: we return the exit code, and 0 means success */

value check(value dummy) {
    int code;
    code = timerfd_create(CLOCK_REALTIME,0);
    return Val_int(code >= 0 ? 0 : 1);
}
