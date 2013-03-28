#define _GNU_SOURCE

#include <unistd.h>
#include <time.h>

#include "caml/mlvalues.h"
#include "caml/alloc.h"

/* reminder: we return the exit code, and 0 means success */

value check(value dummy) {
    struct timespec ts;
    int r;
    clock_gettime(CLOCK_REALTIME, &ts);
    if (r == -1) Val_int(1);
    return Val_int(0);
}
