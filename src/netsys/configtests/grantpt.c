#define _GNU_SOURCE

#include <unistd.h>
#include <stdlib.h>

#include "caml/mlvalues.h"
#include "caml/alloc.h"

/* reminder: we return the exit code, and 0 means success */

value check(value dummy) {
    close(0);
    grantpt(0);
    return Val_int(0);
}
