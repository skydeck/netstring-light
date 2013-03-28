#define _GNU_SOURCE

#include <unistd.h>
#include <stdlib.h>
#include <grp.h>

#include "caml/mlvalues.h"
#include "caml/alloc.h"

/* reminder: we return the exit code, and 0 means success */

value check(value dummy) {
    initgroups("foo", 0);
    return Val_int(0);
}
