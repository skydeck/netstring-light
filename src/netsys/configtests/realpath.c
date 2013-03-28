#define _GNU_SOURCE

#include <unistd.h>
#include <stdlib.h>
#include <limits.h>

#include "caml/mlvalues.h"
#include "caml/alloc.h"

/* reminder: we return the exit code, and 0 means success */

/* We only accept here the variant specified by POSIX.1-2008 where
   the second arg can be NULL. Older versions are rejected (usually
   by sigsegv)
*/

value check(value dummy) {
    realpath("/", NULL);
    return Val_int(0);
}
