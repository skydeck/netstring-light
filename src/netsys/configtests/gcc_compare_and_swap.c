#define _GNU_SOURCE

#include <unistd.h>
#include <stdlib.h>
#include <limits.h>

#include "caml/mlvalues.h"
#include "caml/alloc.h"

/* reminder: we return the exit code, and 0 means success */

/* This builtin is also provided by other compilers than gcc;
   actually it is an Intel invention.
*/

value check(value dummy) {
    int r;
    int x;
    x = 0;
    r = __sync_bool_compare_and_swap(&x, 0, 1);
    return Val_int(r != 0 && x == 1 ? 0 : 1);
}
