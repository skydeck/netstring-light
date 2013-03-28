#define _GNU_SOURCE

#include <errno.h>
#include <unistd.h>
#include <semaphore.h>

#include "caml/mlvalues.h"
#include "caml/alloc.h"

/* reminder: we return the exit code, and 0 means success */

value check(value dummy) {
    sem_t s;
    int code;

    sem_unlink("/foo_khfkshdfhf");
    if (errno == ENOSYS) code = 1; else code = 0;

    return Val_int(code);
}
