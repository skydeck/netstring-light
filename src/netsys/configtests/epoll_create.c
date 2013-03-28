#define _GNU_SOURCE

#include <unistd.h>
#include <time.h>
#include <sys/epoll.h>

#include "caml/mlvalues.h"
#include "caml/alloc.h"

/* reminder: we return the exit code, and 0 means success */

value check(value dummy) {
    int code;
    code = epoll_create(1);
    return Val_int(code >= 0 ? 0 : 1);
}
