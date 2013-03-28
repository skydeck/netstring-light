#include <errno.h>
#include <sys/types.h>
#include <fcntl.h>
#include <unistd.h>

#include "caml/mlvalues.h"
#include "caml/alloc.h"

/* reminder: we return the exit code, and 0 means success */

value check(value dummy) {
    uid_t uid;
    gid_t gid;

    getpeereid(0, &uid, &gid);
    return Val_int(0);
}
