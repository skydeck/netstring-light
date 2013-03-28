/* $Id: tclqueue.c 5 2003-10-04 15:36:51Z gerd $
 * ----------------------------------------------------------------------
 *
 */

#include <tcl.h>
#include "caml/mlvalues.h"
#include "caml/memory.h"
#include "caml/alloc.h"
#include "caml/fail.h"
#include "caml/callback.h"

#include "tclqueue.h"

typedef struct _filehandler {
    value callback_fn;
    int fd;
} filehandler;


typedef struct _timerhandler {
    value callback_fn;
    Tcl_TimerToken token;
} timerhandler;



static void file_proc(ClientData cdata, int mask) {
    filehandler *h;
    value r;
    
    h = (filehandler *) cdata;

    r = callback_exn(h->callback_fn, Val_int(0));
    if (Is_exception_result(r)) {
	fprintf(stderr, "In file_proc: Uncaught Ocaml exception\n");
    };
}


value caml_Tcl_CreateFileHandler(value callback_fn, 
				 value file_descriptor,
				 value mask) {
    filehandler *h;
    int m, tcl_m;
    CAMLparam3(callback_fn, file_descriptor, mask);

    h = (filehandler *) (stat_alloc(sizeof(filehandler)));
    /* This must be a malloc'ed data block. */

    register_global_root(&(h->callback_fn));
    h->callback_fn = callback_fn;
    h->fd = Int_val(file_descriptor);

    m = Int_val(mask);

    tcl_m = 0;
    if (m & 1) tcl_m |= TCL_READABLE;
    if (m & 2) tcl_m |= TCL_WRITABLE;
    if (m & 4) tcl_m |= TCL_EXCEPTION;

    Tcl_CreateFileHandler(Int_val(file_descriptor),
			  tcl_m,
			  file_proc,
			  (ClientData) h);

    CAMLreturn((value) h);
}


value caml_Tcl_DeleteFileHandler(value descriptor) {
    filehandler *h;
    CAMLparam1(descriptor);
    
    h = (filehandler *) descriptor;
    Tcl_DeleteFileHandler(h->fd);

    remove_global_root(&(h->callback_fn));

    free(h);

    CAMLreturn(Val_int(0));
}


static void timer_proc(ClientData cdata) {
    timerhandler *h;
    value r;
    
    h = (timerhandler *) cdata;

    r = callback_exn(h->callback_fn, Val_int(0));
    if (Is_exception_result(r)) {
	fprintf(stderr, "In timer_proc: Uncaught Ocaml exception\n");
    };
}


value caml_Tcl_CreateTimerHandler(value callback_fn, 
				  value milliseconds) {
    timerhandler *h;
    CAMLparam2(callback_fn, milliseconds);

    h = (timerhandler *) (stat_alloc(sizeof(timerhandler)));
    /* This must be a malloc'ed data block. */

    register_global_root(&(h->callback_fn));
    h->callback_fn = callback_fn;
    h->token = 
	Tcl_CreateTimerHandler(Int_val(milliseconds),
			       timer_proc,
			       (ClientData) h);

    CAMLreturn((value) h);
}


value caml_Tcl_DeleteTimerHandler(value descriptor) {
    timerhandler *h;
    CAMLparam1(descriptor);
    
    h = (timerhandler *) descriptor;
    Tcl_DeleteTimerHandler(h->token);

    remove_global_root(&(h->callback_fn));

    free(h);

    CAMLreturn(Val_int(0));
}


value caml_return_null (value dummy) {
    return (value) NULL;
}
