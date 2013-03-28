/* $Id: tclqueue.h 5 2003-10-04 15:36:51Z gerd $
 * ----------------------------------------------------------------------
 *
 */

#ifndef TCLQUEUE_H
#define TCLQUEUE_H


value caml_Tcl_CreateFileHandler(value callback_fn, 
				 value file_descriptor,
				 value mask);
/* callback_fn: Caml closure.
 * file_descriptor: a Unix.file_descr. 
 * mask: the condition to wait for
 * Returns: A descriptor describing the handler.
 * Note that the closure is registered as global roots until
 * DeleteFileHandler is called.
 */

value caml_Tcl_DeleteFileHandler(value descriptor);
/* descriptor: the descriptor returned by CreateFileHandler.
 * returns: ()
 */

value caml_return_null(value dummy);
/* Returns a Null pointer */

value caml_Tcl_CreateTimerHandler(value callback_fn, 
				  value milliseconds);
/* callback_fn: Caml closure
 * milliseconds: How long to wait.
 * retunrs: A descriptor describing the handler
 */

value caml_Tcl_DeleteTimerHandler(value descriptor);
/* descriptor: the descriptor returned by CreateTimerHandler.
 * returns: ()
 */

#endif
