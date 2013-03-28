/* Wrappers around data structures.
 * Copyright (C) 2003 Merjis Ltd.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the Free
 * Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 * $Id: wrappers.h,v 1.1 2003/09/07 19:41:14 rwmj Exp $
 */

#ifndef _mod_caml_wrappers_
#define _mod_caml_wrappers_

#include "config.h"

#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

#include <httpd.h>
#include <http_config.h>

#if APACHE2
#define table apr_table_t
#endif

/* Wrap up an arbitrary void pointer in an opaque OCaml object. */
extern value Val_voidptr (void *ptr);

/* Unwrap an arbitrary void pointer from an opaque OCaml object. */
#define Voidptr_val(type,rv) ((type *) Field ((rv), 0))

/* Hide the Apache types in opaque OCaml objects. */
#define Val_table(r) (Val_voidptr ((r)))
#define Table_val(rv) (Voidptr_val (table, (rv)))
#define Val_request_rec(r) (Val_voidptr ((r)))
#define Request_rec_val(rv) (Voidptr_val (request_rec, (rv)))
#define Val_server_rec(r) (Val_voidptr ((r)))
#define Server_rec_val(rv) (Voidptr_val (server_rec, (rv)))
#define Val_conn_rec(r) (Val_voidptr ((r)))
#define Conn_rec_val(rv) (Voidptr_val (conn_rec, (rv)))
#define Val_module_struct(r) (Val_voidptr ((r)))
#define Module_struct_val(rv) (Voidptr_val (struct module_struct, (rv)))
#define Val_cmd_parms(r) (Val_voidptr ((r)))
#define Cmd_parms_val(rv) (Voidptr_val (cmd_parms, (rv)))

/* Wrap up a char * as a string option. */
extern value Val_optstring (const char *);

#endif /* _mod_caml_wrappers_ */
