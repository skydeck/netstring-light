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
 * $Id: wrappers.c,v 1.1 2003/09/07 19:41:14 rwmj Exp $
 */

#include <caml/fail.h>
#include <caml/mlvalues.h>

#include <httpd.h>

#include "wrappers.h"

value
Val_voidptr (void *ptr)
{
  value rv = alloc (1, Abstract_tag); /* XXX Is this correct? */
  Field(rv, 0) = (value) ptr;
  return rv;
}

value
Val_optstring (const char *str)
{
  if (str)			/* Return "Some str". */
    {
      CAMLparam0();
      CAMLlocal2(rv, s);
      s = copy_string (str);
      rv = alloc_small (1, 0);
      Field(rv, 0) = s;
      CAMLreturn(rv);
    }
  else				/* Return "None". */
    return Val_int (0);
}
