/* Apache "mod" interface.
 * Copyright (C) 2003 Merjis Ltd.
 * Copyright (C) 2007 Christophe Troestler
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
 */

/* This is a fork from mod_caml cleaned up and tailored to Netcgi2. */

#include "config.h"

#include <stdio.h>
#include <sys/stat.h>

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/fail.h>
#include <caml/memory.h>

#include <httpd.h>
#include <http_config.h>
#include <http_protocol.h>
#include <http_request.h>

#if APACHE2
#include <apr_strings.h>
#include <apr_file_info.h>
#include <apr_time.h>
#endif

#include "wrappers.h"

#if !(APACHE2)
/* Apache 1 */
#define apr_pstrdup ap_pstrdup
#define apr_palloc  ap_palloc
#define apr_table_clear ap_clear_table
#define apr_table_get ap_table_get
#define apr_table_set ap_table_set
#define apr_table_unset ap_table_unset
#define apr_table_add ap_table_add
#define apr_table_do ap_table_do
#endif

extern module netcgi_module;


/*----- Tables. -----*/

CAMLprim value
netcgi2_apache_table_clear(value tv)
{
  CAMLparam1(tv);
  table *t = Table_val(tv);
  apr_table_clear(t);
  CAMLreturn(Val_unit);
}

CAMLprim value
netcgi2_apache_table_get (value tv, value str)
{
  CAMLparam2(tv, str);
  table *t = Table_val (tv);
  const char *res = apr_table_get(t, String_val (str));
  if (res)
    CAMLreturn (copy_string (res));
  else
    raise_not_found ();
}

static int
netcgi2_apache_table_get_loop(void *res, const char *key, const char *val)
{
  CAMLparam0();
  CAMLlocal1(cons); /* head cell of new list */
  cons = alloc(2, 0); /* :: */
  Store_field(cons, 0, copy_string(val)); /* value :: */
  Store_field(cons, 1, *((value *) res)); /* :: previous list */
  *((value *) res) = cons;
  CAMLreturn(1);
}

CAMLprim value
netcgi2_apache_table_get_all(value tv, value str)
{
  CAMLparam2(tv, str);
  CAMLlocal1(res); /* list */
  table *t = Table_val(tv);
  char *key = String_val(str);
  res = Val_int(0); /* empty list [] */

  /* Only iterates over values associated with [key]. */
  apr_table_do(&netcgi2_apache_table_get_loop, &res, t, key, NULL);
  CAMLreturn(res);
}



static int
netcgi2_apache_table_fields_loop(void *res, const char *key, const char *val)
{
  CAMLparam0();
  CAMLlocal2(cons, pair);
  pair = alloc_tuple(2); /* (,) */
  Store_field(pair, 0, copy_string(key));
  Store_field(pair, 1, copy_string(val));
/*   fprintf(stderr, "(%s, %s)\n", key, val); /\* DEBUG *\/ */
  cons = alloc(2, 0); /* :: */
  Store_field(cons, 0, pair); /* pair :: */
  Store_field(cons, 1, *((value *) res)); /* :: list */
  *((value *) res) = cons;
  CAMLreturn(1);
}

CAMLprim value
netcgi2_apache_table_fields(value tv)
{
  CAMLparam1(tv);
  CAMLlocal1(res); /* list */
  table *t = Table_val(tv);
  res = Val_int(0); /* empty list [] */

  /* Only iterates over *all* values of the table. */
  apr_table_do(&netcgi2_apache_table_fields_loop, &res, t, NULL);
  CAMLreturn(res);
}


CAMLprim value
netcgi2_apache_table_set (value tv, value key, value val)
{
  CAMLparam3 (tv, key, val);
  table *t = Table_val (tv);
  apr_table_set(t, String_val (key), String_val (val));
  CAMLreturn (Val_unit);
}

CAMLprim value
netcgi2_apache_table_add (value tv, value key, value val)
{
  CAMLparam3 (tv, key, val);
  table *t = Table_val (tv);
  apr_table_add (t, String_val (key), String_val (val));
  CAMLreturn (Val_unit);
}

CAMLprim value
netcgi2_apache_table_unset (value tv, value key)
{
  CAMLparam2 (tv, key);
  table *t = Table_val (tv);
  apr_table_unset (t, String_val (key));
  CAMLreturn (Val_unit);
}

/*----- Server structure. -----*/

CAMLprim value
netcgi2_apache_server_hostname (value sv)
{
  CAMLparam1(sv);
  server_rec *s = Server_rec_val(sv);
  if (s->server_hostname)
    CAMLreturn(copy_string(s->server_hostname));
  else
    raise_not_found ();
}

CAMLprim value
netcgi2_apache_server_admin(value sv)
{
  CAMLparam1(sv);
  server_rec *s = Server_rec_val(sv);
  if (s->server_admin)
    CAMLreturn(copy_string(s->server_admin));
  else
    raise_not_found();
}

CAMLprim value
netcgi2_apache_server_is_virtual(value sv)
{
  CAMLparam1(sv);
  server_rec *s = Server_rec_val(sv);
  CAMLreturn(Val_bool(s->is_virtual));
}

/*----- Connection structure. -----*/

#define CONNECTION(field) \
CAMLprim value                                  \
netcgi2_apache_connection_ ## field (value cv)        \
{                                               \
  CAMLparam1 (cv);                              \
  conn_rec *c = Conn_rec_val (cv);              \
  if (c->field)                                 \
    CAMLreturn (copy_string (c->field));        \
  else                                          \
    raise_not_found ();                         \
}

CONNECTION(remote_ip)
CONNECTION(remote_host)

/*----- Request structure. -----*/

CAMLprim value
netcgi2_apache_request_connection (value rv)
{
  CAMLparam1 (rv);
  request_rec *r = Request_rec_val (rv);
  CAMLreturn (Val_conn_rec (r->connection));
}

CAMLprim value
netcgi2_apache_request_server (value rv)
{
  CAMLparam1 (rv);
  request_rec *r = Request_rec_val (rv);
  CAMLreturn (Val_server_rec (r->server));
}

CAMLprim value
netcgi2_apache_request_next (value rv)
{
  CAMLparam1 (rv);
  request_rec *r = Request_rec_val (rv), *rr = r->next;
  if (rr)
    CAMLreturn (Val_request_rec (rr));
  else
    raise_not_found ();
}

CAMLprim value
netcgi2_apache_request_prev (value rv)
{
  CAMLparam1 (rv);
  request_rec *r = Request_rec_val (rv), *rr = r->prev;
  if (rr)
    CAMLreturn (Val_request_rec (rr));
  else
    raise_not_found ();
}

CAMLprim value
netcgi2_apache_request_main (value rv)
{
  CAMLparam1 (rv);
  request_rec *r = Request_rec_val (rv), *rr = r->main;
  if (rr)
    CAMLreturn (Val_request_rec (rr));
  else
    raise_not_found ();
}

CAMLprim value
netcgi2_apache_request_the_request (value rv)
{
  CAMLparam1 (rv);
  request_rec *r = Request_rec_val (rv);
  if (r->the_request)
    CAMLreturn (copy_string (r->the_request));
  else
    raise_not_found ();
}

CAMLprim value
netcgi2_apache_request_assbackwards (value rv)
{
  CAMLparam1 (rv);
  request_rec *r = Request_rec_val (rv);
  CAMLreturn (Val_bool (r->assbackwards));
}

CAMLprim value
netcgi2_apache_request_header_only (value rv)
{
  CAMLparam1 (rv);
  request_rec *r = Request_rec_val (rv);
  CAMLreturn (Val_bool (r->header_only));
}

CAMLprim value
netcgi2_apache_request_protocol (value rv)
{
  CAMLparam1 (rv);
  request_rec *r = Request_rec_val (rv);
  if (r->protocol)
    CAMLreturn (copy_string (r->protocol));
  else
    raise_not_found ();
}

CAMLprim value
netcgi2_apache_request_proto_num (value rv)
{
  CAMLparam1 (rv);
  request_rec *r = Request_rec_val (rv);
  CAMLreturn (Val_int (r->proto_num));
}

CAMLprim value
netcgi2_apache_request_hostname (value rv)
{
  CAMLparam1 (rv);
  request_rec *r = Request_rec_val (rv);
  if (r->hostname)
    CAMLreturn (copy_string (r->hostname));
  else
    raise_not_found ();
}

CAMLprim value
netcgi2_apache_request_request_time (value rv)
{
  CAMLparam1 (rv);
  request_rec *r = Request_rec_val (rv);
  CAMLreturn (copy_double ((double) r->request_time));
}

CAMLprim value
netcgi2_apache_request_status_line (value rv)
{
  CAMLparam1 (rv);
  request_rec *r = Request_rec_val (rv);
  if (r->status_line)
    CAMLreturn (copy_string (r->status_line));
  else
    raise_not_found ();
}

CAMLprim value
netcgi2_apache_request_set_status_line (value rv, value str)
{
  CAMLparam2 (rv, str);
  request_rec *r = Request_rec_val (rv);
  r->status_line = apr_pstrdup (r->pool, String_val (str));
  CAMLreturn (Val_unit);
}

CAMLprim value
netcgi2_apache_request_status (value rv)
{
  CAMLparam1 (rv);
  request_rec *r = Request_rec_val (rv);
  CAMLreturn (Val_int (r->status));
}

CAMLprim value
netcgi2_apache_request_set_status (value rv, value i)
{
  CAMLparam2 (rv, i);
  request_rec *r = Request_rec_val (rv);
  r->status = Int_val (i);
  CAMLreturn (Val_unit);
}

CAMLprim value
netcgi2_apache_request_method (value rv)
{
  CAMLparam1 (rv);
  request_rec *r = Request_rec_val (rv);
  CAMLreturn (copy_string (r->method));
}

CAMLprim value
netcgi2_apache_request_method_number (value rv)
{
  CAMLparam1 (rv);
  request_rec *r = Request_rec_val (rv);
  CAMLreturn (Val_int (r->method_number));
}

CAMLprim value
netcgi2_apache_request_headers_in (value rv)
{
  CAMLparam1 (rv);
  request_rec *r = Request_rec_val (rv);
  CAMLreturn (Val_table (r->headers_in));
}

CAMLprim value
netcgi2_apache_request_headers_out (value rv)
{
  CAMLparam1 (rv);
  request_rec *r = Request_rec_val (rv);
  CAMLreturn (Val_table (r->headers_out));
}

CAMLprim value
netcgi2_apache_request_err_headers_out (value rv)
{
  CAMLparam1 (rv);
  request_rec *r = Request_rec_val (rv);
  CAMLreturn (Val_table (r->err_headers_out));
}

CAMLprim value
netcgi2_apache_request_subprocess_env (value rv)
{
  CAMLparam1 (rv);
  request_rec *r = Request_rec_val (rv);
  CAMLreturn (Val_table (r->subprocess_env));
}

CAMLprim value
netcgi2_apache_request_notes (value rv)
{
  CAMLparam1 (rv);
  request_rec *r = Request_rec_val (rv);
  CAMLreturn (Val_table (r->notes));
}

CAMLprim value
netcgi2_apache_request_content_type (value rv)
{
  CAMLparam1 (rv);
  request_rec *r = Request_rec_val (rv);
  if (r->content_type)
    CAMLreturn (copy_string (r->content_type));
  else
    raise_not_found ();
}

CAMLprim value
netcgi2_apache_request_set_content_type (value rv, value str)
{
  CAMLparam2 (rv, str);
  request_rec *r = Request_rec_val (rv);
  r->content_type = apr_pstrdup (r->pool, String_val (str));
  CAMLreturn (Val_unit);
}

CAMLprim value
netcgi2_apache_request_user (value rv)
{
  CAMLparam1 (rv);
  request_rec *r = Request_rec_val (rv);
#if APACHE2
  if (r->user)
    CAMLreturn (copy_string (r->user));
#else
  if (r->connection->user)
    CAMLreturn (copy_string (r->connection->user));
#endif
  else
    raise_not_found ();
}

CAMLprim value
netcgi2_apache_request_uri (value rv)
{
  CAMLparam1 (rv);
  request_rec *r = Request_rec_val (rv);
  if (r->uri)
    CAMLreturn (copy_string (r->uri));
  else
    raise_not_found ();
}

CAMLprim value
netcgi2_apache_request_port(value rv)
{
  CAMLparam1(rv);
  request_rec *r = Request_rec_val (rv);

  CAMLreturn(Val_int(ap_get_server_port(r)));
}


CAMLprim value
netcgi2_apache_request_set_uri (value rv, value str)
{
  CAMLparam2 (rv, str);
  request_rec *r = Request_rec_val (rv);
  r->uri = apr_pstrdup (r->pool, String_val (str));
  CAMLreturn (Val_unit);
}

CAMLprim value
netcgi2_apache_request_filename (value rv)
{
  CAMLparam1 (rv);
  request_rec *r = Request_rec_val (rv);
  if (r->filename)
    CAMLreturn (copy_string (r->filename));
  else
    raise_not_found ();
}

CAMLprim value
netcgi2_apache_request_set_filename (value rv, value str)
{
  CAMLparam2 (rv, str);
  request_rec *r = Request_rec_val (rv);
  r->filename = apr_pstrdup (r->pool, String_val (str));
  CAMLreturn (Val_unit);
}

CAMLprim value
netcgi2_apache_request_path_info (value rv)
{
  CAMLparam1 (rv);
  request_rec *r = Request_rec_val (rv);
  if (r->path_info)
    CAMLreturn (copy_string (r->path_info));
  else
    raise_not_found ();
}

CAMLprim value
netcgi2_apache_request_set_path_info (value rv, value str)
{
  CAMLparam2 (rv, str);
  request_rec *r = Request_rec_val (rv);
  r->path_info = apr_pstrdup (r->pool, String_val (str));
  CAMLreturn (Val_unit);
}

CAMLprim value
netcgi2_apache_request_args (value rv)
{
  CAMLparam1 (rv);
  request_rec *r = Request_rec_val (rv);
  if (r->args)
    CAMLreturn (copy_string (r->args));
  else
    raise_not_found ();
}

CAMLprim value
netcgi2_apache_request_set_args (value rv, value str)
{
  CAMLparam2 (rv, str);
  request_rec *r = Request_rec_val (rv);
  r->args = apr_pstrdup (r->pool, String_val (str));
  CAMLreturn (Val_unit);
}

#if APACHE2
static int file_kind_table[] = {
  APR_REG, APR_DIR, APR_CHR, APR_BLK, APR_LNK, APR_PIPE, APR_SOCK
};
#else
static int file_kind_table[] = {
  S_IFREG, S_IFDIR, S_IFCHR, S_IFBLK, S_IFLNK, S_IFIFO, S_IFSOCK
};
#endif

static value cst_to_constr (int n, int *tbl, int size, int deflt)
{
  int i;
  for (i = 0; i < size; i++)
    if (n == tbl[i]) return Val_int(i);
  return Val_int(deflt);
}

CAMLprim value netcgi2_apache_request_finfo (value rv)
{
  CAMLparam1 (rv);
  request_rec *r = Request_rec_val (rv);
  CAMLlocal5 (v, sb, atime, mtime, ctime);

#if APACHE2
  if (r->finfo.filetype != APR_NOFILE) /* Some statbuf */
    {
      atime = (r->finfo.valid & APR_FINFO_ATIME) ?
	copy_double ((double) apr_time_sec (r->finfo.atime)) :
	copy_double (0.);
      mtime = (r->finfo.valid & APR_FINFO_MTIME) ?
	copy_double ((double) apr_time_sec (r->finfo.mtime)) :
	copy_double (0.);
      ctime = (r->finfo.valid & APR_FINFO_CTIME) ?
	copy_double ((double) apr_time_sec (r->finfo.ctime)) :
	copy_double (0.);

      sb = alloc_small (12, 0);
      Field (sb, 0) = Val_int (r->finfo.device);
      Field (sb, 1) = Val_int (r->finfo.inode);
      Field (sb, 2) =
	cst_to_constr (r->finfo.filetype, file_kind_table,
		       sizeof (file_kind_table) / sizeof (int), 0);
      Field (sb, 3) = Val_int (r->finfo.protection);
      Field (sb, 4) = Val_int (r->finfo.nlink);
      Field (sb, 5) = Val_int (r->finfo.user);
      Field (sb, 6) = Val_int (r->finfo.group);
      Field (sb, 7) = Val_int (0); /* FIXME rdev? */
      Field (sb, 8) = Val_int (r->finfo.size); /* FIXME 64 bit file offsets */

      Field (sb, 9) = atime;
      Field (sb, 10) = mtime;
      Field (sb, 11) = ctime;

      v = alloc (1, 0);		/* The "Some" block. */
      Field (v, 0) = sb;
    }
else
    v = Val_int (0);		/* None. */

#else /* not APACHE2 */

  if (r->finfo.st_mode)		/* Some statbuf */
    {
      /* This code copied and modified from otherlibs/unix/stat.c. */
      atime = copy_double ((double) r->finfo.st_atime);
      mtime = copy_double ((double) r->finfo.st_mtime);
      ctime = copy_double ((double) r->finfo.st_ctime);

      sb = alloc_small (12, 0);
      Field (sb, 0) = Val_int (r->finfo.st_dev);
      Field (sb, 1) = Val_int (r->finfo.st_ino);
      Field (sb, 2) =
	cst_to_constr (r->finfo.st_mode & S_IFMT, file_kind_table,
		       sizeof (file_kind_table) / sizeof (int), 0);
      Field (sb, 3) = Val_int (r->finfo.st_mode & 07777);
      Field (sb, 4) = Val_int (r->finfo.st_nlink);
      Field (sb, 5) = Val_int (r->finfo.st_uid);
      Field (sb, 6) = Val_int (r->finfo.st_gid);
      Field (sb, 7) = Val_int (r->finfo.st_rdev);
      Field (sb, 8) = Val_int (r->finfo.st_size); /* FIXME: 64 bit file offsets */
      Field (sb, 9) = atime;
      Field (sb, 10) = mtime;
      Field (sb, 11) = ctime;

      v = alloc (1, 0);		/* The "Some" block. */
      Field (v, 0) = sb;
    }
  else
    v = Val_int (0);		/* None. */
#endif /* not APACHE2 */

  CAMLreturn (v);
}

CAMLprim value
netcgi2_apache_request_send_http_header (value rv)
{
  CAMLparam1 (rv);
#if APACHE2
  /* FIXME do nothing in Apache 2.x? */
#else
  request_rec *r = Request_rec_val (rv);
  ap_send_http_header (r);
#endif
  CAMLreturn (Val_unit);
}

CAMLprim value
netcgi2_apache_request_rflush (value rv)
{
  CAMLparam1 (rv);
  request_rec *r = Request_rec_val (rv);
  int i = ap_rflush (r);
  CAMLreturn (Val_int (i));
}

CAMLprim value
netcgi2_apache_request_setup_client_block (value rv, value rp)
{
  CAMLparam2 (rv, rp);
  request_rec *r = Request_rec_val (rv);
  int i = ap_setup_client_block (r, Int_val (rp));
  CAMLreturn (Val_int(i)); /* possible error dealt with on the Caml side */
}

CAMLprim value
netcgi2_apache_request_should_client_block (value rv)
{
  CAMLparam1 (rv);
  request_rec *r = Request_rec_val (rv);
  CAMLreturn (Val_bool (ap_should_client_block (r)));
}

CAMLprim value
netcgi2_apache_request_get_client_block (value rv)
{
  CAMLparam1 (rv);
  CAMLlocal1 (str);
  request_rec *r = Request_rec_val (rv);
  const int huge_string_len = 8192; /* Same as Apache's HUGE_STRING_LEN. */
  char buffer [huge_string_len];
  int i;

  str = Val_unit;
  i = ap_get_client_block (r, buffer, huge_string_len);
  if (i == -1) {
    /* FIXME: It seems there is now way to get more info about the error. */
    caml_failwith("ap_get_client_block");
  }

  str = alloc_string (i);
  memcpy (String_val (str), buffer, i);

  CAMLreturn (str);
}

CAMLprim value
netcgi2_apache_request_get_client_block_buffered(value rv, value bufv,
                                                 value ofsv, value lenv)
{
  CAMLparam4(rv, bufv, ofsv, lenv);
  request_rec *r = Request_rec_val(rv);
  int ofs = Int_val(ofsv);
  int len = Int_val(lenv);
  int i;

  i = ap_get_client_block (r, String_val(bufv) + ofs, len);
  /* Error dealt with on the Caml side. */
  CAMLreturn(Val_int(i));
}

CAMLprim value
netcgi2_apache_request_discard_request_body (value rv)
{
  CAMLparam1 (rv);
  request_rec *r = Request_rec_val (rv);
  int i = ap_discard_request_body (r);
  CAMLreturn (Val_int(i)); /* possible error dealt with on the Caml side */
}


CAMLprim value netcgi2_apache_auth_type(value rv)
{
  CAMLparam1(rv);
  request_rec *r = Request_rec_val(rv);
#if APACHE2
  if (r->ap_auth_type)
    CAMLreturn(copy_string(r->ap_auth_type));
#else
  if (r->connection->ap_auth_type)
    CAMLreturn(copy_string(r->connection->ap_auth_type));
#endif
  else
    raise_not_found();
}


CAMLprim value
netcgi2_apache_request_note_auth_failure (value rv)
{
  CAMLparam1 (rv);
  request_rec *r = Request_rec_val (rv);
  ap_note_auth_failure (r);
  CAMLreturn (Val_unit);
}

CAMLprim value
netcgi2_apache_request_note_basic_auth_failure (value rv)
{
  CAMLparam1 (rv);
  request_rec *r = Request_rec_val (rv);
  ap_note_basic_auth_failure (r);
  CAMLreturn (Val_unit);
}

CAMLprim value
netcgi2_apache_request_note_digest_auth_failure (value rv)
{
  CAMLparam1 (rv);
  request_rec *r = Request_rec_val (rv);
  ap_note_digest_auth_failure (r);
  CAMLreturn (Val_unit);
}

CAMLprim value
netcgi2_apache_request_get_basic_auth_pw (value rv)
{
  CAMLparam1 (rv);
  CAMLlocal1 (c);
  request_rec *r = Request_rec_val (rv);
  const char *pw = 0;
  int i = ap_get_basic_auth_pw (r, &pw); /* no need to free(pw) */
  /* Return [i] as the first component of a couple so we can deal with
   * the possible errors on the Caml side. */
  if (i == DECLINED) pw = NULL;	/* FIXME */
  c = alloc_tuple (2);
  Store_field(c, 0, Val_int(i));
  Store_field(c, 1, Val_optstring(pw));
  CAMLreturn (c);
}

CAMLprim value
netcgi2_apache_request_internal_redirect (value new_uri, value rv)
{
  CAMLparam2 (new_uri, rv);
  request_rec *r = Request_rec_val (rv);
  ap_internal_redirect (String_val (new_uri), r);
  CAMLreturn (Val_unit);
}

CAMLprim value
netcgi2_apache_request_internal_redirect_handler (value new_uri, value rv)
{
  CAMLparam2 (new_uri, rv);
  request_rec *r = Request_rec_val (rv);
  ap_internal_redirect_handler (String_val (new_uri), r);
  CAMLreturn (Val_unit);
}

CAMLprim value
netcgi2_apache_request_print_char (value rv, value cv)
{
  CAMLparam2 (rv, cv);
  request_rec *r = Request_rec_val (rv);
  int c = Int_val (cv);
  if (ap_rputc (c, r) == EOF)
    raise_sys_error(copy_string("Netcgi_mod#out_channel#output_char"));
  CAMLreturn (Val_unit);
}

CAMLprim value
netcgi2_apache_request_output(value rv, value bufv, value ofsv, value lenv)
{
  CAMLparam4(rv, bufv, ofsv, lenv);
  request_rec *r = Request_rec_val(rv);
  int ofs = Int_val(ofsv);
  int len = Int_val(lenv);
  int i = ap_rwrite(String_val(bufv) + ofs, len, r);
  CAMLreturn(Val_int (i));
}


static int
run_cleanup (void *fv)
{
  value f = *(value *) fv;

  callback (f, Val_unit);
  remove_global_root ((value *) fv);
  return OK;
}

CAMLprim value
netcgi2_apache_request_register_cleanup (value rv, value f)
{
  CAMLparam2 (rv, f);
  request_rec *r = Request_rec_val (rv);
  value *v = (value *) apr_palloc (r->pool, sizeof (value));

  *v = f;
  register_global_root (v);

#if APACHE2
  apr_pool_cleanup_register (r->pool, v, run_cleanup, apr_pool_cleanup_null);
#else
  ap_register_cleanup (r->pool, v,
		       (void (*)(void *)) run_cleanup, ap_null_cleanup);
#endif

  CAMLreturn (Val_unit);
}

/*----- Miscellaneous functions. -----*/

CAMLprim value
netcgi2_apache_get_server_config (value rv)
{
  CAMLparam1 (rv);
  CAMLlocal1 (config);
  request_rec *r = Request_rec_val (rv);
  if (r->server && r->server->module_config) {
    void *c = ap_get_module_config (r->server->module_config, &netcgi_module);
    if (c) config = *(value *) c;
    else goto not_found;
  } else
  not_found:
    raise_not_found ();
  CAMLreturn (config);
}

CAMLprim value
netcgi2_apache_get_dir_config (value rv)
{
  CAMLparam1 (rv);
  CAMLlocal1 (config);
  request_rec *r = Request_rec_val (rv);
  if (r->per_dir_config) {
    void *c = ap_get_module_config (r->per_dir_config, &netcgi_module);
    if (c) config = *(value *) c;
    else goto not_found;
  } else
  not_found:
    raise_not_found ();
  CAMLreturn (config);
}
