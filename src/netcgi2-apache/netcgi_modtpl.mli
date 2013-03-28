(* netcgi_modtpl.mli

   (C) 2005 Christophe Troestler

   This code may be used under either, the GNU GPL, or the same license
   as ocamlnet (see the file LICENSE).

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
   LICENSE for more details.
*)

(** @deprecated Mod_caml style of templates.  This template module is
 * simple to use.  It is here to provide a simple upgrade path from
 * mod_caml.  It should not be considered as the default template
 * system of Netcgi.  *)


type var =
  | VarString of string				(** ::tag:: *)
    | VarTable of table_row list		(** ::table(tag):: *)
    | VarConditional of bool			(** ::if(tag):: *)
    | VarCallback of (string list -> string)	(** ::call(f, x1,...):: *)
and table_row = (string * var) list

(** Variables are either simple string, tables, conditionals or callbacks.

    A simple string is set with [template#set "name" s] where [s]
    will be automatically escaped depending on the declaration in
    the template:

    - [::name::] does no escaping;
    - [::name_url::] escapes for URL encoding, make it suitable in a
    link [<a href="::name_url::">];
    - [::name_html::] escapes for HTML display of the string;
    - [::name_html_tag::] escapes the string to make it suitable to be
    placed between quotes in an HTML tag, e.g.
    [<input value="::name_html_tag::">];
    - [::name_html_textarea::] escapes the string to make it suitable
    to be placed between [<textarea>...</textarea>].

    Tables are declared in the template by [::table(name)::] {i row
    template} [::end::].  The {i row template} can contain other
    variables.  Calling [template#table "name" rows], where [rows] is
    a list [[row1, row2,...,rowN]], will insert [N] {i row templates}
    with each template having its variables set thanks to [row1],...
    each of which is an associative list name -> value (of type
    {!Template.table_row}).

    Conditionals are declared in the template by [::if(name)
    .. ::else:: .. ::end::] with the "else" clause being optional.
    Calling [template#conditional] sets up a conditional value.

    Calling [template#callback "fname" f] sets up the callback
    function declared by [::call(fname,arg1,...,argN)::] replacing the
    call by the value of [f] applied to the list [[arg1,...,argN]].
    The string returned by [f] can be escaped by using suffices in the
    template as for simple tags: [::call(fname,arg1,...,argN)_html::],...

    A template may also include other templates with [::include(filename)::].
*)


(** [new template ?filename tpl] computes a new template from the
    string [tpl].  Once the object has been created, it can be used
    in a single thread.

    @param filename if set, it is used to determine the base path for
    [::include()::] tags in the template (default: current directory)
    and to reuse the templates of already compiled files
    (e.g. headers, footers,...).  *)
class template : ?filename:string -> string ->
object
  method set : string -> string -> unit
    (** Set a variable in the template. *)

  method table : string -> table_row list -> unit
    (** Set a table in the template. *)

  method conditional : string -> bool -> unit
    (** Set a conditional in the template. *)

  method callback : string -> (string list -> string) -> unit
    (** Set a callback in the template. *)

  method to_string : string
    (** Return the template as a string. *)

  method to_channel : out_channel -> unit
    (** Write the template to a channel. *)

  method output : Netcgi.cgi -> unit
    (** [output cgi] outputs the template to the CGI session [cgi]. *)

  method source : string
    (** Return the original source code for the template. *)
end


val template : string -> template
  (** Compile the template from a named file.  Not thread safe. *)

val template_from_string : ?filename:string -> string -> template
  (** Compile the template from a literal string.  Not thread safe. *)

val template_from_channel : ?filename:string -> in_channel -> template
  (** Compile the template from a channel.  Not thread safe. *)
