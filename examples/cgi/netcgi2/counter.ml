(* $Id: counter.ml 552 2001-10-19 05:32:42Z pdoane $  *)

(***********************************************************************
 * This example demonstrates a very simple CGI page that contains
 * a counter that is incremented by the submit button.
 *
 * See add.ml for a slightly more complex example with more detailed
 * information.
 ***********************************************************************)

open Netcgi
open Printf

let text = Netencoding.Html.encode_from_latin1
  (* This function encodes "<", ">", "&", double quotes, and Latin 1
     characters as character entities.  E.g. text "<" = "&lt;", and
     text "ä" = "&auml;" *)

let main (cgi:cgi) =
  cgi#set_header
    ~cache:`No_cache
    ~content_type:"text/html; charset=\"iso-8859-1\""
    ();
  let n = int_of_string(cgi#argument_value ~default:"0" "Count") in
  let out = cgi#out_channel#output_string in
  out "<!DOCTYPE html PUBLIC \"-//W3C//DTD HTML 4.0//EN\" \
	\"http://www.w3.org/TR/REC-html40/strict.dtd\">\n";
  out "<html>\n";
  out "  <head>\n";
  out "    <title>counter</title>\n";
  out "  </head>\n";
  out "  <body>\n";
  out "    <h1>Counter</h1>\n";
  out (sprintf "    <form action=\"%s\" method=\"GET\" \
	enctype=\"application/x-www-form-urlencoded\" />\n" (text(cgi#url())));
  out (sprintf "    <input type=\"submit\" name=\"Count\" value=\"%i\" />\n"
          (n + 1));
  out "    </form>\n";
  out "  </body>\n";
  out "</html>\n"

