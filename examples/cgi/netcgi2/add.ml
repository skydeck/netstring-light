(* $Id: add.ml,v 1.3 2005/10/13 17:54:49 chris_77 Exp $ *)

(** This example demonstrates a very simple CGI page that refers to
    itself using the GET method.  *)

open Netcgi
open Printf

let text = Netencoding.Html.encode_from_latin1
  (* This function encodes "<", ">", "&", double quotes, and Latin 1
     characters as character entities.  E.g. text "<" = "&lt;", and
     text "ä" = "&auml;" *)

(* Wrap the page output -- the return of [html out] function -- with a
   consistent style and a [title]. *)
let html_page (cgi:cgi) title html =
  let out = cgi#out_channel#output_string in
  out "<!DOCTYPE html PUBLIC \"-//W3C//DTD HTML 4.0//EN\" \
	\"http://www.w3.org/TR/REC-html40/strict.dtd\">\n";
  out ("<html>\n<head>
<title>" ^ text title ^ "</title>
<style type=\"text/css\">
  body { background: #cce3f9; color: black; }
  a:link {color : #003893; text-decoration : none;}
  a:visited {color: #777777; text-decoration : none;}
  a:hover {text-decoration: none; color: #0000ff; background-color: #fff78e}
  a:active {color : Red; text-decoration : underline;}
</style>
</head>
<body>
<h1>" ^ text title ^ "</H1>\n");
  html out;
  out "</body>\n</html>\n"


(* Display the query form. *)
let query_page cgi =
  html_page cgi "Add Two Numbers"
    (fun out ->
       out "<p>This CGI page can perform additions. \
	Please enter two integers, and press the button!\n";
       (* Note that cgi#url() returns the URL of this script
	  (without ?  clause).  We pass this string through the text
	  function to avoid problems with some characters.  *)
       out(sprintf "<p><form method=\"GET\" action=\"%s\">\n"
              (text(cgi#url())));
       out "<input type=text name=\"x\"> + <input type=text name=\"y\"> = ";
       out "<input type=submit value=\"Go!\">\n";
       (* The hidden field only indicates that now the result page
	  should be consulted.  *)
       out "<input type=hidden name=\"page\" value=\"result\">\n";
       out "</form>\n";
    )


(* Compute the result, and display it *)
let result_page cgi =
  html_page cgi "Sum"
    (fun out ->
       let x = cgi#argument_value "x"
       and y = cgi#argument_value "y" in
       let sum = (int_of_string x) + (int_of_string y) in
       out (sprintf "<p>The result is:\n %s + %s = %d\n" x y sum);
       let url =
	 (* Here, the URL contains the CGI argument "page", but no
	    other arguments. *)
	 cgi#url
	   ~with_query_string:(`This [Argument.simple "page" "query"])
	   () in
       out ("<p><a href=\"" ^ text url ^ "\">Add further numbers</a>\n")
    )


let main (cgi:cgi) =
  (* Set the header. Here, the header specifies that the page must not
     be cached. This is important for dynamic pages called by the GET
     method, otherwise the browser might display an old version of
     the page.

     Furthermore, we set the content type and the character set.  Note
     that the header is not sent immediately to the browser because we
     have enabled HTML buffering.  *)
  cgi#set_header
    ~cache:`No_cache
    ~content_type:"text/html; charset=\"iso-8859-1\""
    ();

  (* Dispatcher: check which page is to be displayed.  This is
     contained in the CGI argument "page".  *)
  begin match cgi#argument_value "page" with
  | ""
  | "query" ->
      (* When the argument is the empty string, or the argument is
	 missing, it is considered the same like the page "query".  *)
      query_page cgi
  | "result" ->
      result_page cgi
  | p ->
      failwith(sprintf "page %S does not exist" p)
  end;
  cgi#out_channel#commit_work();
  cgi#finalize()


(* Need to run the [main] function through a connector.  See the files
   add_cgi.ml, add_mod.ml,... for that (separate to reuse this
   code).  *)
