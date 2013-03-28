(* Simple script to demonstrate the use of templates. *)

open Netcgi

(* Normally, you should put the template into a file and load it with
   [Netcgi_modtpl.template filename].  We have inlined it here for
   simplicity.  *)
let template = Netcgi_modtpl.template_from_string "\
<html>
  <head><title>::title_html::</title></head>
  <body bgcolor=\"#ffffff\">
    <h1>::title_html::</h1>
    <p>
      <b>If you see this message, then everything looks like it's installed
      and working fine!</b>
    </p>
  </body>
</html>"

let main (cgi:cgi) =
  template#set "title" "Hello, world.";
  cgi#set_header
    ~cache:`No_cache
    ~content_type:"text/html; charset=\"iso-8859-1\""
    ();
  template#output cgi


let () =
  let buffered _ ch = new Netchannels.buffered_trans_channel ch in
  (* To use a different connector, just change the next line -- the rest of
     the code stays the same. *)
  Netcgi_cgi.run ~output_type:(`Transactional buffered) main
