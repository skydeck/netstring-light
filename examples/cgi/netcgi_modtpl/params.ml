(* Use a template to display the parameters passed to the script.  In
   particular, it illustrates how to use the template "table"
   directive and the escaping of template arguments.  *)

open Netcgi
module T = Netcgi_modtpl

let template = T.template_from_string "\
<html>
 <head><title>::title_html::</title></head>
  <body bgcolor=\"#ffffff\">
  <h1>::title_html::</h1>
  <h2>Parameters passed into the script</h2>
  <table border=\"1\"><tr><th>Name</th><th>Value</th></tr>
    ::table(params)::
    <tr><td>::name_html::</td><td>::value_html::</td></tr>
    ::end::
  </table>
  <h2>Form</h2>
  <p>
    This is a simple form which POSTs data to the script:
  </p>
  <form method=\"post\" action=\"::self::\">
    Name: <input name=\"name\" value=\"\">
    <input type=\"submit\" name=\"submit\" value=\"Send\">
  </form>
 </body>
</html>"


let display_params (cgi:cgi) =
  let table = List.map (fun a ->
    [ "name",  T.VarString a#name;
      "value", T.VarString a#value ]
  ) cgi#arguments in
  template#set "title" "Params demonstration CGI script";
  template#table "params" table;
  (* Note the unescaped "self" parameter that allows to locate and
     name the script the way you like. *)
  template#set "self" (cgi#url());
  template#output cgi


let () =
  let buffered _ ch = new Netchannels.buffered_trans_channel ch in
  (* To use a different connector, just change the next line -- the rest of
     the code stays the same. *)
  Netcgi_cgi.run ~output_type:(`Transactional buffered) display_params
