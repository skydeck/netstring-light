(* File: netcgi_template.ml

   Copyright (C) 2006

     Christophe Troestler
     email: Christophe.Troestler@umh.ac.be
     WWW: http://math.umh.ac.be/an/software/

   This library is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License version 2.1 or
   later as published by the Free Software Foundation, with the special
   exception on linking described in the file LICENSE.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
   LICENSE for more details. *)


(* Define a template engine by the services it offers (and how those
   are represented programmatically in OCaml) and provide several
   concrete languages (all will be compiled to OCaml modules).  That
   way, the user can choose the style he likes the most -- or the more
   approriate one (simple template language for mail, Tapestry style
   for HTML,...). *)
