This is a small subset of the netstring library extracted from
OCamlnet, written by Gerd Stolpmann
(http://projects.camlcity.org/projects/ocamlnet.html).

netstring-light currently provides the following modules:
* `Nldate` (`Netdate`) without localization
* `Nlencoding.Base64` (`Netencoding.Base64`)
* `Nlencoding.Url` (`Netencoding.Url`)
* other submodules inherited from Netencoding but not `Nlencoding.Html`

Installation:

```
$ make
$ make install
```
