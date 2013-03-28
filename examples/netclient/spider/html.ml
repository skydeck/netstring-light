(* Types for the HTML scanner: *)

type token =
    Lcomment
  | Rcomment
  | Mcomment
  | Ldoctype
  | Rdoctype
  | Mdoctype
  | Lelement of string
  | Lelementend of string
  | Relement
  | Cdata of string 
  | Space of int
  | Name of string
  | Is
  | Literal of string
  | Other
  | Eof
;;


(* Types for the HTML parser: *)

type document =
    Element of (string * (string*string)list * document list)
  | Data of string
;;

