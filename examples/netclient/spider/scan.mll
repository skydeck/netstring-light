{
  open Html
}

(* Simplified rules: Only Latin-1 is recognized as character set *)

let letter = ['A'-'Z' 'a'-'z' '\192'-'\214' '\216'-'\246' '\248'-'\255']
let extender = '\183'
let digit = ['0'-'9']
let hexdigit = ['0'-'9' 'A'-'F' 'a'-'f']
let namechar = letter | digit | '.' | ':' | '-' | '_' | extender
let name = ( letter | '_' | ':' ) namechar*
let nmtoken = namechar+
let ws = [ ' ' '\t' '\r' '\n' ]
let string_literal1 = '"' [^ '"' '>' '<' '\n']* '"'
let string_literal2 = "'" [^ '\'' '>' '<' '\n']* "'"


(* This following rules reflect HTML as it is used, not the SGML
 * rules.
 *)

rule scan_document = parse
  | "<!--"
      { Lcomment }
  | "<!"
      { Ldoctype }
  | "<" name
      { let s = Lexing.lexeme lexbuf in
	Lelement (String.sub s 1 (String.length s - 1))
      }
  | "</" name
      { let s = Lexing.lexeme lexbuf in
	Lelementend (String.sub s 2 (String.length s - 2))
      }
  | "<"                (* misplaced "<" *)
      { Cdata "<" }
  | eof
      { Eof }
  | [^ '<' ]+
      { Cdata (Lexing.lexeme lexbuf)}

and scan_special = parse
  | "</" name 
      { let s = Lexing.lexeme lexbuf in
	Lelementend (String.sub s 2 (String.length s - 2))
      }
  | "<"
      { Cdata "<" }
  | eof
      { Eof }
  | [^ '<' ]+
      { Cdata (Lexing.lexeme lexbuf)}


and scan_comment = parse
  | "-->"
      { Rcomment }
  | "-"
      { Mcomment }
  | eof
      { Eof }
  | [^ '-']+
      { Mcomment }

and scan_doctype = parse
  | ">"                   (* Occurence in strings, and [ ] brackets ignored *)
      { Rdoctype }
  | eof
      { Eof }
  | [^ '>' ] +
      { Mdoctype }

and scan_element = parse
  | ">"
      { Relement }
  | ws+
      { Space (String.length (Lexing.lexeme lexbuf)) }
  | name
      { Name (Lexing.lexeme lexbuf) }
  | "="
      { Is }
  | string_literal1
      { let s = Lexing.lexeme lexbuf in
	Literal (String.sub s 1 (String.length s - 2)) 
      }
  | string_literal2
      { let s = Lexing.lexeme lexbuf in
	Literal (String.sub s 1 (String.length s - 2)) 
      }
  | eof
      { Eof }
  | _
      { Other }
