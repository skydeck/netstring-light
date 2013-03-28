(* $Id: netglob_lex.mll 1514 2010-12-17 18:24:59Z gerd $ *)

{
  exception Bracket_Unsupported
  exception Lexing_Error

  type bracket_token =
      Bracket_char of char
    | Bracket_range of (char * char)
    | Bracket_code of int  (* see Netglob.reparse_bracket_expr *)
    | Bracket_end

  type brace_token =
      Brace_literal of string
    | Brace_comma
    | Brace_braces of brace_token list  (* inner braces *)
    | Brace_end

  type glob_features =
      { enable_star : bool;
	enable_qmark : bool;
	enable_brackets : bool;
	enable_braces : bool;
	enable_tilde : bool;
	enable_escape : bool;
	mutable escaped : bool;  (* after a backslash *)
      }

  type glob_token =
      Glob_literal of string
    | Glob_star
    | Glob_qmark
    | Glob_brackets of (bool * bracket_token list)
    | Glob_braces of brace_token list
    | Glob_tilde of string * bool (* whether there is a slash *)
    | Glob_end

  type exploded_char =
      C of char   (* An unescaped character *)
    | E of char   (* An escaped character *)
    | Delim of char  (* delimiter *)



  let rec collect_until end_token parse_fun lexbuf =
    let tok = parse_fun lexbuf in
    if tok = end_token then
      []
    else
      tok :: (collect_until end_token parse_fun lexbuf)


  let string_of_exploded l =
    String.concat "" 
      (List.map
	 (function 
	    | C c -> String.make 1 c
	    | E c -> String.make 1 c
	    | Delim _ -> ""
	 )
	 l
      )

  let have_delim l =
    List.exists (function Delim _ -> true | _ -> false) l

}

(* bracket_rest: Scans a bracket expression beginning at the second 
 * character (where ']' is always the terminating character)
 *)

rule bracket_rest = parse
    "[:" [^ ':' ] ":]" { raise Bracket_Unsupported }
  | "[." [^ '.' ] ".]" { raise Bracket_Unsupported }
  | "[=" [^ '=' ] "=]" { raise Bracket_Unsupported }
  | "]"                { Bracket_end }
  | [ ^ ']' ] "-" [^ ']' ]     
                       { let c0 = Lexing.lexeme_char lexbuf 0 in
			 let c1 = Lexing.lexeme_char lexbuf 2 in
			 if c0 > '\127' || c1 > '\127' then raise Lexing_Error;
			 if c0 > c1 then raise Lexing_Error;
			 Bracket_range(c0,c1)
		       }
  | eof                { raise Lexing_Error }
  | [ ^ ']' ]          { Bracket_char (Lexing.lexeme_char lexbuf 0) }

(* bracket_first: Scans the first token of a bracket expression
 * (after "[", "[^", or "[!").
 * Here, ']' is not recognized as terminating character.
 *)

and bracket_first = parse
    "[:" [^ ':' ] ":]" { raise Bracket_Unsupported }
  | "[." [^ '.' ] ".]" { raise Bracket_Unsupported }
  | "[=" [^ '=' ] "=]" { raise Bracket_Unsupported }
  | _ "-" [^ ']' ]     { let c0 = Lexing.lexeme_char lexbuf 0 in
			 let c1 = Lexing.lexeme_char lexbuf 2 in
			 if c0 > '\127' || c1 > '\127' then raise Lexing_Error;
			 if c0 > c1 then raise Lexing_Error;
			 Bracket_range(c0,c1)
		       }
  | eof                { raise Lexing_Error }
  | _                  { Bracket_char (Lexing.lexeme_char lexbuf 0) }


(* brace: Collects material within brace expressions (case: backslash
 * is escape character
 *)

and brace = parse
    "}"                { Brace_end }
  | ","                { Brace_comma }
  | "{"                { let l = collect_until Brace_end brace lexbuf in
			 Brace_braces l }
  | '\\' _             { Brace_literal (Lexing.lexeme lexbuf) }
  | [^ '}' ',' '\\' '{' ]  { Brace_literal (Lexing.lexeme lexbuf) }
  | eof                { raise Lexing_Error }
  | _                  { raise Lexing_Error }

(* brace_noescape: Used for the case that backslash is not an escape
 * character
 *)

and brace_noescape = parse
    "}"                { Brace_end }
  | ","                { Brace_comma }
  | "{"                { let l = collect_until Brace_end brace_noescape lexbuf in
			 Brace_braces l }
  | [^ '}' ',' '{']    { Brace_literal (Lexing.lexeme lexbuf) }
  | eof                { raise Lexing_Error }
  | _                  { raise Lexing_Error }

and glob_expr feat = parse
    "*"                { if feat.enable_star && not feat.escaped then 
			   Glob_star
			 else (
			   feat.escaped <- false;
			   Glob_literal "*"
			 )
		       }
  | "?"                { if feat.enable_qmark && not feat.escaped then 
			   Glob_qmark
			 else (
			   feat.escaped <- false;
			   Glob_literal "?"
			 )
		       }
  | "[" [ '!' '^' ]?   { if feat.enable_brackets && not feat.escaped then (
			   let negated = 
			     String.length(Lexing.lexeme lexbuf) > 1 in
			   let t0 = bracket_first lexbuf in
			   let l = collect_until 
				     Bracket_end bracket_rest lexbuf in
			   Glob_brackets (negated, t0 :: l)
			 )
			 else (
			   feat.escaped <- false;
			   Glob_literal (Lexing.lexeme lexbuf)
			 )
		       }
  | "{"                { if feat.enable_braces && not feat.escaped then (
			   let p =
			     if feat.enable_escape then
			       brace
			     else
			       brace_noescape in
			   let l = collect_until Brace_end p lexbuf in
			   Glob_braces l
			 )
			 else (
			   feat.escaped <- false;
			   Glob_literal "{"
			 )
		       }
  | "~"                { if (feat.enable_tilde && not feat.escaped && 
                             Lexing.lexeme_start lexbuf = 0) then (
			   let p =
			     if feat.enable_escape then
			       generic_lex_until '/'
			     else
			       generic_lex_noescape_until '/' in
			   let l = p lexbuf in
			   let s = string_of_exploded l in
			   let slash = have_delim l in
			   Glob_tilde(s,slash)
			 ) else (
			   feat.escaped <- false;
			   Glob_literal "~"
			 )
                       }
  | "\\"               { if feat.enable_escape && not feat.escaped then (
			   feat.escaped <- true;
			   Glob_literal ""
			 )
			 else (
			   feat.escaped <- false;
			   Glob_literal "\\"
			 )
		       }
  | [ ^ '*' '?' '[' '{' '\\' '~' ]+ 
                       { feat.escaped <- false;
			 Glob_literal (Lexing.lexeme lexbuf)
		       }
  | eof                { if feat.escaped then raise Lexing_Error;
			 Glob_end
		       }

and generic_lex_until c = parse
    '\\' _             { let char = E (Lexing.lexeme_char lexbuf 1) in
                         char :: generic_lex_until c lexbuf }
  | _                  { let lc = Lexing.lexeme_char lexbuf 0 in
			 if c = lc then [ Delim c ] else (
                           let char = C lc in
                           char :: generic_lex_until c lexbuf
			 ) }
  | eof                { [] }

and generic_lex_noescape_until c = parse
  | _                  { let lc = Lexing.lexeme_char lexbuf 0 in
			 if c = lc then [ Delim c ] else (
                           let char = C lc in
                           char :: generic_lex_noescape_until c lexbuf
			 ) }
  | eof                { [] }

