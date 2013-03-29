module Base64 = struct
  let b64_pattern plus slash =
    [| 'A'; 'B'; 'C'; 'D'; 'E'; 'F'; 'G'; 'H'; 'I'; 'J'; 'K'; 'L'; 'M';
       'N'; 'O'; 'P'; 'Q'; 'R'; 'S'; 'T'; 'U'; 'V'; 'W'; 'X'; 'Y'; 'Z';
       'a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h'; 'i'; 'j'; 'k'; 'l'; 'm';
       'n'; 'o'; 'p'; 'q'; 'r'; 's'; 't'; 'u'; 'v'; 'w'; 'x'; 'y'; 'z';
       '0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9'; plus; slash |];;


  let rfc_pattern = b64_pattern '+' '/';;
  let url_pattern = b64_pattern '-' '/';;

  let encode_with_options b64 equal s pos len
                          linelen first_linelen crlf =
  (* encode using "base64".
   * 'b64': The encoding table, created by b64_pattern.
   * 'equal': The character that should be used instead of '=' in the original
   *          encoding scheme. Pass '=' to get the original encoding scheme.
   * s, pos, len, linelen: See the interface description of encode_substring.
   * first_linelen: The length of the first line.
   *
   * Returns: (s,last_linelen) where [s] is the encoded string, and
   *   [last_linelen] is the length of the last line
   *)
    assert (Array.length b64 = 64);
    if len < 0 || pos < 0 || pos > String.length s || linelen < 0 then
      invalid_arg "Netencoding.Base64.encode";
    if pos + len > String.length s then
      invalid_arg "Netencoding.Base64.encode";

    let linelen = (linelen asr 2) lsl 2 in
    let first_linelen = (first_linelen asr 2) lsl 2 in

    let l_t = if len = 0 then 0 else ((len - 1) / 3 + 1) * 4 in
    (* l_t: length of the result without additional line endings *)

    let factor = if crlf then 2 else 1 in
    let l_t' =
      if linelen < 4 then
	l_t
      else
	if l_t <= first_linelen then
	  ( if l_t = 0 then 0 else l_t + factor )
	else
	  let n_lines = ((l_t - first_linelen - 1) / linelen) + 2 in
	  l_t + n_lines * factor
    in
    (* l_t': length of the result with CRLF or LF characters *)

    let t = String.make l_t' equal in
    let j = ref 0 in
    let q = ref (linelen - first_linelen) in
    for k = 0 to len / 3 - 1 do
      let p = pos + 3*k in
      (* p >= pos >= 0: this is evident
       * p+2 < pos+len <= String.length s:
       *   Because k <= len/3-1
       *         3*k <= 3*(len/3-1) = len - 3
       *   pos+3*k+2 <= pos + len - 3 + 2 = pos + len - 1 < pos + len
       * So it is proved that the following unsafe string accesses always
       * work.
       *)
      let bits = (Char.code (String.unsafe_get s (p))   lsl 16) lor
		 (Char.code (String.unsafe_get s (p+1)) lsl  8) lor
		 (Char.code (String.unsafe_get s (p+2))) in
      (* Obviously, 'bits' is a 24 bit entity (i.e. bits < 2**24) *)
      assert(!j + 3 < l_t');
      String.unsafe_set t !j     (Array.unsafe_get b64 ( bits lsr 18));
      String.unsafe_set t (!j+1) (Array.unsafe_get b64 ((bits lsr 12) land 63));
      String.unsafe_set t (!j+2) (Array.unsafe_get b64 ((bits lsr  6) land 63));
      String.unsafe_set t (!j+3) (Array.unsafe_get b64 ( bits         land 63));
      j := !j + 4;
      if linelen > 3 then begin
	q := !q + 4;
	if !q + 4 > linelen then begin
	  (* The next 4 characters won't fit on the current line. So insert
	   * a line ending.
	   *)
	  if crlf then begin
	    t.[ !j ] <- '\013';
	    t.[ !j+1 ] <- '\010';
	    j := !j + 2;
	  end
	  else begin
	    t.[ !j ] <- '\010';
	    incr j
	  end;
	  q := 0;
	end;
      end;
    done;
    (* padding if needed: *)
    let m = len mod 3 in
    begin
      match m with
	  0 -> ()
	| 1 ->
            let bits = Char.code (s.[pos + len - 1]) in
	    t.[ !j     ] <- b64.( bits lsr 2);
	    t.[ !j + 1 ] <- b64.( (bits land 0x03) lsl 4);
	    j := !j + 4;
	    q := !q + 4;
	| 2 ->
	    let bits = (Char.code (s.[pos + len - 2]) lsl 8) lor
                       (Char.code (s.[pos + len - 1])) in
	    t.[ !j     ] <- b64.( bits lsr 10);
	    t.[ !j + 1 ] <- b64.((bits lsr  4) land 0x3f);
	    t.[ !j + 2 ] <- b64.((bits lsl  2) land 0x3f);
	    j := !j + 4;
	    q := !q + 4;
	| _ -> assert false
    end;

    (* If required, add another line end: *)

    if linelen > 3 && !q > 0 && len > 0 then begin
      if crlf then begin
	t.[ !j ] <- '\013';
	t.[ !j+1 ] <- '\010';
	j := !j + 2;
      end
      else begin
	t.[ !j ] <- '\010';
	incr j;
      end;
    end;

    (t, !q) ;;



  let encode ?(pos=0) ?len ?(linelength=0) ?(crlf=false) s =
    let l = match len with None -> String.length s - pos | Some x -> x in
    let s,_ =
      encode_with_options rfc_pattern '=' s pos l linelength linelength crlf in
    s
  ;;


  let url_encode ?(pos=0) ?len ?(linelength=0) ?(crlf=false) s =
    let l = match len with None -> String.length s - pos | Some x -> x in
    let s,_ =
      encode_with_options url_pattern '.' s pos l linelength linelength crlf in
    s


  let decode_prefix t pos len p_url p_spaces p_full p_null =
    (* Decodes the prefix of a Base64-encoded string. Returns a triple
     * (s,n,eof) where s is the decoded prefix, and n is the number of
     * processed characters from t (i.e. the characters pos to pos+n-1 have
     * been processed), and where eof is the boolean flag whether the
     * padding '=' characters at the end of the string have been seen.
     *
     * p_url: accepts strings produced by url_endode
     * p_spaces: accepts spaces in [t] (at the price of reduced speed)
     * p_full: [t] must be a closed encoded string (i.e. no prefix)
     * p_null: [t] must be an encoded null string
     *)

    if len < 0 || pos < 0 || pos > String.length t then
      invalid_arg "Netencoding.Base64.decode";
    if pos + len > String.length t then
      invalid_arg "Netencoding.Base64.decode";

    (* Compute the number of effective characters l_t in 't';
     * pad_chars: number of '=' characters at the end of the string.
     *)
    let l_t, pad_chars =
      if p_spaces then begin
	(* Count all non-whitespace characters: *)
	let c = ref 0 in
	let p = ref 0 in
	for i = pos to pos + len - 1 do
	  match String.unsafe_get t i with
	      (' '|'\t'|'\r'|'\n'|'>') -> ()
	    | ('='|'.') as ch ->
		if ch = '.' && not p_url then
		  invalid_arg "Netencoding.Base64.decode";
		incr c;
		incr p;
		if !p > 2 then
		  invalid_arg "Netencoding.Base64.decode";
		for j = i+1 to pos + len - 1 do
		  match String.unsafe_get t j with
		      (' '|'\t'|'\r'|'\n'|'.'|'=') -> ()
		    | _ ->
			(* Only another '=' or spaces allowed *)
			invalid_arg "Netencoding.Base64.decode";
		done
	    | _ -> incr c
	done;
	!c, !p
      end
      else
	len,
	( if len > 0 then (
	    if String.sub t (len - 2) 2 = "==" ||
	       (p_url && String.sub t (len - 2) 2 = "..") then 2
	    else
	      if String.sub t (len - 1) 1 = "=" ||
		 (p_url && String.sub t (len - 1) 1 = ".") then 1
	      else
		0
	  )
	  else 0
	)
    in

    if p_null && l_t <> 0 then invalid_arg "Netencoding.Base64.decode";

    (* Compute the number of characters [l_t] that can be processed now
     * (i.e. the effective prefix)
     *)
    let l_t, pad_chars =
      let m = l_t mod 4 in
      if m = 0 then (
	(l_t, pad_chars)         (* a multiple of 4 *)
      ) else (
	if p_full then invalid_arg "Netencoding.Base64.decode";
	(l_t - m, 0)             (* rounded to a multiple of 4 *)
      )
    in

    let l_s = (l_t / 4) * 3 - pad_chars in
    let s = String.create l_s in

    let decode_char c =
      match c with
	  'A' .. 'Z'  -> Char.code(c) - 65     (* 65 = Char.code 'A' *)
	| 'a' .. 'z'  -> Char.code(c) - 71     (* 71 = Char.code 'a' - 26 *)
	| '0' .. '9'  -> Char.code(c) + 4      (* -4 = Char.code '0' - 52 *)
	| '+'         -> 62
	| '-'         -> if not p_url then
	                   invalid_arg "Netencoding.Base64.decode";
	                 62
	| '/'         -> 63
	| _           -> invalid_arg "Netencoding.Base64.decode";
    in

    (* Decode all but the last quartet: *)

    let cursor = ref pos in
    let rec next_char() =
      match t.[ !cursor ] with
	  (' '|'\t'|'\r'|'\n'|'>') ->
	    if p_spaces then (incr cursor; next_char())
	    else invalid_arg "Netencoding.Base64.decode"
	| c ->
	    incr cursor; c
    in

    if p_spaces then begin
      for k = 0 to l_t / 4 - 2 do
	let q = 3*k in
	let c0 = next_char() in
	let c1 = next_char() in
	let c2 = next_char() in
	let c3 = next_char() in
	let n0 = decode_char c0 in
	let n1 = decode_char c1 in
	let n2 = decode_char c2 in
	let n3 = decode_char c3 in
	let x0 = (n0 lsl 2) lor (n1 lsr 4) in
	let x1 = ((n1 lsl 4) land 0xf0) lor (n2 lsr 2) in
	let x2 = ((n2 lsl 6) land 0xc0) lor n3 in
	String.unsafe_set s q     (Char.chr x0);
	String.unsafe_set s (q+1) (Char.chr x1);
	String.unsafe_set s (q+2) (Char.chr x2);
      done;
    end
    else begin
      (* Much faster: *)
      for k = 0 to l_t / 4 - 2 do
	let p = pos + 4*k in
	let q = 3*k in
	let c0 = String.unsafe_get t p in
	let c1 = String.unsafe_get t (p + 1) in
	let c2 = String.unsafe_get t (p + 2) in
	let c3 = String.unsafe_get t (p + 3) in
	let n0 = decode_char c0 in
	let n1 = decode_char c1 in
	let n2 = decode_char c2 in
	let n3 = decode_char c3 in
	let x0 = (n0 lsl 2) lor (n1 lsr 4) in
	let x1 = ((n1 lsl 4) land 0xf0) lor (n2 lsr 2) in
	let x2 = ((n2 lsl 6) land 0xc0) lor n3 in
	String.unsafe_set s q     (Char.chr x0);
	String.unsafe_set s (q+1) (Char.chr x1);
	String.unsafe_set s (q+2) (Char.chr x2);
      done;
      cursor := pos + l_t - 4;
    end;

    (* Decode the last quartet: *)

    if l_t > 0 then begin
      let q = 3*(l_t / 4 - 1) in
      let c0 = next_char() in
      let c1 = next_char() in
      let c2 = next_char() in
      let c3 = next_char() in

      if (c2 = '=' & c3 = '=') or (p_url & c2 = '.' & c3 = '.') then begin
	let n0 = decode_char c0 in
	let n1 = decode_char c1 in
	let x0 = (n0 lsl 2) lor (n1 lsr 4) in
	s.[ q ]   <- Char.chr x0;
      end
      else
	if (c3 = '=') or (p_url & c3 = '.') then begin
	  let n0 = decode_char c0 in
	  let n1 = decode_char c1 in
	  let n2 = decode_char c2 in
	  let x0 = (n0 lsl 2) lor (n1 lsr 4) in
	  let x1 = ((n1 lsl 4) land 0xf0) lor (n2 lsr 2) in
	  s.[ q ]   <- Char.chr x0;
	  s.[ q+1 ] <- Char.chr x1;
	end
	else begin
	  let n0 = decode_char c0 in
	  let n1 = decode_char c1 in
	  let n2 = decode_char c2 in
	  let n3 = decode_char c3 in
	  let x0 = (n0 lsl 2) lor (n1 lsr 4) in
	  let x1 = ((n1 lsl 4) land 0xf0) lor (n2 lsr 2) in
	  let x2 = ((n2 lsl 6) land 0xc0) lor n3 in
	  s.[ q ]   <- Char.chr x0;
	  s.[ q+1 ] <- Char.chr x1;
	  s.[ q+2 ] <- Char.chr x2;
	end

    end
    else cursor := 0;

    (s, !cursor - pos, pad_chars > 0)
  ;;


  let decode ?(pos=0) ?len ?(url_variant=true) ?(accept_spaces=false) s =
    let l = match len with None -> String.length s - pos | Some x -> x in
    let (s,_,_) = decode_prefix s pos l url_variant accept_spaces true false in
    s
end



module QuotedPrintable = struct

  let encode_substring ?(crlf = true) ?(eot = false) ?(line_length = ref 0) s ~pos ~len =
    (* line_length:
     * - on input, the length of the line where the encoding starts
     * - on output, the length of the last written line
     * eot:
     * - false: it is known that the chunk is not at the end of text
     * - true: the chunk may be at the end of the text
     * eot has only an effect on trailing spaces
     *)

    if len < 0 or pos < 0 or pos > String.length s then
      invalid_arg "Netencoding.QuotedPrintable.encode";
    if pos + len > String.length s then
      invalid_arg "Netencoding.QuotedPrintable.encode";

    let eol_len = if crlf then 2 else 1 in    (* length of eol *)

    (* Note: The [count] algorithm must strictly correspond to the
     * "for" loop below.
     *)

    let rec count l n i =
      (* l: output line length
       * n: output byte count
       * i: input byte count
       *)
      if i < len then
	match String.unsafe_get s (pos+i) with
	    '\r' ->              (* CR is deleted *)
	      count l n (i+1)
	  | '\n' ->              (* LF may be expanded to CR/LF *)
	      count 0 (n+eol_len) (i+1)
	  | ('\000'..'\031'|'\127'..'\255'|
	     '!'|'"'|'#'|'$'|'@'|'['|']'|'^'|'\''|'{'|'|'|'}'|'~'|'=') ->
	      if l <= 69 then
		count (l+3) (n+3) (i+1)
	      else
		(* Add soft line break after the encoded char: *)
		count 0 (n+4+eol_len) (i+1)
	  | 'F' when l=0 ->
	      (* Protect 'F' at the beginning of lines *)
	      count (l+3) (n+3) (i+1)
	  | ' ' when (i=len-1 && eot) ||   (* at end of text *)
	             l>69 ||               (* line too long *)
                     (i<len-1 && (s.[pos+i+1]='\r' || s.[pos+i+1]='\n'))
		        (* end of line *)
		     ->
	      (* Protect spaces only if they occur at the end of a line,
	       * or just before soft line breaks
	       *)
	      if l <= 69 then
		count (l+3) (n+3) (i+1)
	      else
		(* Add soft line after the encoded space: *)
		count 0 (n+4+eol_len) (i+1)
	  | _ ->
	      if l>71 then
		(* Add soft line break after the char: *)
		count 0 (n+2+eol_len) (i+1)
	      else
		count (l+1) (n+1) (i+1)
      else
	n
    in

    let t_len = count !line_length 0 0 in
    let t = String.create t_len in

    let hexdigit =
      [| '0'; '1'; '2'; '3'; '4'; '5'; '6'; '7';
	 '8'; '9'; 'A'; 'B'; 'C'; 'D'; 'E'; 'F'; |] in

    let k = ref 0 in

    let add_quoted c =
      t.[ !k ]   <- '=';
      t.[ !k+1 ] <- hexdigit.( Char.code c lsr 4 );
      t.[ !k+2 ] <- hexdigit.( Char.code c land 15 )
    in

    let add_soft_break() =
      t.[ !k ]   <- '=';
      if crlf then (
	t.[ !k+1 ] <- '\r';
	t.[ !k+2 ] <- '\n';
      )
      else
	t.[ !k+1 ] <- '\n';
    in

    (* In the following, the soft break criterion is [!l > 72]. Why?
     * We need to be able to add at least an encoded char (3 bytes)
     * plus the "=" sign for the soft break. So we are on the safe side
     * when there are four bytes space on the line. Lines must not be
     * longer than 76 chars (w/o CRLF), so 76-4=72.
     *)

    let l = ref !line_length in
    for i = 0 to len - 1 do
      match String.unsafe_get s i with
	  '\r' ->   (* CR is deleted *)
	    ()
	| '\n' ->   (* LF is expanded to CR/LF *)
	    if crlf then (
	      t.[ !k ] <- '\r';
	      t.[ !k+1 ] <- '\n';
	      k := !k + 2;
	    ) else (
	      t.[ !k ] <- '\n';
	      k := !k + 1;
	    );
	    l := 0
	| ('\000'..'\031'|'\127'..'\255'|
	   '!'|'"'|'#'|'$'|'@'|'['|']'|'^'|'\''|'{'|'|'|'}'|'~'|'=') as c ->
	    add_quoted c;
	    k := !k + 3;
	    l := !l + 3;
	    if !l > 72 then (
	      (* Add soft line break: *)
	      add_soft_break();
	      k := !k + 1 + eol_len;
	      l := 0
	    )
	| 'F' when !l = 0 ->
	    (* Protect 'F' at the beginning of lines *)
	    add_quoted 'F';
	    k := !k + 3;
	    l := !l + 3;
	| ' ' when ((i=len-1 && eot) ||
	              !l > 69 ||
                     (i<len-1 && (s.[pos+i+1]='\r' || s.[pos+i+1]='\n'))) ->
	    add_quoted ' ';
	    k := !k + 3;
	    l := !l + 3;
	    if !l > 72 then (
	      add_soft_break();
	      k := !k + 1 + eol_len;
	      l := 0;
	    )
	| c ->
	    String.unsafe_set t !k c;
	    incr k;
	    incr l;
	    if !l > 72 then (
	      add_soft_break();
	      k := !k + 1 + eol_len;
	      l := 0;
	    )
    done;

    assert(!k == t_len);

    line_length := !l;

    t ;;


  let encode ?crlf ?(pos=0) ?len s =
    let l = match len with None -> String.length s - pos | Some x -> x in
    encode_substring ?crlf ~eot:true s ~pos ~len:l

  let decode_substring s ~pos ~len =

    if len < 0 or pos < 0 or pos > String.length s then
      invalid_arg "Netencoding.QuotedPrintable.decode";
    if pos + len > String.length s then
      invalid_arg "Netencoding.QuotedPrintable.decode";

    let decode_hex c =
      match c with
	  '0'..'9' -> Char.code c - 48
	| 'A'..'F' -> Char.code c - 55
	| 'a'..'f' -> Char.code c - 87
	| _ ->
	   invalid_arg "Netencoding.QuotedPrintable.decode";
    in

    let rec count n i =
      if i < len then
	match String.unsafe_get s (pos+i) with
	    '=' ->
	      if i+1 = len then
		(* A '=' at EOF is ignored *)
		count n (i+1)
	      else
		if i+1 < len then
		  match s.[pos+i+1] with
		      '\r' ->
			(* Official soft break *)
			if i+2 < len & s.[pos+i+2] = '\n' then
			  count n (i+3)
			else
			  count n (i+2)
		    | '\n' ->
			(* Inofficial soft break *)
			count n (i+2)
		    | _ ->
			if i+2 >= len then
			  invalid_arg
			    "Netencoding.QuotedPrintable.decode";
			let _ = decode_hex s.[pos+i+1] in
			let _ = decode_hex s.[pos+i+2] in
			count (n+1) (i+3)
		else
		  invalid_arg "Netencoding.QuotedPrintable.decode"
	  | _ ->
	      count (n+1) (i+1)
      else
	n
    in

    let l = count 0 0 in
    let t = String.create l in
    let k = ref pos in
    let e = pos + len in
    let i = ref 0 in

    while !i < l do
      match String.unsafe_get s !k with
	  '=' ->
	    if !k+1 = e then
	      (* A '=' at EOF is ignored *)
	      ()
	    else
	      if !k+1 < e then
		match s.[!k+1] with
		    '\r' ->
		      (* Official soft break *)
		      if !k+2 < e & s.[!k+2] = '\n' then
			k := !k + 3
		      else
			k := !k + 2
		  | '\n' ->
		      (* Inofficial soft break *)
		      k := !k + 2
		  | _ ->
		      if !k+2 >= e then
			invalid_arg
			  "Netencoding.QuotedPrintable.decode_substring";
		      let x1 = decode_hex s.[!k+1] in
		      let x2 = decode_hex s.[!k+2] in
		      t.[ !i ] <- Char.chr ((x1 lsl 4) lor x2);
		      k := !k + 3;
		      incr i
	      else
		invalid_arg "Netencoding.QuotedPrintable.decode_substring"
	| c ->
	    String.unsafe_set t !i c;
	    incr k;
	    incr i
    done;

    t ;;


  let decode ?(pos=0) ?len s =
    let l = match len with None -> String.length s - pos | Some x -> x in
    decode_substring s pos l

end


module Q = struct

  let encode_substring s ~pos ~len =

    if len < 0 or pos < 0 or pos > String.length s then
      invalid_arg "Netencoding.Q.encode_substring";
    if pos + len > String.length s then
      invalid_arg "Netencoding.Q.encode_substring";

    let rec count n i =
      if i < len then
	match String.unsafe_get s (pos+i) with
	  | ('A'..'Z'|'a'..'z'|'0'..'9') ->
	      count (n+1) (i+1)
	  | _ ->
	      count (n+3) (i+1)
      else
	n
    in

    let l = count 0 0 in
    let t = String.create l in

    let hexdigit =
      [| '0'; '1'; '2'; '3'; '4'; '5'; '6'; '7';
	 '8'; '9'; 'A'; 'B'; 'C'; 'D'; 'E'; 'F'; |] in

    let k = ref 0 in

    let add_quoted c =
      t.[ !k ]   <- '=';
      t.[ !k+1 ] <- hexdigit.( Char.code c lsr 4 );
      t.[ !k+2 ] <- hexdigit.( Char.code c land 15 )
    in

    for i = 0 to len - 1 do
      match String.unsafe_get s i with
	| ('A'..'Z'|'a'..'z'|'0'..'9') as c ->
	    String.unsafe_set t !k c;
	    incr k
	| c ->
	    add_quoted c;
	    k := !k + 3
    done;

    t ;;


  let encode ?(pos=0) ?len s =
    let l = match len with None -> String.length s - pos | Some x -> x in
    encode_substring s pos l;;



  let decode_substring s ~pos ~len =

    if len < 0 or pos < 0 or pos > String.length s then
      invalid_arg "Netencoding.Q.decode_substring";
    if pos + len > String.length s then
      invalid_arg "Netencoding.Q.decode_substring";

    let decode_hex c =
      match c with
	  '0'..'9' -> Char.code c - 48
	| 'A'..'F' -> Char.code c - 55
	| 'a'..'f' -> Char.code c - 87
	| _ ->
	   invalid_arg "Netencoding.Q.decode_substring";
    in

    let rec count n i =
      if i < len then
	match String.unsafe_get s (pos+i) with
	    '=' ->
	      if i+2 >= len then
		invalid_arg "Netencoding.Q.decode_substring";
	      let _ = decode_hex s.[pos+i+1] in
	      let _ = decode_hex s.[pos+i+2] in
	      count (n+1) (i+3)
	  | _ ->  (* including '_' *)
	      count (n+1) (i+1)
      else
	n
    in

    let l = count 0 0 in
    let t = String.create l in
    let k = ref pos in
    let e = pos + len in
    let i = ref 0 in

    while !i < l do
      match String.unsafe_get s !k with
	  '=' ->
	    if !k+2 >= e then
	      invalid_arg "Netencoding.Q.decode_substring";
	    let x1 = decode_hex s.[!k+1] in
	    let x2 = decode_hex s.[!k+2] in
	    t.[ !i ] <- Char.chr ((x1 lsl 4) lor x2);
	    k := !k + 3;
	    incr i
	| '_' ->
	    String.unsafe_set t !i ' ';
	    incr k;
	    incr i
	| c ->
	    String.unsafe_set t !i c;
	    incr k;
	    incr i
    done;

    t ;;


  let decode ?(pos=0) ?len s =
    let l = match len with None -> String.length s - pos | Some x -> x in
    decode_substring s pos l ;;
end


module Url = struct

  (* The Ocamlnet3 version is from Christophe Troestler.
     See netcgi.mli for the full copyright message.

     This module is a drop-in replacement of [Netencoding.Url].  Not
     only it is much faster (about 10 times) but it also avoids the
     dependency on PCRE (less dependencies are good, especially on
     windows or if you need to install by hand).
   *)

  (* Decoding -------------------------------------------------- *)
  exception Hex_of_char

  let hex_of_char =
    let code_a = Char.code 'a' - 10
    and code_A = Char.code 'A' - 10 in
    function
    | '0' .. '9' as c -> Char.code c - Char.code '0'
    | 'a' .. 'f' as c -> Char.code c - code_a
    | 'A' .. 'F' as c -> Char.code c - code_A
    | _ -> raise Hex_of_char

  (* Overwrite the part of the range [s.[i0 .. up-1]] with the decoded
     string.  Returns [i] such that [s.[i0 .. i]] is the decoded
     string.  Invalid '%XX' are left unchanged.  *)
  let rec decode_range_loop plus i0 i up s =
    if i0 >= up then i else begin
      match String.unsafe_get s i0 with
      | '+' ->
	  String.unsafe_set s i plus;
	  decode_range_loop plus (succ i0) (succ i) up s
      | '%' when i0 + 2 < up ->
          let i1 = succ i0 in
          let i2 = succ i1 in
          let i0_next =
            try
              let v = hex_of_char(String.unsafe_get s i1) lsl 4
                + hex_of_char(String.unsafe_get s i2) in
	      String.unsafe_set s i (Char.chr v);
	      succ i2
            with Hex_of_char ->
	      String.unsafe_set s i '%';
	      i1 in
	  decode_range_loop plus i0_next (succ i) up s
      | c ->
	  String.unsafe_set s i c;
	  decode_range_loop plus (succ i0) (succ i) up s
    end

  (* We do not strip heading and trailing spaces of key-value data
     because it does not conform the specs.  However certain browsers
     do it, so the user should not rely on them.  See e.g.
     https://bugzilla.mozilla.org/show_bug.cgi?id=114997#c6 *)

  let decode ?(plus=true) ?(pos=0) ?len s =
    let real_len =
      match len with
	| None -> String.length s - pos
	| Some l -> l in
    let s = String.sub s pos real_len in
    let up = decode_range_loop (if plus then ' ' else '+') 0 0 real_len s in
    if up <> real_len then String.sub s 0 up else s


  (* Query parsing -------------------------------------------------- *)

  (* It is ASSUMED that the range is valid i.e., [0 <= low] and [up <=
     String.length s].  *)
  let decode_range s low up =
    if low >= up then "" else
      let len = up - low in
      let s = String.sub s low len in
      let up = decode_range_loop ' ' 0 0 len s in
      if up <> len then String.sub s 0 up else s

  (* Split the query string [qs] into a list of pairs (key,value).
     [i0] is the initial index of the key or value, [i] the current
     index and [up-1] the last index to scan. *)
  let rec get_key qs i0 i up =
    if i >= up then [(decode_range qs i0 up, "")] else
      match String.unsafe_get qs i with
      | '=' -> get_val qs (i+1) (i+1) up (decode_range qs i0 i)
      | '&' ->
	  (* key but no val *)
	  (decode_range qs i0 i, "") :: get_key qs (i+1) (i+1) up
      | _ ->
	  get_key qs i0 (i+1) up
  and get_val qs i0 i up key =
    if i >= up then [(key, decode_range qs i0 up)] else
      match String.unsafe_get qs i with
      | '&' -> (key, decode_range qs i0 i) :: get_key qs (i+1) (i+1) up
      | _ -> get_val qs i0 (i+1) up key


  let dest_url_encoded_parameters qs =
    if qs = "" then [] else get_key qs 0 0 (String.length qs)



  (* Encoding -------------------------------------------------- *)

  let hex = [| '0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9';
	       'A'; 'B'; 'C'; 'D'; 'E'; 'F' |]
  let char_of_hex i = Array.(*unsafe_*)get hex i

  let encode_wrt is_special s0 =
    let len = String.length s0 in
    let encoded_length = ref len in
    for i = 0 to len - 1 do
      if is_special(String.unsafe_get s0 i) then
	encoded_length := !encoded_length + 2
    done;
    let s = String.create !encoded_length in
    let rec do_enc i0 i = (* copy the encoded string in s *)
      if i0 < len then begin
	let s0i0 = String.unsafe_get s0 i0 in
	(* It is important to check first that [s0i0] is special in
	   case [' '] is considered as such a character. *)
	if is_special s0i0 then begin
          let c = Char.code s0i0 in
          let i1 = succ i in
          let i2 = succ i1 in
          String.unsafe_set s i '%';
          String.unsafe_set s i1 (char_of_hex (c lsr 4));
          String.unsafe_set s i2 (char_of_hex (c land 0x0F));
          do_enc (succ i0) (succ i2)
	end
	else if s0i0 = ' ' then begin
	  String.unsafe_set s i '+';
          do_enc (succ i0) (succ i)
	end
	else begin
          String.unsafe_set s i s0i0;
          do_enc (succ i0) (succ i)
	end
      end in
    do_enc 0 0;
    s


  (* Unreserved characters consist of all alphanumeric chars and the
     following limited set of punctuation marks and symbols: '-' | '_' |
     '.' | '!' | '~' | '*' | '\'' | '(' | ')'.  According to RFC 2396,
     they should not be escaped unless the context requires it. *)
  let special_rfc2396 = function
    | ';' | '/' | '?' | ':' | '@' | '&' | '=' | '+' | '$' | ',' (* Reserved *)
    | '\000' .. '\031' | '\127' .. '\255' (* Control chars and non-ASCII *)
    | '<' | '>' | '#' | '%' | '"'         (* delimiters *)
    | '{' | '}' | '|' | '\\' | '^' | '[' | ']' | '`' (* unwise *)
    | '~' (* causes sometimes problems *)
	-> true
    | _ -> false
  (* ' ' must also be encoded but its encoding '+' takes a single char. *)

  let encode ?(plus=true) s =
    let is_special =
      if plus then special_rfc2396
      else (fun c -> special_rfc2396 c || c = ' ') in
    encode_wrt is_special s

  let mk_url_encoded_parameters params =
    String.concat "&"
      (List.map (fun (name, value) -> encode name ^ "=" ^ encode value) params)

end
