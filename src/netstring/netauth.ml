(* $Id: netauth.ml 1543 2011-02-08 02:08:35Z gerd $ *)

let xor_s s u =
  let s_len = String.length s in
  let u_len = String.length u in
  assert(s_len = u_len);
  let x = String.create s_len in
  for k = 0 to s_len-1 do
    x.[k] <- Char.chr ((Char.code s.[k]) lxor (Char.code u.[k]))
  done;
  x

let hmac ~h ~b ~l ~k ~message =
  if String.length k > b then
    failwith "Netauth.hmac: key too long";
  
  let k_padded = k ^ String.make (b - String.length k) '\000' in
  let ipad = String.make b '\x36' in
  let opad = String.make b '\x5c' in
  h((xor_s k_padded opad) ^ (h ((xor_s k_padded ipad) ^ message)))

let add_1_complement s1 s2 =
  (* Add two bitstrings s1 and s2 (in big-endian order) with one's complement
     addition
   *)
  let l1 = String.length s1 in
  let l2 = String.length s2 in
  if l1 <> l2 then
    invalid_arg "Netauth.add_1_complement";
  let r = String.make l1 '\000' in
  let carry = ref 0 in
  for k = l1-1 downto 0 do
    let i1 = Char.code s1.[k] in
    let i2 = Char.code s2.[k] in
    let sum = i1 + i2 + !carry in
    r.[k] <- Char.chr (sum land 0xff);
    carry := if sum > 0xff then 1 else 0;
  done;
  if !carry > 0 then (
    for k = l1-1 downto 0 do
      let i = Char.code r.[k] in
      let sum = i + !carry in
      r.[k] <- Char.chr (sum land 0xff);
      carry := if sum > 0xff then 1 else 0;
    done
  );
  r


let rotate_right n s =
  (* Rotate the (big-endian) bitstring s to the right by n bits *)
  let l = String.length s in
  let b = 8 * l in  (* bit length of s *)
  let n' = n mod b in
  let n' = if n' < 0 then b+n' else n' in
  let u = String.create l in
  (* First byte-shift the string, then bit-shift the remaining 0-7 bits *)
  let bytes = n' lsr 3 in
  let bits = n' land 7 in
  String.blit s 0 u bytes (l-bytes);
  if bytes > 0 then
    String.blit s (l-bytes) u 0 bytes;
  let mask =
    match bits with
      | 0 -> 0
      | 1 -> 1
      | 2 -> 3
      | 3 -> 7 
      | 4 -> 15
      | 5 -> 31
      | 6 -> 63
      | 7 -> 127 
      | _ -> assert false in
  let carry = ref 0 in
  if bits > 0 && l > 0 then (
    for k = 0 to l-1 do
      let x = Char.code u.[k] in
      u.[k] <- Char.chr ((x lsr bits) lor (!carry lsl (8-bits)));
      carry := x land mask;
    done;
    u.[0] <- Char.chr((Char.code u.[0]) lor (!carry lsl (8-bits)));
  );
  u

let n_fold n s =
  (** n-fold the number given by the bitstring s. The length of the number
      is taken as the byte-length of s. n must be divisible by 8.
   *)
  if n=0 || n mod 8 <> 0 then
    invalid_arg "Netauth.n_fold";
  let p = n / 8 in
  let buf = Buffer.create (String.length s) in
  let rec add_rot u len =
    if len > 0 && len mod p = 0 then
      ()
    else (
      Buffer.add_string buf u;
      add_rot (rotate_right 13 u) (len+String.length u)
    ) in
  add_rot s 0;
  let blen = Buffer.length buf in
  let s = ref (Buffer.sub buf 0 p) in
  for k = 1 to (blen / p) - 1 do
    s := add_1_complement !s (Buffer.sub buf (k*p) p)
  done;
  !s


type key_type =
    [ `Kc | `Ke | `Ki ]

let k_truncate k s =
  let b = k/8 in
  String.sub s 0 b

let derive_key_rfc3961_simplified
      ~encrypt ~random_to_key ~block_size ~k ~usage ~key_type =
  if block_size < 40 then
    invalid_arg "Netauth.derive_key_rfc3961: bad block_size";
  if k <= 0 || k mod 8 <> 0 then
    invalid_arg "Netauth.derive_key_rfc3961: bad k";
  if usage < 0 || usage > 255 then
    invalid_arg "Netauth.derive_key_rfc3961: bad usage (only 0-255 allowed)";
  let usage_s =
    String.make 3 '\000' ^ String.make 1 (Char.chr usage) ^
      (match key_type with
	 | `Kc -> "\x99"
	 | `Ke -> "\xaa"
	 | `Ki -> "\x55"
      ) in
  let usage_exp = n_fold block_size usage_s in
  let kbuf = Buffer.create 80 in
  let ki = ref (encrypt usage_exp) in
  Buffer.add_string kbuf !ki;
  while 8*(Buffer.length kbuf) < k do
    ki := encrypt !ki;
    Buffer.add_string kbuf !ki
  done;
  let derived_random = k_truncate k (Buffer.contents kbuf) in
  random_to_key derived_random
