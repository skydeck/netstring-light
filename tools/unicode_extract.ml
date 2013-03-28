(* $Id$ *)

(* This is a script to be interpreted with "ocaml". It generates
   the unicode_charinfo.txt file
 *)

#use "topfind";;
#require "camomile";;

open Printf
open CamomileLibraryDefault

let lc_table =
  Camomile.UCharInfo.load_to_lower1_tbl()

let uc_table =
  Camomile.UCharInfo.load_to_upper1_tbl()

let tc_table =
  Camomile.UCharInfo.load_to_title1_tbl()

type range =
    { mutable valid : bool;
      mutable length : int;
      mutable map_from : int;
      mutable map_to : int;
    }

let new_range() =
  { valid = false;
    length = 0;
    map_from = 0;
    map_to = 0
  }

let init_range range p q =
  range.valid <- true;
  range.length <- 1;
  range.map_from <- p;
  range.map_to <- q
    
let print_range label range =
  printf "%s %d-%d %d-%d\n"
    label
    range.map_from
    (range.map_from + range.length - 1)
    range.map_to
    (range.map_to + range.length - 1)


let acc_range label range p q =
  if range.valid then (
    let l = range.length in
    if range.map_from + l = p && range.map_to + l = q then (
      range.length <- l+1
    )
    else (
      print_range label range;
      init_range range p q
    )
  )
  else
    init_range range p q
      

let () =
  let range_lc = new_range() in
  let range_uc = new_range() in
  let range_tc = new_range() in
  for c = 0 to 0x10ffff do
    let uchar = Camomile.UChar.chr c in
    let uchar_lc = Camomile.UCharTbl.get lc_table uchar in
    let c_lc0 = Camomile.UChar.code uchar_lc in
    if c_lc0 <> 0 then
      acc_range "L" range_lc c c_lc0;

    let uchar_uc = Camomile.UCharTbl.get uc_table uchar in
    let c_uc0 = Camomile.UChar.code uchar_uc in
    if c_uc0 <> 0 then
      acc_range "U" range_uc c c_uc0;

    let uchar_tc = Camomile.UCharTbl.get tc_table uchar in
    let c_tc0 = Camomile.UChar.code uchar_tc in
    if c_tc0 <> 0 then
      acc_range "T" range_tc c c_tc0

  done;
  print_range "L" range_lc;
  print_range "U" range_uc;
  print_range "T" range_tc

