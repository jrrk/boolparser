(**************************************************************************)
(*                                                                        *)
(* OCaml template Copyright (C) 2004-2010                                 *)
(*  Sylvain Conchon, Jean-Christophe Filliatre and Julien Signoles        *)
(* Adapted to boolean logic by Jonathan Kimmitt                           *)
(*  Copyright 2016 University of Cambridge                                *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2.1, with the special exception on linking            *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

open Bool_types
open Bool

let rec scan v = function
    | Atom x -> if not (List.mem x !v) then v := x :: !v
    | Not x -> scan v x
    | And lst -> List.iter (scan v) lst
    | Or lst -> List.iter (scan v) lst
    | oth -> ()

let rec recurs (fn:(string*string formula) list->string formula->string formula) x assoc = function
    | [] -> List.iter (fun (k,x) -> dump' x; print_char ' ') (List.sort compare assoc); dump' (fn assoc x); print_newline()
    | hd :: tl -> let rf logic = recurs fn x ((hd,logic) :: assoc) tl in rf False; rf True

let rec tabulate assoc = function
    | Atom x -> if not (List.mem_assoc x assoc) then failwith x else List.assoc x assoc
    | Not x -> simplify (Not (tabulate assoc x))
    | And lst -> simplify (And (List.map (tabulate assoc) lst))
    | Or lst -> simplify (Or (List.map (tabulate assoc) lst))
    | oth -> simplify (tabulate assoc oth)

let kmap x =
  let v = ref [] in scan v x;
  let uniq = List.sort compare !v in
  List.iter (fun k -> print_string k; print_char ' ') uniq; dump' x; print_newline();
  recurs tabulate x [] uniq
  
let rec sop = function
| And (Or lst :: lst') -> Not (Or (And (notlst lst) :: notlst lst'))
| oth -> simplify oth

and notlst x = List.map (fun itm -> simplify (Not itm)) x

let sop x = (sop (sop x))

let (bool_lst) = ref []

let parse_bool_ast () =
 try while true do
  print_string ("Sum of products: "); flush stdout;
  let linbuf = input_line stdin in
  let rslt = parse_bool_ast_from_string linbuf in
  dump sop rslt;
  bool_lst := rslt :: !bool_lst
 done with End_of_file -> ()

let _ = let args = Array.length Sys.argv - 1 in
        if args = 0 then parse_bool_ast ()
        else for i = 1 to args do let rslt = parse_bool_ast_from_string Sys.argv.(i) in dump sop rslt; bool_lst := rslt :: !bool_lst; done
