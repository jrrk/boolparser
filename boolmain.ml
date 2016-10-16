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

let (bool_lst) = ref []

let parse_bool_ast () =
 try while true do
  print_string ("Simplify: "); flush stdout;
  let linbuf = input_line stdin in
  let rslt = parse_bool_ast_from_string linbuf in
  dump simplify rslt;
  bool_lst := rslt :: !bool_lst
 done with End_of_file -> ()

let _ = let args = Array.length Sys.argv - 1 in
        if args = 0 then parse_bool_ast ()
        else for i = 1 to args do let rslt = parse_bool_ast_from_string Sys.argv.(i) in dump simplify rslt; bool_lst := rslt :: !bool_lst; done
