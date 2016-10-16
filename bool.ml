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

let simplify = function
  | Not (Not b) -> b
  | Not (True) -> False
  | Not (False) -> True
  | And (False :: lst) -> False
  | And (True :: lst) -> And lst
  | Or (False :: lst) -> Or lst
  | Or (True :: lst) -> True
  | oth -> oth

let parse_bool_ast_from_string s =
  let lb = Lexing.from_string (s^"\n") in
  let bool = try
      Bool_parser.bool_line Bool_lexer.token lb
  with
    | Parsing.Parse_error ->
      let n = Lexing.lexeme_start lb in
      Printf.printf "Bool.parse: parse error at character %d\n" n;
      Undecidable
(*
    | _ ->
      failwith (Printf.sprintf "Parser error at line %d" !Scope.lincnt)
*)
  in
  bool

let (bool_lst) = ref []

let rec dump = function
| Atom x -> print_string ("\""^x^"\"")
| Or lst -> let delim = ref '(' in List.iter (fun itm -> print_char !delim; dump itm; delim := '+') lst; print_char ')'
| And lst -> let delim = ref '(' in List.iter (fun itm -> print_char !delim; dump itm; delim := '.') lst; print_char ')'
| Not x -> print_char '('; dump x; print_string ")'"
| True -> print_string "1"
| False -> print_string "0"
| Undecidable -> print_string "?"

let dump x = dump (simplify x); print_newline()

let parse_bool_ast () =
 try while true do
  let linbuf = input_line stdin in
  let rslt = parse_bool_ast_from_string linbuf in
  dump rslt;
  bool_lst := rslt :: !bool_lst
 done with End_of_file -> ()

let _ = for i = 1 to (Array.length Sys.argv - 1) do let rslt = parse_bool_ast_from_string Sys.argv.(i) in dump rslt; bool_lst := rslt :: !bool_lst; done
