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

let rec simplify = function
  | Not (Not b) -> b
  | Not (True) -> False
  | Not (False) -> True
  | And [] -> True
  | And (False :: lst) -> False
  | And (True :: lst) -> simplify (And lst)
  | And (oth :: lst) -> (match simplify (And lst) with And lst' -> And (oth :: lst') | True -> oth | False -> False | oth' -> And (oth :: oth' :: []))
  | Or [] -> False
  | Or (False :: lst) -> simplify (Or lst)
  | Or (True :: lst) -> True
  | Or (oth :: lst) -> (match simplify (Or lst) with Or lst' -> Or (oth :: lst') | False -> oth | True -> True| oth' -> And (oth :: oth' :: []))
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

let rec dump' = function
| Atom x -> print_string x
| Or lst -> let delim = ref '(' in List.iter (fun itm -> print_char !delim; dump' itm; delim := '+') lst; print_char ')'
| And lst -> let delim = ref '(' in List.iter (fun itm -> print_char !delim; dump' itm; delim := '.') lst; print_char ')'
| Not x -> print_char '('; dump' x; print_string ")'"
| True -> print_string "1"
| False -> print_string "0"
| Undecidable -> print_string "?"

let dump f x = dump' (f x); print_newline()
