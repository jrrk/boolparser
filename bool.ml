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

let rec precedence = function
| Bracket _ -> 50
| True -> 40
| False -> 40
| Undecidable -> 40
| Atom _ -> 40
| Not _ -> 30
| And _ -> 20
| Or _ -> 10

let rec bracket = function
| Not x -> if precedence x < precedence (Not x) then Not (Bracket x) else (Not x)
| And [] -> True
| And lst -> And (List.map (fun itm -> if precedence itm < precedence (And lst) then Bracket itm else itm) lst)
| Or [] -> False
| Or lst -> Or (List.map (fun itm -> if precedence itm < precedence (Or lst) then Bracket itm else itm) lst)
| oth -> oth

and prec_lst = function
 | [] -> 99
 | hd::tl -> List.fold_right (fun x -> min (precedence x)) tl (precedence hd)

let rec dump' = function
| Atom x -> print_string x
| Bracket x -> print_char '('; dump' x; print_string ")"
| Or lst -> let delim = ref ' ' in List.iter (fun itm -> print_char !delim; dump' itm; delim := '+') lst
| And lst -> let delim = ref ' ' in List.iter (fun itm -> print_char !delim; dump' itm; delim := '.') lst
| Not x -> dump' x; print_string "'"
| True -> print_string "1"
| False -> print_string "0"
| Undecidable -> print_string "?"

let dump' x = dump' (bracket x)
