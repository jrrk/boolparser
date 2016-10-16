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

{
  open Lexing
  open Bool_parser

  let keyword =
    let h = Hashtbl.create 17 in
    List.iter 
      (fun (k,s) -> Hashtbl.add h s k)
      [
    SUPPLY0, "false";
    SUPPLY1, "true";
      ];
    fun s -> let s = String.lowercase s in Hashtbl.find h s

}

let ident = ['a'-'z' 'A'-'Z']+
let space = [' ' '\t' '\r']+
let newline = ['\n']

rule token = parse
  | space
      { token lexbuf }
  | newline
      { NEWLINE }
  | '('
      { LPAREN }
  | ')'
      { RPAREN }
  | '+'
      { OR }
  | '.'
      { AND }
  | '\''
      { NOT }
  | '0'
      { SUPPLY0 }
  | '1'
      { SUPPLY1 }
  | ident as s
      { try keyword s with Not_found -> IDENTIFIER s }
  | eof
      { EOF }
  | _ as c
      { CHAR c }
