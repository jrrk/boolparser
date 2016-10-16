/**************************************************************************/
/*                                                                        */
/* OCaml template Copyright (C) 2004-2010                                 */
/*  Sylvain Conchon, Jean-Christophe Filliatre and Julien Signoles        */
/* Adapted to boolean logic by Jonathan Kimmitt                           */
/*  Copyright 2016 University of Cambridge                                */
/*                                                                        */
/*  This software is free software; you can redistribute it and/or        */
/*  modify it under the terms of the GNU Library General Public           */
/*  License version 2.1, with the special exception on linking            */
/*  described in file LICENSE.                                            */
/*                                                                        */
/*  This software is distributed in the hope that it will be useful,      */
/*  but WITHOUT ANY WARRANTY; without even the implied warranty of        */
/*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  */
/*                                                                        */
/**************************************************************************/

/* $Id:$ */

%{
  open Parsing
  open Bool_types
%}

%token    NEWLINE
%token    SUPPLY0
%token    SUPPLY1
%token    LPAREN
%token    RPAREN
%token    OR
%token    AND
%token    NOT
%token <char>   CHAR
%token <string>   IDENTIFIER;
%token    EOF

%type <string Bool_types.formula> bool_line
%start bool_line
%%


/* Parser rules */

bool_line:
    decl NEWLINE { $1 }

decl:
    decl OR term { Or [ $1 ; $3 ] }
    | term { $1 }
    ;

term:
    term AND factor { And [ $1; $3 ] }
    | factor { $1 }
    ;

factor:
    IDENTIFIER { Atom $1 }
    | SUPPLY0 { False }
    | SUPPLY1 { True }
    | factor NOT { Not $1 }
    | LPAREN decl RPAREN { $2 }
    ;
