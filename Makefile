#**************************************************************************)
#*                                                                        *)
#* OCaml template Copyright (C) 2004-2010                                 *)
#*  Sylvain Conchon, Jean-Christophe Filliatre and Julien Signoles        *)
#* Adapted to boolean logic by Jonathan Kimmitt                           *)
#*  Copyright 2016 University of Cambridge                                *)
#*                                                                        *)
#*  This software is free software; you can redistribute it and/or        *)
#*  modify it under the terms of the GNU Library General Public           *)
#*  License version 2.1, with the special exception on linking            *)
#*  described in file LICENSE.                                            *)
#*                                                                        *)
#*  This software is distributed in the hope that it will be useful,      *)
#*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
#*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
#*                                                                        *)
#**************************************************************************)

.PHONY: everything

everything: boolparser.top boolparser

boolparser.top: bool_types.mli bool_parser.mli ord.ml bool_parser.ml bool_lexer.ml bool.ml
	ocamlmktop -g -o $@ bool_types.mli bool_parser.mli ord.ml bool_parser.ml bool_lexer.ml bool.ml

boolparser: bool_types.mli bool_parser.mli ord.ml bool_parser.ml bool_lexer.ml bool.ml
	ocamlopt -g -o $@ bool_types.mli bool_parser.mli ord.ml bool_parser.ml bool_lexer.ml bool.ml

bool_lexer.ml: bool_lexer.mll
	ocamllex bool_lexer.mll

bool_parser.mli bool_parser.ml: bool_parser.mly
	ocamlyacc bool_parser.mly 

ord.ml: ord.sh bool_parser.mli
	sh ord.sh

clean:
	rm -f bool_lexer.ml bool_parser.mli bool_parser.ml boolparser boolparser.top ord.ml *.cm? *.o
