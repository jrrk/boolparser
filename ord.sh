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

echo open String > ord.ml
echo open Bool_parser >> ord.ml
echo let getstr = function >> ord.ml
sed '/exception/,$d' bool_parser.mli | grep '\ [A-Z][A-Za-z_]' | cut -d\( -f1 | tr '\011' ' ' |\
sed -e 's=[|\ ]*\([A-Z][A-Za-z0-9_\ o]*\)=|\ \1 -> lowercase(\"\1\") \;=' -e 's= of [A-Za-z0-9\ ]*= _=' -e 's= of[A-Za-z0-9\ ]*==' | cut -d\; -f1 | sort >> ord.ml
