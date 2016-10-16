type ('a)formula = False
     		 | Undecidable
                 | True
                 | Atom of 'a
                 | Not of ('a)formula
                 | And of ('a)formula list
                 | Or of ('a)formula list
