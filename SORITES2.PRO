(* This is a subset of sorites.pro, comprising the statements which are *)
(* sufficient for solving the query.                                    *)

pet(X) :- gazes_at_moon(X) .
prowls_at_night(X) :- carnivore(X) .
kills_mice(cats) .
carnivore(X) :- kills_mice(X) .
gazes_at_moon(X) :- prowls_at_night(X) .

?- pet(X) .

