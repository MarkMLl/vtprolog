(*  1. The only animals in this house are cats;                         *)
(*  2. Every animal is suitable for a pet, that loves to gaze at the moon; *)
(*  3. When I detest an animal, I avoid it;                             *)
(*  4. No animals are carnivorous, unless they prowl at night;          *)
(*  5. No cat fails to kill mice;                                       *)
(*  6. No animals ever take to me, except what are in this house;       *)
(*  7. Kangaroos are not suitable for pets;                             *)
(*  8. None but carnivora kill mice;                                    *)
(*  9. I detest animals that do not take to me;                         *)
(* 10. Animals, that prowl at night, always love to gaze at the moon.   *)

in_this_house(cats) .
pet(X) :- gazes_at_moon(X) .
detest(X) :- avoid(X) .
prowls_at_night(X) :- carnivore(X) .
kills_mice(cats) .
takes_to_me(X) :- in_this_house(X) .
not_pet(kangaroos) .
carnivore(X) :- kills_mice(X) .
detest(X) :- not_take_to_me(X) .
gazes_at_moon(X) :- prowls_at_night(X) .

?- pet(X) .

(* There are actually two sub-puzzles in here without connection:       *)
(*                                                                      *)
(*  1. The only animals in this house are cats;                         *)
(*  3. When I detest an animal, I avoid it;                             *)
(*  6. No animals ever take to me, except what are in this house;       *)
(*  7. Kangaroos are not suitable for pets;                             *)
(*  9. I detest animals that do not take to me;                         *)
(*                                                                      *)
(*  2. Every animal is suitable for a pet, that loves to gaze at the moon; *)
(*  4. No animals are carnivorous, unless they prowl at night;          *)
(*  5. No cat fails to kill mice;                                       *)
(*  8. None but carnivora kill mice;                                    *)
(* 10. Animals, that prowl at night, always love to gaze at the moon.   *)
(*                                                                      *)
(* Noting also that (7) is completely disjoint. The classic query "what *)
(* animal is suitable as a pet?" can be completely answered by the      *)
(* second set, leaving the first as a distraction.                      *)

