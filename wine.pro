likes(john,X) :- likes(X,wine) , female(X) .
likes(joan,wine) .
likes(alice,candy) .
male(john) .
female(alice) .
female(joan) .

?- likes(john,Who) .

