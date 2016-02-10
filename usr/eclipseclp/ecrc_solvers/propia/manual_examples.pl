:- lib(propia).

mymember(H,[H|_T]).
mymember(X,[_Y|T]) :- mymember(X,T).

notin3to6(X) :- X#<3.
notin3to6(X) :- X#>6.

noclash(ST1,_D1,ST2,D2) :-
    ST1 #>= ST2+D2.
noclash(ST1,D1,ST2,_D2) :-
    ST2 #>= ST1+D1.

and(true,true,true).
and(true,false,false).
and(false,true,false).
and(false,false,false).

product(p1,1,19,1).
product(p2,2,17,2).
product(p3,3,15,3).
product(p4,4,13,4).
product(p5,10,8,5).
product(p6,16,4,4).
product(p7,17,3,3).
product(p8,18,2,2).
product(p9,19,1,1).

sum([],0,0,0).
sum([Name|Products],Count1,Count2,Profit) :- 
    [Count1,Count2,Profit]::0..100,
    product(Name,Ct1a,Ct2a,Profita) infers most,
    Count1 #= Ct1a+Ct1b,
    Count2 #= Ct2a+Ct2b,
    Profit #= Profita+Profitb,
    sum(Products,Ct1b,Ct2b,Profitb).

solve(Products,Batch,Max1,Max2,MinProfit) :-
    length(Products,Batch),
    Comp1 #<= Max1,
    Comp2 #<= Max2,
    Profit #>= MinProfit,
    sum(Products,Comp1,Comp2,Profit),
    labeling(Products).


