% export_all.pl: Export all predicates from a module


export_all :-
  current_predicate(Pred),
  (export Pred),
  fail.
export_all.

:- export_all.
