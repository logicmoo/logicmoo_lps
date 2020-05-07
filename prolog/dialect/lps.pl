:- module(lps, [on_lps_read/1]).

:- rexport(library(lps_syntax)).

:- rexport('../../engine/interpreter.P').

:- module_transparent(on_lps_read/1).
on_lps_read(T):- notrace(T == end_of_file), !, listing(example_query/1).
on_lps_read(:- T):- must(T),!.
on_lps_read(T):- on_lps_read1(T).

on_lps_read1((askable G)):- !,
   assertz(askabl(G)).   
on_lps_read1((assumable G)) :- !, 
   assertz(assumabl(G)).   


on_lps_read1(example_query(X)):- !,assertz(example_query(X)).
on_lps_read1(random(X)):- !, random(X). 
on_lps_read1(T):- \+ predicate_property(lps:T, imported_from(_)), !, must(lps:T).
on_lps_read1(T):- must(T).

:- include(library(dialect/lps_shared/dialect_loader_incl)).

prolog:dialect_reads(lps, on_lps_read).

