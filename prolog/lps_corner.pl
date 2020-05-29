:- module(lps_corner,[golps/1,golps/3]).

golps(Out):- golps(T,DFA, []),!,(Out=T->true;Out=DFA).
golps(T,DFAgraph,Options) :-
 /* ignore((
    \+ member(cycle_hook(_, _, _), Options),
    \+ member(background(_), Options),
    (   catch(lps_server_UI:lps_user_is_super, _, fail)
    ->  true
    ;   \+ member(timeout(_), Options)
    ))),*/
    visualizer:gojson(_File, [dc, silent|Options], [], T, DFAgraph).

:- ensure_loaded('../swish/user_module_file').

:- ensure_loaded('../swish/user_module_repl').

