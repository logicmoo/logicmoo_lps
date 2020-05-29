/** <module> LPS Compatibility module

This  module  provides  compatibility  to   LPS  through  the  directive
expects_dialect/1:

	==
	:- expects_dialect(lps)
	==

@tbd this module meeds

	* Implement system predicates available in LPS we do not yet or
	do not wish to support in SWI-Prolog.  Export these predicates.

	* Provide lps_<name>(...) predicates for predicates that exist
	both in LPS and SWI-Prolog and define goal_expansion/2 rules to
	map calls to these predicates to the lps_<name> version.
	Export these predicates.

	* Alter the library search path, placing dialect/lps *before*
	the system libraries.

	* Allow for =|.lps|= extension as extension for Prolog files.
	If both a =|.pl|= and =|.lps|= is present, the =|.lps|= file
	is loaded if the current environment expects LPS.

@tbd	The dialect-compatibility packages are developed in a
	`demand-driven' fashion.  Please contribute to this package. Fill it in!
@author Douglas R. Miles
*/

:- module(lps, [pop_lps_dialect/0,push_lps_dialect/0]).


		 /*******************************
		 *	     EXPANSION		*
		 *******************************/

:- multifile
	user:goal_expansion/2,
	user:file_search_path/2,
	user:prolog_file_type/2,
	lps_gOAL_expansion/2.
	
:- dynamic
	user:goal_expansion/2,
	user:file_search_path/2,
	user:prolog_file_type/2.

:- notrace(interpreter:ensure_loaded(library('../engine/interpreter.P'))).
:- notrace(user:use_module(library('../swish/term_expander.pl'))).
:- notrace(lps_repl:ensure_loaded(library('../swish/user_module_repl.pl'))).

%%	lps_gOAL_expansion(+In, +Out)
%
%	goal_expansion rules to emulate LPS behaviour in SWI-Prolog. The
%	expansions  below  maintain  optimization    from   compilation.
%	Defining them as predicates would loose compilation.

lps_gOAL_expansion(expects_dialect(SWI), pop_lps_dialect):- swi == SWI, !.
/*
lps_gOAL_expansion(eval_arith(Expr, Result),
	      Result is Expr).

lps_gOAL_expansion(if(Goal, Then),
	      (Goal *-> Then; true)).
lps_gOAL_expansion(if(Goal, Then, Else),
	      (Goal *-> Then; Else)).
lps_gOAL_expansion(style_check(Style),
	      lps_style_check(Style)).

*/

		 /*******************************
		 *	    LIBRARY SETUP	*
		 *******************************/

%%	push_lps_library
%
%	Pushes searching for  dialect/lps  in   front  of  every library
%	directory that contains such as sub-directory.

push_lps_library :-
	(   absolute_file_name(library(dialect/lps), Dir,
			       [ file_type(directory),
				 access(read),
				 solutions(all),
				 file_errors(fail)
			       ]),
	    asserta((user:file_search_path(library, Dir) :-
		    prolog_load_context(dialect, lps))),
	    fail
	;   push_lps_library_part2
	).

push_lps_library_part2 :-
         prolog_load_context(directory, ThisDir),
         absolute_file_name('../..', Dir,
			       [ file_type(directory),
				 access(read),
                                 relative_to(ThisDir),
				 file_errors(fail)
			       ]),
	    asserta((user:file_search_path(lps_library, Dir))).


%%	push_lps_file_extension
%
%	Looks for .lps files before looking for .pl files if the current
%	dialect is =lps=.

push_lps_file_extension :-
	asserta((user:prolog_file_type(lps, prolog) :-
		    prolog_load_context(dialect, lps))).


:- push_lps_library,
   push_lps_file_extension.


		 /*******************************
		 *	 SYSTEM PREDICATES	*
		 *******************************/

%%	gc
%
%	Garbage collect.
%
%	@compat lps

gc :-
	garbage_collect.

%%	depth_bound_call(:Goal, :Limit)
%
%	Equivalent to call_with_depth_limit(Goal, Limit, _Reached)
%
%	@compat lps

:- module_transparent
	depth_bound_call/2.

depth_bound_call(G, L) :-
	call_with_depth_limit(G, L, _).

%%	system(+Command)
%
%	Equivalent to shell(Command).
%
%	@compat lps

system(Command) :-
	shell(Command).

%%	exists(+File)
%
%	Equivalent to exists_file(File).
%
%	@compat lps

exists(File) :-
	exists_file(File).

%%	assert_static(:Term)
%
%	Assert    as    static    predicate.      SWI-Prolog    provides
%	compile_predicates/1 to achieve this. The   emulation  is a mere
%	alias for assert/1, as  immediate   compilation  would  prohibit
%	further calls to this predicate.
%
%	@compat lps
%	@deprecated Use assert/1 and compile_predicates/1 after
%	completing the predicate definition.

:- module_transparent
	assert_static/1.

assert_static(Term) :-
	assert(Term).


%%	source is det.
%
%	LPS directive to  maintain  source-information.   We  have  that
%	always.

source.


%%	lps_flag(+Key, +Value) is det.
%
%	Map some LPS flags to SWI-Prolog.  Supported flags:
%
%	    * write_strings: Bool
%	    If =on=, writes strings as "..." instead of a list of
%	    integers.  In SWI-Prolog this only affects write routines
%	    that use portray.

lps_flag(write_strings, OnOff) :- !,
	map_bool(OnOff, Bool),
	set_prolog_flag(write_strings, Bool).
lps_flag(Flag, Value) :-
	fixme_true(lps_flag(Flag, Value)).

map_bool(on, true) :- !.
map_bool(off, false) :- !.
map_bool(Bool, Bool).

:- multifile
	user:portray/1.

user:portray(String) :-
	current_prolog_flag(write_strings, true),
	is_list(String),
	length(String, L),
	L > 2,
	maplist(printable, String),
	format('"~s"', [String]).

printable(C) :-	code_type(C, graph), !.
printable(C) :-	code_type(C, space), !.


%%	lps_style_check(+Style) is det.
%
%	Map LPS style-check options onto the SWI-Prolog ones.

lps_style_check(all) :- !,
	system:style_check([ +singleton,
			     +discontiguous
			   ]).
lps_style_check(Style) :-
	fixme_true(lps_style_check(Style)).


		 /*******************************
		 *	   UNIMPLEMENTED		*
		 *******************************/

:- dynamic
	fixme_reported/1.

fixme_true(Goal) :-
	fixme_reported(Goal), !.
fixme_true(Goal) :-
	print_message(warning, lps_unsupported(Goal)),
	assert(fixme_reported(Goal)).


:- multifile
	prolog:message//1.

prolog:message(lps_unsupported(Goal)) -->
	[ 'LPS emulation (lps.pl): unsupported: ~p'-[Goal] ].


calc_dialect_module(M):- 
     '$current_typein_module'(TM), 
     prolog_load_context(module,Load),strip_module(_,Strip,_),
     context_module(Ctx),'$current_source_module'(SM),
     ((TM\==Load,TM\==user) -> M = TM ; (M = SM)),
     once(true;writeln([ti=TM,load=Load,strip=Strip,ctx=Ctx,sm=SM,lps=M])).     


   :- volatile(tmp:module_dialect_lps/3).
:- thread_local(tmp:module_dialect_lps/3).

:- system:module_transparent(lps:push_lps_dialect/0).
:- system:module_transparent(lps:pop_lps_dialect/0).
:- system:module_transparent(lps:setup_dialect/0).
setup_dialect :- push_lps_dialect,!.

push_lps_dialect:-
   calc_dialect_module(M),
   current_input(In),
   style_check(-discontiguous), style_check(-singleton),
   push_operators([
     op(900,fy,(M:not)), 
     op(1200,xfx,(M:then)),
     op(1185,fx,(M:if)),
     op(1200,xfx,(M:if)),
     op(1100,xfy,(M:else)), 
     op(1050,xfx,(M:terminates)),
     op(1050,xfx,(M:initiates)),
     op(1050,xfx,(M:updates)),
% Rejected    (      op(1050,fx,impossible), 
     op(1050,fx,(M:observe)),
     op(1050,fx,(M:false)),
     op(1050,fx,(M:initially)),
     op(1050,fx,(M:fluents)),
     op(1050,fx,(M:events)),
     op(1050,fx,(M:prolog_events)),
     op(1050,fx,(M:actions)),
     op(1050,fx,(M:unserializable)),
% notice ',' has priority 1000
     op(999,fx,(M:update)),
     op(999,fx,(M:initiate)),
     op(999,fx,(M:terminate)),
     op(997,xfx,(M:in)),
     op(995,xfx,(M:at)),
     op(995,xfx,(M:during)),
     op(995,xfx,(M:from)), 
     op(994,xfx,(M:to)), % from's priority higher
     op(1050,xfy,(M:(::))),

% lps.js syntax extras
     op(1200,xfx,(M:(<-))),
     op(1050,fx,(M:(<-))),
% -> is already defined as 1050, xfy, which will do given that lps.js does not support if-then-elses
     op(700,xfx,((M:(<=))))],Undo),
   ignore(retract(tmp:module_dialect_lps(In,_,_))),     
   asserta(tmp:module_dialect_lps(In,M,Undo)),!.


pop_lps_dialect:-
    current_input(StreamIn),
    retract(tmp:module_dialect_lps(StreamIn,_WasM,Undo)),
    pop_operators(Undo).

user:goal_expansion(In, Out) :-
    prolog_load_context(dialect, lps),
    lps_gOAL_expansion(In, Out).

user:term_expansion(In, PosIn, Out, PosOut) :- In == end_of_file,
   prolog_load_context(dialect, lps),
   current_input(StreamIn),
   tmp:module_dialect_lps(StreamIn,_WasM,_Undo),
   pop_lps_dialect,!,
   Out = In,
   PosIn = PosOut.
      




