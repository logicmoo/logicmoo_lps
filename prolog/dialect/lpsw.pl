/** <module> LPSW Compatibility module

This  module  provides  compatibility  to   LPSW  through  the  directive
expects_dialect/1:

	==
	:- expects_dialect(lpsw)
	==

@tbd this module meeds

	* Implement system predicates available in LPSW we do not yet or
	do not wish to support in SWI-Prolog.  Export these predicates.

	* Provide lpsw_<name>(...) predicates for predicates that exist
	both in LPSW and SWI-Prolog and define goal_expansion/2 rules to
	map calls to these predicates to the lpsw_<name> version.
	Export these predicates.

	* Alter the library search path, placing dialect/lpsw *before*
	the system libraries.

	* Allow for =|.lpsw|= extension as extension for Prolog files.
	If both a =|.pl|= and =|.lpsw|= is present, the =|.lpsw|= file
	is loaded if the current environment expects LPSW.

@tbd	The dialect-compatibility packages are developed in a
	`demand-driven' fashion.  Please contribute to this package. Fill it in!
@author Douglas R. Miles
*/

:- module(lpsw, [pop_lpsw_dialect/0,push_lpsw_dialect/0]).


		 /*******************************
		 *	     EXPANSION		*
		 *******************************/

:- multifile
	user:goal_expansion/2,
	user:file_search_path/2,
	user:prolog_file_type/2,
	lpsw_gOAL_expansion/2.
	
:- dynamic
	user:goal_expansion/2,
	user:file_search_path/2,
	user:prolog_file_type/2.

:- notrace(interpreter:ensure_loaded(library('../engine/interpreter.P'))).
:- notrace(user:use_module(library('../swish/term_expander.pl'))).
:- notrace(lpsw_repl:ensure_loaded(library(lpsw_corner))).


% lpsw_debug(_).
 lpsw_debug(X):- format(user_error,'~N% LPSW_DEBUG: ~q.~n',[X]),flush_output(user_error).

%%	lpsw_gOAL_expansion(+In, +Out)
%
%	goal_expansion rules to emulate LPSW behaviour in SWI-Prolog. The
%	expansions  below  maintain  optimization    from   compilation.
%	Defining them as predicates would loose compilation.

lpsw_gOAL_expansion(expects_dialect(SWI), pop_lpsw_dialect):- swi == SWI, !.
/*
lpsw_gOAL_expansion(eval_arith(Expr, Result),
	      Result is Expr).

lpsw_gOAL_expansion(if(Goal, Then),
	      (Goal *-> Then; true)).
lpsw_gOAL_expansion(if(Goal, Then, Else),
	      (Goal *-> Then; Else)).
lpsw_gOAL_expansion(style_check(Style),
	      lpsw_style_check(Style)).

*/

		 /*******************************
		 *	    LIBRARY SETUP	*
		 *******************************/

%%	push_lpsw_library
%
%	Pushes searching for  dialect/lpsw  in   front  of  every library
%	directory that contains such as sub-directory.

push_lpsw_library :-
	(   absolute_file_name(library(dialect/lpsw), Dir,
			       [ file_type(directory),
				 access(read),
				 solutions(all),
				 file_errors(fail)
			       ]),
	    asserta((user:file_search_path(library, Dir) :-
		    prolog_load_context(dialect, lpsw))),
	    fail
	;   push_lpsw_library_part2
	).

push_lpsw_library_part2 :-
         prolog_load_context(directory, ThisDir),
         absolute_file_name('../..', Dir,
			       [ file_type(directory),
				 access(read),
                                 relative_to(ThisDir),
				 file_errors(fail)
			       ]),
	    asserta((user:file_search_path(lpsw_library, Dir))).


%%	push_lpsw_file_extension
%
%	Looks for .lpsw files before looking for .pl files if the current
%	dialect is =lpsw=.

push_lpsw_file_extension :-
	asserta((user:prolog_file_type(lpsw, prolog) :-
		    prolog_load_context(dialect, lpsw))).


:- push_lpsw_library,
   push_lpsw_file_extension.


:- multifile
	prolog:message//1.

prolog:message(lpsw_unsupported(Goal)) -->
	[ 'LPSW emulation (lpsw.pl): unsupported: ~p'-[Goal] ].


calc_dialect_module(M):- 
     '$current_typein_module'(TM), 
     prolog_load_context(module,Load),strip_module(_,Strip,_),
     context_module(Ctx),'$current_source_module'(SM),
     ((TM\==Load,TM\==user) -> M = TM ; (M = SM)),
     once(true;lpsw_debug([ti=TM,load=Load,strip=Strip,ctx=Ctx,sm=SM,lpsw=M])).     


   :- volatile(tmp:module_dialect_lpsw/4).
:- thread_local(tmp:module_dialect_lpsw/4).

:- system:module_transparent(lpsw:setup_dialect/0). 
:- system:module_transparent(lpsw:pop_lpsw_dialect/0).
:- system:module_transparent(lpsw:push_lpsw_dialect/0).
:- system:module_transparent(lpsw:push_lpsw_dialect/2).

setup_dialect:- notrace(push_lpsw_dialect)->true;(trace,push_lpsw_dialect).

% get_lpsw_alt_user_module( user, db):-!.
get_lpsw_alt_user_module( User,LPSW_USER):- is_lpsw_alt_user_module(User,LPSW_USER),!.
get_lpsw_alt_user_module(_User,LPSW_USER):- interpreter:lpsw_program_module(LPSW_USER),!.

% is_lpsw_alt_user_module(user,db):-!.
is_lpsw_alt_user_module(_User,Out):- gensym(lpsw, Out).

% is_lpsw_alt_user_module(db).


push_lpsw_dialect:-
   calc_dialect_module(M),
   push_lpsw_dialect(M, M).   
  
push_lpsw_dialect(User, User):-  
  User==user,
  get_lpsw_alt_user_module(User,LPSW_USER),
  LPSW_USER\==user,
  lpsw_debug(alt_module(User,LPSW_USER)),
  '$set_source_module'(LPSW_USER),!,
  push_lpsw_dialect(User, LPSW_USER).

push_lpsw_dialect(Was, M):-
   interpreter:check_lpsw_program_module(M),
   current_input(In),
   style_check(-discontiguous), style_check(-singleton),
   push_operators([],Undo),
   %ignore(retract(tmp:module_dialect_lpsw(In,_,_,_))),     
   asserta(tmp:module_dialect_lpsw(In,Was,M,Undo)),!.


pop_lpsw_dialect:-
    current_input(StreamIn),
    retract(tmp:module_dialect_lpsw(StreamIn,Was,M,Undo)),
    pop_operators(Undo),!,
    lpsw_debug(un_alt_module(Was,M)),
    nop('$set_source_module'(Was)),!.
pop_lpsw_dialect.


user:goal_expansion(In, Out) :-
    prolog_load_context(dialect, lpsw),
    lpsw_gOAL_expansion(In, Out).

system:term_expansion(In, PosIn, Out, PosOut) :- In == end_of_file,
   prolog_load_context(dialect, lpsw),
   current_input(StreamIn),
   tmp:module_dialect_lpsw(StreamIn,_,_,_),
   pop_lpsw_dialect,!,
   Out = In,
   PosIn = PosOut.
      
      



