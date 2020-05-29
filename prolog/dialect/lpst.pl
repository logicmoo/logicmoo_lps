/** <module> LPST Compatibility module

This  module  provides  compatibility  to   LPST  through  the  directive
expects_dialect/1:

	==
	:- expects_dialect(lpst)
	==

@tbd this module meeds

	* Implement system predicates available in LPST we do not yet or
	do not wish to support in SWI-Prolog.  Export these predicates.

	* Provide lpst_<name>(...) predicates for predicates that exist
	both in LPST and SWI-Prolog and define goal_expansion/2 rules to
	map calls to these predicates to the lpst_<name> version.
	Export these predicates.

	* Alter the library search path, placing dialect/lpst *before*
	the system libraries.

	* Allow for =|.lpst|= extension as extension for Prolog files.
	If both a =|.pl|= and =|.lpst|= is present, the =|.lpst|= file
	is loaded if the current environment expects LPST.

@tbd	The dialect-compatibility packages are developed in a
	`demand-driven' fashion.  Please contribute to this package. Fill it in!
@author Douglas R. Miles
*/

:- module(lpst, [pop_lpst_dialect/0,push_lpst_dialect/0]).


		 /*******************************
		 *	     EXPANSION		*
		 *******************************/

:- multifile
	user:goal_expansion/2,
	user:file_search_path/2,
	user:prolog_file_type/2,
	lpst_gOAL_expansion/2.
	
:- dynamic
	user:goal_expansion/2,
	user:file_search_path/2,
	user:prolog_file_type/2.

:- notrace(interpreter:ensure_loaded(library('../engine/interpreter.P'))).
:- notrace(user:use_module(library('../swish/term_expander.pl'))).
:- notrace(lpst_repl:ensure_loaded(library(lpst_corner))).


% lpst_debug(_).
 lpst_debug(X):- format(user_error,'~N% LPST_DEBUG: ~q.~n',[X]),flush_output(user_error).

%%	lpst_gOAL_expansion(+In, +Out)
%
%	goal_expansion rules to emulate LPST behaviour in SWI-Prolog. The
%	expansions  below  maintain  optimization    from   compilation.
%	Defining them as predicates would loose compilation.

lpst_gOAL_expansion(expects_dialect(SWI), pop_lpst_dialect):- swi == SWI, !.
/*
lpst_gOAL_expansion(eval_arith(Expr, Result),
	      Result is Expr).

lpst_gOAL_expansion(if(Goal, Then),
	      (Goal *-> Then; true)).
lpst_gOAL_expansion(if(Goal, Then, Else),
	      (Goal *-> Then; Else)).
lpst_gOAL_expansion(style_check(Style),
	      lpst_style_check(Style)).

*/

		 /*******************************
		 *	    LIBRARY SETUP	*
		 *******************************/

%%	push_lpst_library
%
%	Pushes searching for  dialect/lpst  in   front  of  every library
%	directory that contains such as sub-directory.

push_lpst_library :-
	(   absolute_file_name(library(dialect/lpst), Dir,
			       [ file_type(directory),
				 access(read),
				 solutions(all),
				 file_errors(fail)
			       ]),
	    asserta((user:file_search_path(library, Dir) :-
		    prolog_load_context(dialect, lpst))),
	    fail
	;   push_lpst_library_part2
	).

push_lpst_library_part2 :-
         prolog_load_context(directory, ThisDir),
         absolute_file_name('../..', Dir,
			       [ file_type(directory),
				 access(read),
                                 relative_to(ThisDir),
				 file_errors(fail)
			       ]),
	    asserta((user:file_search_path(lpst_library, Dir))).


%%	push_lpst_file_extension
%
%	Looks for .lpst files before looking for .pl files if the current
%	dialect is =lpst=.

push_lpst_file_extension :-
	asserta((user:prolog_file_type(lpst, prolog) :-
		    prolog_load_context(dialect, lpst))).


:- push_lpst_library,
   push_lpst_file_extension.


:- multifile
	prolog:message//1.

prolog:message(lpst_unsupported(Goal)) -->
	[ 'LPST emulation (lpst.pl): unsupported: ~p'-[Goal] ].


calc_dialect_module(M):- 
     '$current_typein_module'(TM), 
     prolog_load_context(module,Load),strip_module(_,Strip,_),
     context_module(Ctx),'$current_source_module'(SM),
     ((TM\==Load,TM\==user) -> M = TM ; (M = SM)),
     once(true;lpst_debug([ti=TM,load=Load,strip=Strip,ctx=Ctx,sm=SM,lpst=M])).     


   :- volatile(tmp:module_dialect_lpst/4).
:- thread_local(tmp:module_dialect_lpst/4).

:- system:module_transparent(lpst:setup_dialect/0). 
:- system:module_transparent(lpst:pop_lpst_dialect/0).
:- system:module_transparent(lpst:push_lpst_dialect/0).
:- system:module_transparent(lpst:push_lpst_dialect/2).

setup_dialect:- notrace(push_lpst_dialect)->true;(trace,push_lpst_dialect).

% get_lpst_alt_user_module( user, db):-!.
get_lpst_alt_user_module( User,LPST_USER):- is_lpst_alt_user_module(User,LPST_USER),!.
get_lpst_alt_user_module(_User,LPST_USER):- interpreter:lpst_program_module(LPST_USER),!.

% is_lpst_alt_user_module(user,db):-!.
is_lpst_alt_user_module(_User,Out):- gensym(lpst, Out).

% is_lpst_alt_user_module(db).


push_lpst_dialect:-
   calc_dialect_module(M),
   push_lpst_dialect(M, M).   
  
push_lpst_dialect(User, User):-  
  User==user,
  get_lpst_alt_user_module(User,LPST_USER),
  LPST_USER\==user,
  lpst_debug(alt_module(User,LPST_USER)),
  '$set_source_module'(LPST_USER),!,
  push_lpst_dialect(User, LPST_USER).

push_lpst_dialect(Was, M):-
   interpreter:check_lpst_program_module(M),
   current_input(In),
   style_check(-discontiguous), style_check(-singleton),
   push_operators([],Undo),
   %ignore(retract(tmp:module_dialect_lpst(In,_,_,_))),     
   asserta(tmp:module_dialect_lpst(In,Was,M,Undo)),!.


pop_lpst_dialect:-
    current_input(StreamIn),
    retract(tmp:module_dialect_lpst(StreamIn,Was,M,Undo)),
    pop_operators(Undo),!,
    lpst_debug(un_alt_module(Was,M)),
    nop('$set_source_module'(Was)),!.
pop_lpst_dialect.


user:goal_expansion(In, Out) :-
    prolog_load_context(dialect, lpst),
    lpst_gOAL_expansion(In, Out).

system:term_expansion(In, PosIn, Out, PosOut) :- In == end_of_file,
   prolog_load_context(dialect, lpst),
   current_input(StreamIn),
   tmp:module_dialect_lpst(StreamIn,_,_,_),
   pop_lpst_dialect,!,
   Out = In,
   PosIn = PosOut.
      
      



