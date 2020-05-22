:- module(lps, [on_lps_read/1,on_lps_bof/2, on_lps_eof/1, 
      dialect_on_load/3,get_prolog_load_context/1]).
%
:- notrace(use_module(library(pengines),[pengine_self/1])).
:- notrace(use_module(library(logicmoo_common))).
:- notrace(use_module(library('/../utils/psyntax.P'),[
	syntax2p/4,dumploaded/2,term_colours/2,may_clear_hints/0,timeless_ref/1,set_top_term/1])).
:- notrace(use_module('../../utils/checker.P',[check_js_compliance/1])).

% swish:swish.

:- dynamic(lps_tmp:dialect_decl/1).

get_prolog_load_context([variable_names=Vs,module=M,file=F,line=L,dialect=D,source=S,emulated_dialect=ED,system_dialect=PD,reload=R]):- 
  ignore(source_location(F,L)),
  ignore(prolog_load_context(file,F)),
  ignore(prolog_load_context(variable_names,Vs)),
  ignore(prolog_load_context(module,M)),
  ignore(prolog_load_context(dialect,D)),
  ignore(current_prolog_flag(emulated_dialect,ED)),
  ignore(current_prolog_flag(dialect,PD)),
  ignore(prolog_load_context(source,S)),
  (prolog_load_context(reload,R)->true;R=false).

:- module_transparent(dialect_on_load/3).
:- meta_predicate(dialect_on_load(1,+,:)).
                                                 
:- discontiguous lps:'$exported_op'/3. 
:- module_transparent(on_lps_read/1).
on_lps_read(T):- notrace(T == end_of_file),!,on_lps_eof(on_lps_read).  % ,set_prolog_flag(dialect,swi). % , listing(example_query/1).
on_lps_read(:- T):- call(T),!.
on_lps_read(T):- on_lps_read1(T).

on_lps_read1(example_query(X)):- !,assertz(example_query(X)).
%on_lps_read1(T):- \+ predicate_property(lps:T, imported_from(_)), !, must(lps:T).
%on_lps_read1(random(X)):- !, random(X). 
on_lps_read1(T):- call(assert(T)).



:- if( \+ current_module(lps_corner)).
% :- notrace(reexport(library('../engine/interpreter.P'))).
% :- ensure_loaded(library('../engine/interpreter.P')).
% :- user:[library('../engine/interpreter.P')].
:- notrace(reexport(library(lps_corner))).
:- endif.



% :- multifile(setup_dialect/0).
:- module_transparent(setup_dialect/0).
:- module_transparent(setup_dialect_now/0).
setup_dialect:- notrace(setup_dialect_now)->true;(trace,setup_dialect_now).

setup_dialect_now :- 
   use_module(library(lps_syntax)),
   use_module(library('../engine/interpreter.P')),
   use_module(term_expander),
   dumpST.
   


setup_dialect_now:- prolog_load_context(module, User),User==user,
  get_lps_alt_user_module(LPS_USER),
  LPS_USER\==user,
  user:[library('../engine/interpreter.P')],
  '$set_source_module'(LPS_USER),!,setup_dialect.

setup_dialect_now:- 
  strip_module(_,FromM,_),
  prolog_load_context(module, IntoM),
  current_prolog_flag(dialect,FromD),
  current_prolog_flag(emulated_dialect,IntoD),  
  %asserta(IntoM:lps_test_options([dc,non_prospective])),
  %asserta(IntoM:option(dc)),
  %asserta(db:option(swish)),
  % IntoM:import(lps:dialect_on_load/3),
  (prolog_load_context(file, File);File=user_input),!,
  get_prolog_load_context(Ctx0),
  Ctx = [fromD=FromD,intoD=IntoD,fromM=FromM,intoM=IntoM,file=File|Ctx0],  
  (IntoM\==user->IntoM:use_module(library(dialect/IntoD));true),
  (IntoM\==user->notrace(IntoM:use_module(library(lps_syntax)));true),
  (IntoM\==user->notrace(IntoM:[library('../engine/interpreter.P')]);true),
  (FromM\==IntoM -> FromM:import(lps:dialect_on_load/3); true),
  setup_term_expansion(File,FromM,IntoM,Ctx).



as_listified(List,List):-is_list(List),!.
as_listified(Item,[Item]).

is_db_pred(asserta).
is_db_pred(assert).
is_db_pred(assertz).
is_db_pred(retract).
is_db_pred(retractall).

setup_term_expansion(File,FromM,IntoM,Ctx):- user_input == File,!,
     on_lps_bof(File,Ctx),
     asserta((IntoM:term_expansion(T,O):-  
              FromM:lps_dialect_term_expansion(T,_,O,_))),
     asserta((IntoM:goal_expansion(T,O):- compound(T),compound_name_arity(T,Assert,1),
        is_db_pred(Assert),
        T =.. [Assert,In],
        FromM:lps_dialect_term_expansion(In,_,Out,_),
        as_listified(Out,OutL),O=maplist(Assert,OutL))),
       '$current_typein_module'(M),
       add_import_module(M,system,start),
       ignore(delete_import_module(FromM,M)),
       add_import_module(M,system,start),
       add_import_module(lps,system,start),       
       add_import_module(M,FromM,start).

setup_term_expansion(File,FromM,IntoM,Ctx):- 
  on_lps_bof(File,Ctx),
  asserta((IntoM:term_expansion(T,FP,O,FPO):- 
    (notrace(prolog_load_context(file, File)), 
           FromM:lps_dialect_term_expansion(T,FP,O,FPO)))).
  

on_lps_bof(File,Ctx):- 
  asserta(lps_tmp:dialect_decl(Ctx)), writeln(on_lps_bof(File,Ctx)), !.

on_lps_eof(W):- writeln(on_lps_eof(W)), 
  ignore((current_prolog_flag(dialect,E),set_prolog_flag(emulated_dialect,E))),
  ignore((prolog_load_context(module, LPS_USER),on_lps_eom(LPS_USER))).

get_lps_alt_user_module(LPS_USER):- interpreter:lps_program_module(LPS_USER),!.
get_lps_alt_user_module(LPS_USER):- is_lps_alt_user_module(LPS_USER),!.
is_lps_alt_user_module(lps_user).
lps_user:foo_lps_user.

can_be_lps:- context_module(user),!.
can_be_lps:- context_module(lps),!.
can_be_lps:- get_lps_alt_user_module(LPS_USER),context_module(LPS_USER),!.
can_be_lps:- context_module(LPS_USER),!,is_lps_alt_user_module(LPS_USER),!.

on_lps_eom(LPS_USER):- is_lps_alt_user_module(LPS_USER),!, 
  ignore(delete_import_module(user,LPS_USER)), 
  ignore(delete_import_module(LPS_USER,user)),
  ignore(add_import_module(user,LPS_USER,start)),
  asserta(interpreter:lps_program_module(LPS_USER)),
  '$set_source_module'(user),
  listing(LPS_USER:_).

expand_lps_into_module(User:O,User:O):-!.
expand_lps_into_module(O,LPS_USER:O):- get_lps_alt_user_module(LPS_USER),!.

:- multifile(lps_dialect_term_expansion/4).
:- export(lps_dialect_term_expansion/4).
lps_dialect_term_expansion(O,FP,O,FPO):- O == end_of_file,ignore(on_lps_eof(lps_dialect_term_expansion)),!,FP=FPO.
lps_dialect_term_expansion(T,FP,'$source_location'(File, Line):UserO,FPO):- get_file_line(File,Line),alt_dialect_term_expand(T,O),!, O\==T,FPO=FP, expand_lps_into_module(O,UserO).
lps_dialect_term_expansion(T,FP,UserO,FPO):- alt_dialect_term_expand(T,O),!, O\==T,FPO=FP, expand_lps_into_module(O,UserO).
lps_dialect_term_expansion(T,FP,O,FPO):-
  notrace((
      nonvar(FP), 
     (prolog_load_context(dialect,D);current_prolog_flag(emulated_dialect,D)) ->      
        prolog:dialect_reads(D,P1))),
  get_prolog_load_context(DC),  
  make_dialect_on_load(P1,T,DC,O),
  !, O\==T,FPO=FP,writeln(O-->T).
make_dialect_on_load( _,T, _,O):- alt_dialect_term_expand(T,O),!.
make_dialect_on_load(P1,T,DC,O):- O = (:- dialect_on_load(P1,T,DC)).


get_file_line(File,Line):-
  prolog_load_context(source,File), prolog_load_context(term_position,TP), stream_position_data(line_position,TP,Line),!.
get_file_line(File,Line):- source_location(File,Line).

alt_dialect_term_expand(NiceTerm,ExpandedTerms) :- 
	% somehow the source location is not being kept, causing later failure of clause_info/5 :-(
	can_be_lps, % LPS programs are in the user module
	catch(lps_nlp_translate(NiceTerm,ExpandedTerms),_,fail), !. % hook for LogicalContracts extension
alt_dialect_term_expand(NiceTerm,ExpandedTerm) :-        
	can_be_lps, % LPS programs are in the user module
	may_clear_hints, set_top_term(NiceTerm),
	% current_syntax(lps2p,true), In the future we may want to support other syntax conversions
	% variable names probably not available here, but we don't care about lpsp2p syntax anymore:
	% somehow this fails to... some terms;-) prolog_load_context(file,File), mylog(normal-File),        
	syntax2p(NiceTerm,[],lps2p,ExpandedTerm), !. 
alt_dialect_term_expand(_,_):- fail.



dialect_on_load(P1,T,DC):-
 strip_module(DC,_,DC0),
 b_setval('$term',T),
 b_setval('$prolog_load_context',DC0),
 ignore((memberchk(variable_names=Vs,DC0),
         b_setval('$variable_names',Vs))),
 call(P1,T).   

:- multifile(prolog:dialect_reads/2).
:- dynamic(prolog:dialect_reads/2).

prolog:dialect_reads(lps, on_lps_read).



