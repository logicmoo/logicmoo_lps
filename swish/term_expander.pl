
:- module(lps_term_expander, [lps_term_expander/3]).

:- module_transparent(lps_term_expander/3).

:- use_module(library('../utils/psyntax.P'),[ 
	lps2p_file/2, lps2p/3, syntax2p_file/4, syntax2p/4, syntax2p_literal/7, golps/2, golps/1, dumpjs/2, dumpjs/1,
	file_generator_name/2, may_clear_hints/0,term_colours/2,timeless_ref/1, set_top_term/1, dumploaded/2
	]).
	
% on SWISH we'll avoid the file to file translation, by converting on a term by term basis, assuming the transform to be 1-1 (except for nlp)
% we assume the LPS transform to preserve Prolog 
lps_f_term_expansion(_Module,NiceTerm,'$source_location'(File, Line):ExpandedTerms) :- 
	% somehow the source location is not being kept, causing later failure of clause_info/5 :-(	
	prolog_load_context(source,File), 
        % atom_prefix(File,'pengine://'), % process only SWISH windows
	prolog_load_context(term_position,TP), stream_position_data(line_position,TP,Line),
	catch(call(call,lps_nlp_translate(NiceTerm,ExpandedTerms)),_,fail), !. % hook for LogicalContracts extension
lps_f_term_expansion(_Module,NiceTerm,ExpandedTerm) :- 
	may_clear_hints, set_top_term(NiceTerm),
	% current_syntax(lps2p,true), In the future we may want to support other syntax conversions
	% variable names probably not available here, but we don't care about lpsp2p syntax anymore:
	% somehow this fails to... some terms;-) prolog_load_context(file,File), mylog(normal-File),
	syntax2p(NiceTerm,[],lps2p,ExpandedTerm). 

   :- volatile(tmp:module_dialect_lps/4).
:- thread_local(tmp:module_dialect_lps/4).

lps_term_expander(Module,NiceTerm,ExpandedTerm):- 
  (current_prolog_flag(emulated_dialect,lps) ,
    (current_input(In),tmp:module_dialect_lps(In,_,Module,_Undos))), 
  % context_module(user), % LPS programs are in the user module
  lps_f_term_expansion(Module,NiceTerm,ExpandedTerm),!,
  maybe_inform(Module,NiceTerm,ExpandedTerm).
  
maybe_inform(_Module,NiceTerm,ExpandedTerm):-   
  ignore((fail,(NiceTerm\=@=ExpandedTerm,flush_output(user_error),
          format(user_error,'~N~p.~n',[NiceTerm-->ExpandedTerm]),
          flush_output(user_error)))).

:- module_transparent(user:term_expansion/2).
system:term_expansion(NiceTerm,ExpandedTerm):- 
  current_prolog_flag(emulated_dialect,lps),
  % compound(NiceTerm), NiceTerm \= (_:_), 
  % context_module(Module),
  prolog_load_context(module,Module),
  lps_term_expander(Module,NiceTerm,ExpandedTerm).
