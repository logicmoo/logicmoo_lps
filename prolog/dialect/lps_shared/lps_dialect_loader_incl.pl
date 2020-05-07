:- use_module(library(dialect/lps_shared/lps_dialect_loader_lib)).

:- multifile(setup_dialect/0).
:- module_transparent(setup_dialect/0).
setup_dialect:- 
  current_prolog_flag(emulated_dialect,FromM),
  prolog_load_context(module, IntoM),
  IntoM:import(lps_dialect_loader_lib:dialect_on_load/3), 
  get_prolog_load_context(Ctx),  
  asserta(lps_dialect_loader_lib:setup_dialect_load_context(Ctx)),
  %wdmsg(setup_dialect_load_context(Ctx)),
  IntoM:use_module(library(dialect/FromM)),
  (FromM\=IntoM -> FromM:import(lps_dialect_loader_lib:dialect_on_load/3); true),
  asserta(IntoM:term_expansion(T,FP,O,FPO):- FromM:lps_dialect_term_expansion(T,FP,O,FPO)).

:- multifile(lps_dialect_term_expansion/4).
:- export(lps_dialect_term_expansion/4).
lps_dialect_term_expansion(T,FP,O,FPO):- 
  notrace((
      nonvar(FP), 
     (prolog_load_context(dialect,D);current_prolog_flag(dialect,D)) ->      
        prolog:dialect_reads(D,P1))),
  get_prolog_load_context(DC),
  FPO=FP, 
  O = (:- dialect_on_load(P1,T,DC)),
  !.




%:- use_module(lps_dialect_loader_lib).
:- multifile(prolog:dialect_reads/2).
:- dynamic(prolog:dialect_reads/2).


