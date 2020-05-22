:- module(lps, []).

calc_dialect_module(M):- 
     '$current_typein_module'(TM), 
     prolog_load_context(module,Load),strip_module(_,Strip,_),
     context_module(Ctx),'$current_source_module'(SM),
     ((TM\==Load,TM\==user) -> M = TM ; (M = SM)),
     once(true;writeln([ti=TM,load=Load,strip=Strip,ctx=Ctx,sm=SM,lps=M])).     


   :- volatile(tmp:module_dialect_lps/2).
:- thread_local(tmp:module_dialect_lps/2).

:- system:module_transparent(lps:setup_dialect/0).

setup_dialect :-
     calc_dialect_module(M),
     current_input(In),
     ignore(retract(tmp:module_dialect_lps(In,_))),
     asserta(tmp:module_dialect_lps(In,M)),
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
     op(700,xfx,((M:(<=)))).
   %use_module(library('../engine/interpreter.P')),
   %use_module(library('../swish/term_expander.pl')),!.
   

