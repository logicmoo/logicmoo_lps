:- module(lps, []).


:- thread_local(tmp:m_dialect/1).
:- system:module_transparent(lps:setup_dialect/0).
setup_dialect :- 
     '$current_typein_module'(M1), 
     prolog_load_context(module,M2),strip_module(_,M3,_),
     context_module(M4),'$current_source_module'(M5),
     ((M1\==M2,M1\==user) -> M = M1 ; (M = M5)),
     once(true;writeln([ti=M1,load=M2,strip=M3,ctx=M4,sm=M5,lps=M])),     
     asserta(tmp:m_dialect(M)),
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
   

