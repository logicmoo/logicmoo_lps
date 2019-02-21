% Things that need to be defined in the user module, so Swish finds them
% To start clout as daemon:
% sudo swipl user_module_file.pl ../../swish/daemon.pl --port=80 --no-fork --user=www-data --workers=16
% To start as local server:
% swipl -l user_module_file.pl -l ../../swish/server.pl -g server:server

:- multifile sandbox:safe_primitive/1.
 
% For debugging:
% sandbox:safe_primitive(swish_highlight:server_tokens(_)).  % swish_highlight:server_tokens(source).
% sandbox:safe_primitive(swish_highlight:show_mirror(_)).
% can not print output as usual, would interfere with http responses; uncomment the following for a log:

/*
:- open('mylog.txt',write,S), assert(mylogFile(S)).
mylog(M) :- mylogFile(S), thread_self(T), writeln(S,T:M), flush_output(S).
% :- asserta((prolog:message(A,B,C) :-  mylog(message-A), fail)).
sandbox:safe_primitive(user:mylog(_M)). 
*/
:- use_module(library(settings)).
%:- use_module(library(http/http_log)). % uncomment to produce httpd.log
%:- set_setting_default(http:logfile, 'data/httpd.log'). % swish's writable sub directory

:- multifile swish_config:config/2.
swish_config:config(show_beware,true).
swish_config:config(community_examples,false).
% don't see the point: swish_config:config(public_access,true). % HACK here
swish_config:config(chat,false).
% Forcing SWISH to accept both of the following as legitimate; 
% only the first will get to the Javascript side, I think... but no harm done (apparently):
swish_config:config(include_alias,	example).
swish_config:config(include_alias,	system).
% the above facts must come before this...:
:- use_module('../../swish/swish').

:- use_module(swish(lib/render)).
:- use_module(library(http/http_dispatch)).
:- use_module(swish(lib/plugin/login)).
:- use_module(swish(lib/authenticate)).
:- use_module(library(settings)).

% LPS visualizations will appear courtesy of either of two SWISH answer renderers:
:- use_module(lps_2d_renderer,[]). % need not and can not import the rendering predicate into here
:- use_module(lps_timeline_renderer,[]).
:- multifile 'swish renderer'/2. % to avoid SWISH warnings in other files
:- use_rendering(lps_2d). % this will be the preferred... if available for the current visualization
:- use_rendering(lps_timeline).

:- multifile pengines:prepare_module/3.
pengines:prepare_module(_Module, swish, _Options) :- 
	style_check(-discontiguous), style_check(-singleton).

% If you consider refactoring this out to somewhere else: somehow these must be after use_module('../../swish/swish'):
:- use_module('../utils/visualizer.P'). % this loads LPS
:- use_module('../utils/psyntax.P',[
	syntax2p/4,dumploaded/1,term_colours/2,may_clear_hints/0,timeless_ref/1,set_top_term/1
	]).
% This will be useful below, as file searching handling of relative paths differs from what's used
% by use_module and friends.
user:file_search_path(lps_engine_dir,D) :- interpreter:lps_engine_directory(D).

sandbox:safe_primitive(interpreter:go(_File,Options)) :- \+ member(cycle_hook(_,_,_),Options).
sandbox:safe_primitive(interpreter:go). 
sandbox:safe_primitive(interpreter:lps_welcome_message). 
sandbox:safe_primitive(visualizer:gojson(_JSON)). 
sandbox:safe_primitive(visualizer:gojson(_File,_Options,_Results,_JSON)). 
sandbox:safe_primitive(psyntax:dumploaded(_)). 

/*
The following could be used to prevent pengines (remote goal) access... but bear in mind that swish (user) browsers communicate directly
to the server, so their IPs would have to be allowed. I guess full authentication is needed to prevent remote pengines usage.
:- initialization(( gethostname(H), tcp_host_to_address(H,ip(A,B,C,D)), format(atom(IP),'~w.~w.~w.~w',[A,B,C,D]), set_setting(pengines:allow_from, ['127.0.0.1',IP]))) .
*/

% We'll fill this information at the beginning of each web request; can't use a thread_local fact because 
% SWISH uses more than one thread handling the HTTP request; so we just store it in user, the SWISH transient module
:- dynamic transaction_lps_user/2. % User unique id, e.g. Google's sub; and email

% Access the user authenticated in the current web request
lps_user(User) :- lps_user(User,_).

lps_user(User,Email) :- transaction_lps_user(User,Email), !.
lps_user(unknown_user,unknown_email).

% hack SWISH's http authentication hook in lib/authenticate.pl to maintain the above:
:- dynamic(pengines:authentication_hook/3). % Needed for SWI Prolog 8.x
:- asserta((pengines:authentication_hook(Request, _Application, User) :- !,
    authenticate(Request, User), update_user(Request,User))).
%TODO: try instead http_current_request(Request) ??

update_user(Request,_User) :- 
	retractall(transaction_lps_user(_,_)), % hacky retract, good for all clauses...
	catch( (current_user_info(Request, Info), assert(transaction_lps_user(Info.sub,Info.email))), _Ex, fail), 
	!.
% the above clause may be dumb (or not...) because perhaps the following suffices... TODO: clean up this.
update_user(_Request,User) :- 
		catch(user_property(User,email(Email)),_,fail),
		!,
		assert(transaction_lps_user(User.identity,Email)).   % local (e.g. HTTP-authenticated) account
update_user(_Request,_User) :- 
	assert(transaction_lps_user(unknown_user,unknown_email)).

% patch SWISH so that "local" (HTTP authenticated users) are kept sandboxed:
:- dynamic(swish_pep:approve/2). % Needed for SWI Prolog 8.x. 
:- asserta((
	swish_pep:approve(run(any, _), Auth) :- user_property(Auth, login(local)), !, fail
	)).


:- multifile prolog_colour:term_colours/2, prolog_colour:goal_colours/2.
% Wire our colouring logic into SWI's:
prolog_colour:term_colours(T,C) :- term_colours(T,C).

:- multifile swish_highlight:style/3.
% style(Spec_as_in_specified_item, Type_as_in_prolog_server.js/prolog.css, ? )
swish_highlight:style(lps_delimiter,lps_delimiter,[text,base(atom)]).
swish_highlight:style(fluent,fluent,[text,base(atom)]).
swish_highlight:style(event,event,[text,base(atom)]).
swish_highlight:style(time,time,[text,base(atom)]). % atom?

% :- use_module('../engine/db.P',[head_hint/3]).

% HACK colouring of Prolog clause heads when they're referred only by LPS clauses
:- dynamic(swish_highlight:style/3). % Needed in SWI Prolog 8.1.1... who knows for how long this will be admissible ;-)
:- asserta((swish_highlight:style(head(unreferenced, Head), Type, Attributes) :-
	nonvar(Head),
	functor(Head,F,N), 
	timeless_ref(F/N),
	% TODO: somehow this is not working, may be a thread context issue; 
	% would be nice to color external fluent and event definitions properly:
	% functor(HH,F,N), db:head_hint(HH,Type,_),
	!, % Head is referred as timeless, so it's not really unreferenced:
	swish_highlight:style(head(head, Head), Type, Attributes) )).

% hack to make the SWISH editor not consider this as undefined.
% future LPS system predicates depending on the execution state probably will need to be added here
real_time_beginning(B) :- interpreter:real_time_beginning(B).

/* This might work for XPCE... different style attributes
% used Mac Digital Color Meter to pick visjs timeline colours:
prolog_colour:style(fluent,[colour('#1A1A1A'), background('#D5DD28')]). 
prolog_colour:style(event,[colour('#FDA428'), background('#FFFFFF')]). 
prolog_colour:style(time,S) :- prolog_colour:style(event,S).
prolog_colour:style(lps_delimiter,[bold(true)]) :- mylog(lps_delimiter). */

dump :- psyntax:dumploaded(false).
dumplps :- psyntax:dumploaded(true).
goclassic :- interpreter:lps_welcome_message, writeln('Using main interpreter:'),interpreter:go.
goclassicv :- interpreter:lps_welcome_message, writeln('Using main interpreter:'),interpreter:go(_,[swish,verbose]).
goclassic(Timeline) :- visualizer:gojson(_File,[silent],[],Timeline).
go(T,Options) :- \+ member(cycle_hook(_,_,_),Options), \+ member(background(_),Options), 
	(catch(lps_server_UI:lps_user_is_super,_,fail) -> true ; \+ member(timeout(_),Options)), 
		% TODO: refactor lps_user_is_super into this file?
	interpreter:lps_welcome_message, visualizer:gojson(_File,[dc,silent|Options],[],T).
godc(T) :- visualizer:gojson(_File,[dc,silent],[],T).
go(T) :- godc(T).
go :- interpreter:lps_welcome_message, writeln('Using dc:'),interpreter:go(_,[swish,dc]).
gov :- interpreter:lps_welcome_message, writeln('Using dc:'),interpreter:go(_,[swish,verbose,dc]).
	
:- multifile user:file_search_path/2.
user:file_search_path(profile, lps_engine_dir('../swish/profiles')).
user:file_search_path(lps_resources, lps_engine_dir('../swish/web')).

% PATCH to swish to avoid duplicate example and help menu and profile entries on Linux
% list_without_duplicates(+L,-NL) 
% Remove duplicates from list L, but keeping first occurrences in their original order
list_without_duplicates([X|L],[X|NL]) :- select(X,L,LL), !, list_without_duplicates(LL,NL).
list_without_duplicates([X|L],[X|NL]) :- !, list_without_duplicates(L,NL).
list_without_duplicates([],[]).
:- dynamic(swish_help:help_files/1). % Needed in SWI Prolog 8.1.1... who knows for how long this will be admissible ;-)
:- asserta((
swish_help:help_files(AllExamples) :-
	findall(Index,
		absolute_file_name(swish_help(.), Index,
				   [ access(read),
				     file_type(directory),
				     file_errors(fail),
				     solutions(all)
				   ]),
		ExDirs_), 
	list_without_duplicates(ExDirs_,ExDirs), % patch
	maplist(swish_help:index_json, ExDirs, JSON),
	append(JSON, AllExamples),
	!
)).
:- dynamic(swish_examples:swish_examples_no_cache/1). % Needed in SWI Prolog 8.1.1... who knows for how long this will be admissible ;-)
:- asserta((
	swish_examples:swish_examples_no_cache(SWISHExamples) :-
		http_absolute_location(swish(example), HREF, []),
		findall(Index,
			absolute_file_name(example(.), Index,
					   [ access(read),
						 file_type(directory),
						 file_errors(fail),
						 solutions(all)
					   ]),
			ExDirs_),
		list_without_duplicates(ExDirs_,ExDirs), % patch..
		maplist(swish_examples:index_json(HREF), ExDirs, SWISHExamples)
)).
	
/* Somehow this is NOT working:
:- dynamic(swish_config:config/2). % Needed in SWI Prolog 8.1.1... who knows for how long this will be admissible ;-)
:- asserta((
swish_config:config(What, Profiles) :-
	What==profiles, !,  % hack to allow swish_config_dict/2 to... not lose config items;-)
	findall(Profile, swish_profiles:swish_profile(Profile), Profiles0_),
	list_without_duplicates(Profiles0_,Profiles0), % patch..
	sort(value, =<, Profiles0, Profiles1),
	swish_profiles:join_profiles(Profiles1, Profiles)
)).
:- asserta((
swish_config:config(What, A) :- 
	What==include_alias, !, % hack to allow swish_config_dict/2 to... not lose config items;-)
	once((A=example;A=system))
)).
*/


:- http_handler('/lps', serve_lps_resources, [prefix]). 
serve_lps_resources(Request) :- % http://localhost:3050/lps/foo/Gruntfile.js working :-)
		option(path(Info), Request),  
        http_reply_file(lps_resources(Info), [], Request).

% hack SWISH to inject our CSS and Google Analytics fragment...
:- use_module(swish(lib/page)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_path)).

:- catch(read_file_to_string(googleAnalyticsKey,Key,[]),_,Key=''), 
	format(atom(JS),'
  (function(i,s,o,g,r,a,m){i[\'GoogleAnalyticsObject\']=r;i[r]=i[r]||function(){
  (i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),
  m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)
  })(window,document,\'script\',\'https://www.google-analytics.com/analytics.js\',\'ga\');

  ga(\'create\', \'~w\', \'auto\');
  ga(\'send\', \'pageview\');
',[Key]), assert(google_analytics_script(JS)).

:- multifile swish_config:reply_page/1. % redefine SWISH's page maker:
swish_config:reply_page(Options) :- 
	reply_html_page(
	    swish(main),
	    \(swish_page:swish_title(Options)),
	    \my_swish_page(Options)).

my_swish_page(Options) -->
	my_swish_navbar(Options),
	swish_content(Options). % no need to inject resources here again... is there??

my_swish_navbar(Options) -->
	my_swish_resources, % this may have to move to after the next call's inhards...
	swish_navbar(Options).
	
my_swish_resources -->
	{google_analytics_script(JS)},
	% swish_page:swish_css, swish_page:swish_js, 
	% {http_absolute_location(lps_resources('lps.css'),LPScss,[])},
	html_post(head, link([ type('text/css'),rel('stylesheet'),href('/lps/lps.css') ])),
	html_post(head, script(JS)).
	
sandbox:safe_primitive(interpreter:lps_ask(_A,_B,_C)). 
sandbox:safe_primitive(interpreter:lps_ask(_A,_B)). 
sandbox:safe_primitive(interpreter:lps_outcome(_A,_B)). 


lps_ask(A,B,C) :- interpreter:lps_ask(A,B,C).
lps_ask(A,B) :- interpreter:lps_ask(A,B).
lps_outcome(A,B) :- interpreter:lps_outcome(A,B).