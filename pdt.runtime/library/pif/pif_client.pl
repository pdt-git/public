:- module(pif_client,[
	pif_create/1,
	pif_start/1,
	pif_stop/1,
	pif_status/2,
	pif_set_option/3,
	pif_current_option/3/*,
	pif_session_create/2,
	pif_session_dispose/1,
	pif_session_query/2,
	pif_session_query_once/2,
	pif_session_query_all/2*/
]).

:-dynamic pif/1.

% pif_create(-Handle).
%
% create a new, unconnected pif and unify a handle to it with Handle.
pif_create(Handle):-
  flag(pif_client_next_pif_id,Id,Id+1),
  atom_concat('$pif_',Id,Handle),
  assert(pif(Handle)).


pif_option_default(pif_port,4711).
pif_option_default(pif_host,localhost).
pif_option_default(pif_standalone,true).

pif_option(Option):-
    pif_option_default(Option,_).

% pif_start(-Handle).
%
% start a pif, i.e. create a process running a pif server.

pif_start(_Handle). %TODO Not implemented    

pif_stop(_Handle). %TODO Not implemented

pif_status(_Handle,_Status). %TODO Not implemented

:-dynamic pif_option_value/3.

pif_set_option(Handle,Option,Value):-
    nonvar(Handle),
    nonvar(Option),
    pif(Handle),
    pif_option(Option),
    retractall(pif_option_value(Handle,Option,_)),
    assert(pif_option_value(Handle,Option,Value)).
    
pif_current_option(Handle,Option,Value):-
	pif(Handle),
    pif_option(Option),
    current_option(Handle,Option,Value).

current_option(Handle,Option,Value):-
	pif_option_value(Handle,Option,Value),
	!.
current_option(Handle,Option,Value):-
	pif_option_default(Handle,Option,Value).
    
pif_session_create/2,
pif_session_dispose/1,
pif_session_query/2,
pif_session_query_once/2,
pif_session_query_all/2.