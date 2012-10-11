:- module(outline_demo_multifile_contribution, []).

% Multifile contribution for entity outline_demo.
% 
% The outline presents this information with the arrow on the module icon.
:- multifile(outline_demo:likes/2).

outline_demo:likes(jack_torrance, the_overlook_hotel).

own_predicate.