:- module(abba, [
    edge/5,
    node/3,
    property/2]).
    
:- multifile edge/5.
:- dynamic edge/5.

:- multifile node/3.
:- dynamic node/3.

:- multifile property/2.
:- dynamic property/2.
