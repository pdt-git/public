:- module(abba, [
    edge/5,
    node/3,
    property/2,
    propertytype/2,
    nodetype/2]).
    
:- multifile edge/5.
:- dynamic edge/5.

:- multifile within/2.
:- dynamic within/2.

:- multifile ri_within/2.
:- dynamic ri_within/2.

:- multifile node/3.
:- dynamic node/3.

:- multifile property/2.
:- dynamic property/2.

:- multifile abba:propertytype/2.
:- dynamic abba:propertytype/2.

:- multifile abba:nodetype/2.
:- dynamic abba:nodetype/2.

