/*  $Id: dep_graph_gui.pl,v 1.1 2005/05/19 08:16:35 gk Exp $

    Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (C): 1985-2002, University of Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

/******

%:- module(show_dep_graph,
%          [ show_dep_graph/0
%          ]).

:- use_module(library(pce)).
%:- require([ forall/2
%           , free_variables/2
%           , random/3
%           , term_to_atom/2
%           , depend:ct_edge/4
%           ]).


show_dep_graph :-
        new(GV, show_dep_graph),
        send(GV, open).

:- pce_begin_class(show_dep_graph, frame).


initialise(GV) :->
        "Create graph-viewer"::
        send(GV, send_super, initialise, 'Graph Viewer'),
        send(GV, append, new(P, picture)),
        send(new(D, dialog), below, P),
        fill_dialog(D).

fill_dialog(D) :-
        new(Frame, D?frame),

        send(D, append, label(reporter)),

        send(D, append, button(generate, message(Frame, generate,
                                               D?generator_member?selection))),
        %topo_sort(X), term_to_atom(X,A),
        
        send(D, append, text_item(generator, 'no order',
                                  message(D?generate_member, execute)), right),

        send(D, append, button(quit, message(Frame, destroy))),
        send(D, append, button(clear, message(Frame, clear))),
        send(D, append, button(postscript, message(Frame, postscript))),
        send(D, append, button(layout, message(Frame, layout))),

        display_all_edges(D?frame).


clear(F) :->
        "Clear the diagram"::
        get(F, member, picture, P),
        send(P, clear).


layout(F) :->
        "Run graph layout"::
        get(F, member, picture, P),
        (   get(P?graphicals, head, Head)
        ->  send(Head, layout)
        ;   send(F, report, error, 'No graph to layout')
        ).


:- pce_autoload(finder, library(find_file)).
:- pce_global(@finder, new(finder)).

postscript(F) :->
        "Create PostScript in file"::
        get(@finder, file, @off, '.eps', FileName),
        get(F, member, picture, Pict),
        new(File, file(FileName)),
        send(File, open, write),
        send(File, append, Pict?postscript),
        send(File, close),
        send(File, done),
        send(F, report, status, 'Saved PostScript in %s', FileName).



display_all_edges(F) :-
          send(F, clear),
            forall(ct_edge(From,To,_Label,Type),
                   send(F, display_arc, From, To, Type)),%, Label, Type)),
            send(F, layout).

generate(F, _Generator:name) :->
        "Create graph using generator"::
        (   %term_to_atom(Term, Generator),
            %free_variables(Term, [From, To, _Label, Type])

%           call(Generator),
           %topo_sort(X), term_to_atom(X,A),
%           send(F, label, 'uwe'),

           %send(F?generate_member, label, 'uwe'), %text_item(generator, A, message(F?generate_member, execute)), right),


           display_all_edges(F)
        ;   send(F, report, error,
                 'Generator should be a Prolog goal with 4 variables')
        ).


:- pce_global(@ct_link, make_link_link_link).

make_link_link_link(L) :-
    new(L, link(link, link, new(_LINE, line(0,0,0,0,second)))).
%    send(L, texture, dotted),
%    send(LINE, texture, dotted).
%    new(L, link(link, link, arrow(0,0,closed,250))),


%    send(L, connection_class, send( <- name('uwe')).

%    new(D, device),
%    send(D, display, ).
    
%new(@bo, box(100,100))).

%    send(@ic, display, bitmap(’happy.bm’)),
%    send(@ic, display, text(’Happy’), point(0, 64)),
%    send(@p, display, @ic, point(250, 20)).

%    new(L, link(link, link, line(0,0,0,0,second))).%line(arrows := second))).


%:- pce_global(@graph_link, new(link(link, link, line(0,0,0,0,second)))).
%:- pce_global(@graph_link, new(link(link, link, line(0,0,0,0,second)))).
%link(in, out, line(arrows := second))). %make_graph_link).

%make_graph_link(L) :-
%    new(L, ).
%    new(L, link(link, link, line(0,0,0,0,second))).

display_arc(F, From:name, To:name, Type:name) :-> %, Label:name, Type:name
        "Display arc From -> To"::
        get(F, node, From, NF),
        get(F, node, To, TF),
        dotNegative(Type),
        send(NF, connect, TF, @ct_link),
        %format('Type : ~a ~a ~a ~n',[From,To,Type]),
        send(@ct_link, pen, 2).
%        send(@ct_link, display, new(T, text(uwe, center))).

%        send(@ct_link, texture, dotted).

dotNegative(negative) :- !, send(@ct_link, texture, dotted). %dotted
dotNegative(positive) :- !, send(@ct_link, texture, none).

node(F, Name:name, Node:graph_node) :<-
        "Get (create) node with specified name"::
        get(F, member, picture, Picture),
        (   get(Picture, member, Name, Node)
        ->  true
        ;   get(Picture, visible, area(X, Y, W, H)),
            MX is X + W,
            MY is Y + H,
            random(X, MX, NX),
            random(Y, MY, NY),
            send(Picture, display, new(Node, graph_node(Name)), point(NX, NY))
        ).


:- pce_end_class.


:- pce_begin_class(graph_node(name), device).

handle(w/2, 0, link, link).

initialise(Node, Name:name) :->
        "Create from name"::
        send(Node, send_super, initialise),
%        send(Node, display, circle(10), point(-2, -2)),
        send(Node, display, new(C, circle(30)), point(-15, -15)),
        
%        C->fill_pattern = orange,
        new(_CO, colour(ctcolour,50000,50000,65000,rgb)),

        send(C, fill_pattern, colour(ctcolour)),
        send(Node, display, new(T, text(Name, center))),
        send(T, center, point(0, 30)),
        send(Node, send_super, name, Name).

name(Node, Name:name) :->
        "Change name of a node"::
        get(Node, member, text, Text),
        send(Text, string, Name),
        send(Node, send_super, name, Name).

:- pce_global(@graph_node_recogniser, make_graph_node_recogniser).

make_graph_node_recogniser(R) :-
        new(R, move_gesture(left)),
        send(R, condition,
             ?(@event?position, distance, point(0,0)) < 5).


event(Node, Ev:event) :->
        "Make it movable"::
        (   send(@graph_node_recogniser, event, Ev)
        ->  true
        ;   send(Node, send_super, event, Ev)
        ).

:- pce_end_class.



test(a,b).
test(a,c).
test(a,d).
test(c,d).
******/