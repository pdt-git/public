/*  $Id: main.pl,v 1.1 2005/05/19 08:16:35 gk Exp $

    Author:        Uwe Bardey
    E-mail:        UweBardey@gmx.de

    Modul des 'jTransformer' Transformations-Frameworks zur Analyse von
    Abhängigkeiten zwischen bedingten-Transformationen (CT's).

    Öffentliche Prädikate:
            depend/4,        % Basis Prädikat zur Analyse von Abhängigkeiten
            posDepend/3,     %    positive Abhängigkeit zwischen 2 CT's
            negDepend/3,     %    negative Abhängigkeit        "
            depend/2,        %    beliebige Abängigkeit        "
            circle/2,        %    zyklische Abhängigkeit zwischen N CT's
            conflict/2,      %       negative                "
            iteration/2,     %       positiv-monotone        "
            iteration_add/2,     %       positiv-monotone        "
            iteration_rem/2,     %       positiv-monotone        "

            dep_graph/1,     % Berechnung und Anzeige des Abhängigkeitsgraphen
            gen_dep_graph/1, %    Ausgabe auf Bildschirm und in Faktenbasis
            gen_dep_graph/2, %    Ausgabe in Datei und in Faktenbasis
            ct_node/1,       %       Knoten im Abhängigkeitsgraph
            ct_edge/4,       %       Kante im Abhängigkeitsgraph
            show_dep_graph/0,%       grafische Anzeige des Graphen in Faktenbasis
            del_dep_graph/0, % Löschen in Faktenbasis
            
            topo_sort/2      % Topologische Sortierung von N CT's
            postcond/3       % Berechnet die Nachbedingung einer CT
*/


:- multifile test/1.
:- multifile ct/3.
:- dynamic ct/3.

% Load the CONDOR code base

load_codebase(condor) :-
%  Zur Zeit nur von LogicAJ benötigt:
    consult('L:/Work/gk/gk/data/forschungEigene/code/workspace/JTransformer-NG/engine/depend/preprocess/main.pl'),
%  Wird vom Rest nicht gebraucht, zeigt nur Abhängigkeitsgraph graphisch an.
%  Basiert auf den von dep_graph.pl generierten Fakten:
    consult('L:/Work/gk/gk/data/forschungEigene/code/workspace/JTransformer-NG/engine/depend/preprocess/main.pl'),
%  Abhängigkeitsanalyse und Generierung von Fakten, die den Abhängigkeitsgraphen darstellen:
    consult('L:/Work/gk/gk/data/forschungEigene/code/workspace/JTransformer-NG/engine/depend/dep_graph'),
%  Darstellung des Abhängigkeitsgraphen:
    consult('L:/Work/gk/gk/data/forschungEigene/code/workspace/JTransformer-NG/engine/depend/dep_graph_gui'),
%  Called from dep_graph.pl::gen_order/2:
    consult('L:/Work/gk/gk/data/forschungEigene/code/workspace/JTransformer-NG/engine/depend/topo_sort'),    %  - topo_sort/2
    consult('L:/Work/gk/gk/data/forschungEigene/code/workspace/JTransformer-NG/engine/depend/graph_algos'),  %  - circle/2, conflict/2
%  Called from dep_graph.pl::gen_dep_graph:
    consult('L:/Work/gk/gk/data/forschungEigene/code/workspace/JTransformer-NG/engine/depend/depend'),       %  - Core of dependency analysis: posDepend, negDepend, ...
%  Called from depend.pl::depend/4:
    consult('L:/Work/gk/gk/data/forschungEigene/code/workspace/JTransformer-NG/engine/depend/expand'),       %  - expandAbstractionsAndDNF/6
%  Called from depend.pl::unification_restrictions/1:
    consult('L:/Work/gk/gk/data/forschungEigene/code/workspace/JTransformer-NG/engine/depend/patterns'),     %  - checkPattern(_plist1, _plist2), checkInequalities/1
%  Sprachunabhängige Graphdarstellung des AST auf der depend.pl aufbaut:
    consult('L:/Work/gk/gk/data/forschungEigene/code/workspace/JTransformer-NG/engine/ast/tomSyntax-oldRepresentation'),%  - ast_node/3, ast_edge/3, ast_attr/3 for Tom Mens
    consult('L:/Work/gk/gk/data/forschungEigene/code/workspace/JTransformer-NG/engine/depend/ast2graph'),    %  - ast_node/3, ast_edge/3, ast_attr/3
    true.


?- load_codebase(condor).

% file_search_path(condor, 'L:\Work\gk\gk\data\forschungEigene\code\workspace\JTransformer-NG\engine\depend'),consult(condor(depend)).

/*
% Nur in Standalone-Fassung der Originalversion:
%:- ['importFromJTransformer/general_rules.pl'].

%  Zur Zeit nur von LogicAJ benötigt:
:- ['preprocess/main'].

%  Wird vom Rest nicht gebraucht, zeigt nur Abhängigkeitsgraph graphisch an.
%  Basiert auf den von dep_graph.pl generierten Fakten:
:- [dep_graph_gui].

%  Abhängigkeitsanalyse und Generierung von Fakten, die den
%  Abhängigkeitsgraphen darstellen:
:- [dep_graph].
%  Called from dep_graph.pl::gen_order/2:
:- [topo_sort].    %  - topo_sort/2
:- [graph_algos].  %  - circle/2, conflict/2
%  Called from dep_graph.pl::gen_dep_graph:
:- [depend].       %  - Core of dependency analysis: posDepend, negDepend, ...
%  Called from depend.pl::depend/4:
:- [expand].       %  - expandAbstractionsAndDNF/6
%  Called from depend.pl::unification_restrictions/1:
:- [patterns].   %  - checkPattern(_plist1, _plist2), checkInequalities/1
%  Sprachunabhängige Graphdarstellung des AST auf der depend.pl aufbaut:
:- [ast2graph].    %  - ast_node/3, ast_edge/3, ast_attr/3

% Load some example CTs
%:- ['U:/gk/rootsExt/condor/dependencyAnalysis/cts/paper_cts.pl'].

% Run an example and display the dependency graph
%:- gen_dep_graph([aiset_low, acset_low, icounter_low, ccounter_low]).
%, show_dep_graph.

*/
