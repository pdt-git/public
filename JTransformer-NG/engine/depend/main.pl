/*  $Id: main.pl,v 1.1 2004/09/11 13:20:14 rho Exp $

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

%:- ['../../ct/lowlevelCTs.pl'].

:- [ast2graph].
:- ['../api/general_rules.pl'].
:- [topo_sort].
:- [graph_algos].
:- [dep_graph].
% funktioniert NICHT mit plcon zusammen und damit nicht in A2Ptest!
%:- ['dep_graph_gui.pl'].
:- [ct_filter].
:- [depend].
:- ['abstraction.pl'].

%:- gen_dep_graph([aiset_low, acset_low, icounter_low, ccounter_low]), show_dep_graph.



