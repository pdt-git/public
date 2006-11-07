:- multifile abba:node/3.
:- dynamic abba:node/3.

:- dynamic abba:property/2.
:- multifile abba:property/2.

:- dynamic abba:edge/4.
:- multifile abba:edge/4.

/**
 * Specification of the LogicAJ abba nodes.
 *
 */
abba:nodetype(laj_aspect).
/**
 * Generic advice or generic introduction.
 *
 *  e.g.: before(..) : ....  {} 
 */
abba:nodetype(laj_effect).
/**
 * Show error or warning construct.
 *
 *  e.g.: declare warning ....
 */
abba:nodetype(laj_analysis).

/**
 * Pointcut designator or literal
 * in pointcut definition.
 *
 * e.g.: ... call(* *.*(..)) ...
 */
abba:nodetype(laj_pcd).

/**
 * Pointcut declaration.
 *
 * e.g.: pointcut namedpc(..) : ...
 */
abba:nodetype(laj_pc_dec).

/**
 * Aspect constructs body element.
 *
 * e.g. ... m(); ...
 */
abba:nodetype(laj_body).



%abba:edgetype(parent, laj_effect, laj_aspect).
%abba:edgetype(parent, laj_analysis, laj_aspect).
%abba:edgetype(parent, laj_pcd, laj_effect).
%abba:edgetype(parent, laj_pcd, laj_analysis).
%abba:edgetype(parent, laj_pcd, laj_pc_dec).
%abba:edgetype(parent, laj_body, laj_effect).
%abba:edgetype(parent, laj_pc_dec, laj_aspect).

abba:propertytype(laj_effect, kind(atom)).
abba:propertytype(laj_analysis, kind(atom)).

abba:property(laj_effect, propertytype(position(int, int))).
abba:property(laj_analysis, propertytype(position(int, int))).
abba:property(laj_pcd, propertytype(position(int, int))).
abba:property(laj_pc_dec, propertytype(position(int, int))).
abba:property(laj_aspect, propertytype(position(int, int))).

abba:property(laj_aspect, propertytype(file(string))).

delete_logicaj_abba_model :-
  forall((
      abba:node(ID, Kind, Name), 
      logicaj_abba_node_type(Kind)
    ),(
      retract(abba:node(ID, Kind, Name)),
      retractall(abba:property(ID, _)),
      retractall(abba:within(ID, _)),
      retractall(abba:ri_within(ID, _)),
      retractall(abba:edge(_,_,ID, _)),
      retractall(abba:edge(_,_,_, ID))
    )).
      
logicaj_abba_node_type(laj_effect).
logicaj_abba_node_type(laj_analysis).
logicaj_abba_node_type(laj_pcd).
logicaj_abba_node_type(laj_pc_dec).
logicaj_abba_node_type(laj_body).
logicaj_abba_node_type(laj_aspect).

