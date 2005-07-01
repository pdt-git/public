:- module( ct_seq_export_to_XML, [export_to_XML_document/2] ).

/*
 * TODO: Extend this to cover also export of pure conditions to
 * condition documents (<CDocument> ... </CDocument>).
 */

/* --------- Syntax of CTs and CT Sequences 

(New) Language definition for CTs and CT sequences with external 
identities for terms, and the distinction of OR sequences and AND 
sequences as required for ConTraCT:

   APPLYCT_QUERY   = ct_apply( CT ).
   APPLYSEQ_QUERY  = ct_sequence_apply( SEQUENCE ).
   COMPOSE_QUERY   = ct_sequence_compose( SEQUENCE ).

   SEQUENCE = ORSEQ
            | ANDSEQ

   ORSEQ    = orSequence(EXTID, HEAD, [ELEMOR, ELEMOR, ...])
            | true % orSequence(EXTID, HEAD, []) <-- möglich aber sinnlos

   ANDSEQ   = andSequence(EXTID, HEAD, [CT, CT, ...])
            | true % andSequence(EXTID, HEAD, []) <-- möglich aber sinnlos

   ELEMOR   = ANDSEQ
            | CT

   CT       = ct(EXTID, HEAD, COND, TRANS_TOP)
            | true

   CONDLIST = [COND, COND, ...]
            | [] % ConTraCT setzt in diesem Fall ‘true’ als Bedingung.

   COND     = and(EXTID,CONDLIST)
            |  or(EXTID,CONDLIST)
            | not(EXTID,CONDLIST)
            | SIMPLE

   SIMPLE   = exists(EXTID,PEF)
            | true
            | false
            | new_id(VAR)
            | ... ? ...
   
   TRANS_TOP= transformation(EXTID,TRANSLIST)
            | true

   TRANSLIST= [TRANS, TRANS, ...]
            | []

   TRANS    = add(EXTID, PEF)
            | delete(EXTID, PEF)
            | replace(EXTID, PEF, PEF)

   PEF      = A term such that
               ( ast_node_term(Language, PEF)  % see ast/languageIndependentSyntax.pl
               ; cond( PEF )                   % see api/high_level_api.pl
               ; action( PEF )                 % see api/high_level_api.pl
               )
             is true.
*/

 /******************************************************************
  * Next group of predicates: Templates for generating XML output.
  * 
  * By adding a parameter to each of the following predicates, 
  * we can generate any kind of output syntax without modifying
  * the "visitor" / "interpreter" predicate export_to_XML_document/2.
  * ****************************************************************      
  */
xml_template_ctseq_document_(CTseq, Params, XML) :-
   atom_concat('<CTSDocument>', CTseq, '<ParameterList>', Params, '</ParameterList></CTSDocument>',
               XML).
                
xml_template_ct_document_(CT, Params, XML) :-
   atom_concat('<CTDocument>', CT, '<ParameterList>', Params, '</ParameterList></CTDocument>',
               XML).
                
xml_empty_and_seq_('<CTSequence type="ConditionalTransformationAndSequence"> </CTSequence>').

xml_empty_or_seq_('<CTSequence type="ConditionalTransformationOrSequence"> </CTSequence>').

xml_template_ctseq_(Kind, EXTID, XMLBody, XML) :-
   xml_type(Kind,Type),
   atom_concat('<CTSequence type="', Type, '" treeId="', EXTID, '" >', XMLBody, '</CTSequence>',
               XML).  
                              
xml_type(and,'ConditionalTransformationAndSequence').
xml_type(or,'ConditionalTransformationOrSequence').

xml_template_ct_(EXTID, XMLCond, XMLTrans, XML) :-
   atom_concat('<CT treeId="', EXTID, '" >', XMLCond, XMLTrans, '</CT>',
               XML).                 
   
xml_template_param_deflist_(XMLparamDefs,XMLparamDefList) :-
   atom_concat('<ParameterList>', XMLparamDefs, '</ParameterList>',
               XMLparamDefList ).

xml_template_param_def_(Var,XMLparamDef) :-
    ( ground(Var), Val=Var    % constant is its own dummy varName
    ; var(Var), Val=null      % set dummy value for free var
    ),
    atom_concat( '<Parameter><name>', Var, '</name><value>', Val, '</value></Parameter>',
                 XMLparamDef ).

xml_template_param_use_(Var,XMLparamUse) :-
    atom_concat( '<Parameter>', Var, '</Parameter>', XMLparamUse ).
               
        
xml_template_condition_exists_(EXTID, XMLName, XMLParam, XMLCondition) :-
   atom_concat( '<Condition type="',XMLName,'" treeId="',EXTID,'" >', XMLParam, '</Condition>',
                 XMLCondition ).
    
xml_template_condition_not_(EXTID, XMLCond, XMLNot) :-   
   atom_concat( '<Condition type="NotCondition" treeId="',EXTID,'" >', XMLCond, '</Condition>',
                 XMLNot ).

xml_template_condition_or_(EXTID, XMLConds, XMLOr) :- 
   atom_concat( '<Condition type="OrCondition" treeId="',EXTID,'" >',  XMLConds, '</Condition>',
                 XMLOr ).

xml_template_condition_or_(EXTID, XMLConds, XMLAnd) :- 
   atom_concat( '<Condition type="AndCondition" treeId="',EXTID,'" >', XMLConds, '</Condition>',
                 XMLAnd ). 

xml_template_transformation_(EXTID, XMLName, XMLParams, XMLTrans) :-
   atom_concat( '<Transformation type="XMLName" treeId="',EXTID,'" >', XMLParams, '</Transformation>',
                 XMLTrans ).     
                            
 /******************************************************************
  * Below this line: Export CTs and CT sequences that are 
  * on the top-level, that is, correspond to an XML-document.
  * ****************************************************************      
  */
  
 /**
  * export_to_XML_document(+Term, ?XMLAtom) 
  *   Arg1 is an or-sequence, and-sequence or 'true'. The latter stands for 
  *   any empty sequence. Arg2 is the XML representation of a Document 
  *   describing Arg1. 
  */     
                      % check expected input mode
export_to_XML_document( SEQUENCE, _VeryBigResultAtomInXMLSyntax ) :-
    var(SEQUENCE),
    throw( ct_export_with_unbound_sequence(SEQUENCE) ).

                      % empty sequence (pretends to be an or sequence)
export_to_XML_document( true, XML ) :- !,
   xml_empty_or_seq_(EmptySeq),
   xml_template_ctseq_document_(EmptySeq,'', XML).
                
                      % empty or sequence
export_to_XML_document( orSequence(_,_,[]), XML ) :- !,
   xml_empty_or_seq_(EmptySeq),
   xml_template_ctseq_document_(EmptySeq,'', XML).
                            
                      % empty and sequence
export_to_XML_document( andSequence(_,_,[]), XML ) :- !,
   xml_empty_and_seq_(EmptySeq),
   xml_template_ctseq_document_(EmptySeq,'', XML).
                
                      % non-empty or sequence
export_to_XML_document( orSequence(EXTID,HEAD,ELEMS), XML ) :- !,
   export_or_sequence_( orSequence(EXTID,HEAD,ELEMS), XMLseq),
   HEAD =.. [Functor|Params],
   export_document_param_deflist_(Params, XMLparams), 
   xml_template_ctseq_document_(XMLseq, XMLparams, XML).
   
                      % non-empty and sequence
export_to_XML_document( andSequence(EXTID,HEAD,ELEMS), XML ) :- !,
   export_and_sequence_( andSequence(EXTID,HEAD,ELEMS), XMLseq),
   HEAD =.. [Functor|Params],
   export_document_param_deflist_(Params, XMLparams), 
   xml_template_ctseq_document_(XMLseq, XMLparams, XML).

                      % a single ct
export_to_XML_document( ct(EXTID,HEAD,COND,TRANS), XML ) :- !, 
   export_ct_( ct(EXTID,HEAD,COND,TRANS), XMLct ),  
   HEAD =.. [Functor|Params],
   export_document_param_deflist_(Params, XMLparams), 
   xml_template_ct_document_(XMLct, XMLparams, XML).

 /******************************************************************
  * Below this line: Export CTs and CT sequences that are not 
  * on the top-level, that is, do not correspond to an XML-document.
  * ****************************************************************
  */

 /**
  * export_and_sequence_(+Term, ?XML)
  * export_or_sequence_(+Term, ?XML)
  * export_ct_(+Term, ?XML)
  *   Arg1 is an or-sequence, and-sequence or CT. 
  *   Arg2 is the respective external representation. 
  */
export_or_sequence_( orSequence(EXTID,HEAD,ELEMS), XML) :-   
   export_seq_elems_(ELEMS, XMLBody),
   xml_template_ctseq_(or, EXTID, XMLBody, XML).

export_and_sequence_( andSequence(EXTID,HEAD,ELEMS), XML) :-
   export_seq_elems_(ELEMS, XMLBody),
   xml_template_ctseq_(and, EXTID, XMLBody, XML).

export_ct_( ct(EXTID,HEAD,COND,TRANS), XML ) :-
   export_cond_or_trans_(COND, XMLCond),
   export_trans_toplevel_(TRANS, XMLTrans),
   xml_template_ct_(EXTID, XMLCond, XMLTrans, XML).


 /**
  *   export_document_param_deflist_(+Params, ?XMLparams)
  *      Arg1 is the parameter list of a Condition, CT or CT sequence. 
  *      Arg2 contains the XML representation of each 
  *       of Arg1's elements, in exactly the same order.  
  */  
export_document_param_deflist_(Params, XMLparamDefList) :-  
   export_document_param_defs_(Params, XMLparamDefs),
   xml_template_param_deflist_(XMLparamDefs,XMLparamDefList).


export_document_param_defs_([], []).
export_document_param_defs_([H|T], [XMLh|XMLt]) :-
    xml_template_param_def_(H,XMLh),
    export_document_param_defs_(T, XMLt).

 /**
  *   export_param_uses_(+Params, ?XMLparams)
  *      Arg1 is the parameter list of a condition or transformation. 
  *      Arg2 contains the XML representation of each 
  *      of Arg1's elements, in exactly the same order.  
  */  
export_param_uses_([], []).
export_param_uses_([H,T], [XMLh|HMLt]) :-
    xml_template_param_use_(H,XMLh),
    export_param_uses_(T,HMLt).


 /**
  *   export_seq_elems_(+NonemptyList, ?XMLatom)
  *      Arg1 is a nonempty list of CTs or and-sequences.
  *      Arg2 is its XML representation (order preserving).
  */
export_seq_elems_(List,  XML) :-
    export_seq_elems_2list_(List,  XMLlist),
    atom_list_concat(XMLlist,XML),
    !.

 /**
  *   export_seq_elems_2list_(+NonemptyTermList, ?XMLatomList)
  *      Arg1 is a nonempty list of CTs or and-sequences.
  *      Arg2 is a list of XML representations of the individual
  *      elements (order preserving).
  */
export_seq_elems_2list_([Only], Result) :- %   Nonempty Sequence
    export_andsequence_or_ct_(Only, Result). 
export_seq_elems_2list_([First|Rest], [XMLfirst|XMLrest] ) :-  
    export_andsequence_or_ct_(First, XMLfirst),
    export_seq_elems_2list_(Rest, XMLrest).
 
    
export_andsequence_or_ct_(Term, Atom) :-
   export_ct_(Term,Atom).
export_andsequence_or_ct_(Term, Atom) :-
   export_and_sequence_(Term,Atom).


 /******************************************************************
  * Below this line: Export conditions and transformations.
  * ****************************************************************
  */
  
 /** 
  * export_trans_toplevel_(transformation(EXTID,TransList), XML)
  */
   % The Identity of a transformation sequence is not represented in the
   % current XML-format of ConTraCT. So it is ignored in the export!
export_trans_toplevel_(transformation(EXTID,TransList), XML) :-
   export_ct_elems_(TransList,XML).
export_trans_toplevel_(true, '') .


 /**
  *   export_ct_elems_(+List, ?XMLatom)
  *      Arg1 is a list of conditions or transformations.
  *      Arg2 is its XML representation (order preserving).
  */
export_ct_elems_(List,  XML) :-
    export_ct_elems_2list_(List, XMLlist),
    atom_list_concat(XMLlist,XML),
    !.

 /**
  *   export_ct_elems_2list_(+TermList, ?XMLatomList)
  *      Arg1 is a list of conditions or transformations.
  *      Arg2 is a list of XML representations of the individual
  *      elements (order preserving).
  */
export_ct_elems_2list_([], []) .   
export_ct_elems_2list_([H|T], [XMLh|XMLt]) :-  
    export_cond_or_trans_(H, XMLh),
    export_ct_elems_2list_(T, XMLt).


 /**
  *   export_cond_or_trans_(+Term, ?XMLatom)
  *      Arg1 is a single condition or transformation. 
  *      Arg2 is its XML representation.
  */        
export_cond_or_trans_( and(EXTID,CONDLIST), XMLAnd ) :-
   export_ct_elems_(CONDLIST, XMLConds),
   xml_template_condition_and_(EXTID, XMLConds, XMLAnd). 

export_cond_or_trans_( or(EXTID,CONDLIST), XMLOr ) :-
   export_ct_elems_(CONDLIST, XMLConds),
   xml_template_condition_or_(EXTID, XMLConds, XMLOr). 

export_cond_or_trans_( not(EXTID,COND), XMLNot ) :-
   export_cond_or_trans_(COND, XMLCond),
   xml_template_condition_not_(EXTID, XMLCond, XMLNot).         

export_cond_or_trans_( exists(EXTID,PEF), XMLCondition ) :- 
   PEF =.. [Funktor|Params],
   export_params_(Params, XMLParams), 
   export_name_condition_(Funktor,XMLName),
   xml_template_condition_exists_(EXTID, XMLName, XMLParams, XMLCondition).

export_cond_or_trans_( add(EXTID,PEF), XMLTrans ) :-
   PEF =.. [Funktor|Params],
   export_params_(Params, XMLParams), 
   export_name_transformation_add_(Funktor,XMLName),
   xml_template_transformation_(EXTID, XMLName, XMLParams, XMLTrans).
                                  
   
export_cond_or_trans_( true,  '<Condition type="True"></Condition>' ) :- !.

export_cond_or_trans_( false, '<Condition type="False"></Condition>' ) :- !.

export_cond_or_trans_( new_id(_), '' ) :- !.  
        
                                 
 /**
  *   export_conditionName_(+F,?XMLName)
  *      Arg2 is the value of the "type" attribute in a <Condition> tag.
  *      It represents the PEF with functor arg1.
  *      TODO: Check whether the implemented translation is correct!!!
  *            (I just guessed it!) 
  */   
export_name_condition_(F,XMLName):-
    atom_concat(NodeType,'T',F),
    atom_concat(exists,NodeType,XMLName).

export_name_trans_replace_(F1,F2,XMLName):-
    atom_concat(NodeType1,'T',F1),
    atom_concat(NodeType2,'T',F2),
    atom_concat(replace,NodeType1,by,NodeType2,XMLName).
    
export_name_trans_add_(F,XMLName):-
    atom_concat(NodeType,'T',F),
    atom_concat(add,NodeType,XMLName).

export_name_trans_del_(F,XMLName):-
    atom_concat(NodeType,'T',F),
    atom_concat(delete,NodeType,XMLName).

/*
More detailed definition of export_condition_name_ contained in "translation.pl":
:- [translation].
*/
