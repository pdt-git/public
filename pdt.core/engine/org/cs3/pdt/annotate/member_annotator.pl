%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This file is part of the Prolog Development Tool (PDT)
%  
% Author: Lukas Degener (among others) 
% E-mail: degenerl@cs.uni-bonn.de
% WWW: http://roots.iai.uni-bonn.de/research/pdt 
% Copyright (C): 2004-2006, CS Dept. III, University of Bonn
% 
% All rights reserved. This program is  made available under the terms 
% of the Eclipse Public License v1.0 which accompanies this distribution, 
% and is available at http://www.eclipse.org/legal/epl-v10.html
% 
% In addition, you may at your option use, modify and redistribute any
% part of this program under the terms of the GNU Lesser General Public
% License (LGPL), version 2.1 or, at your option, any later version of the
% same license, as long as
% 
% 1) The program part in question does not depend, either directly or
%   indirectly, on parts of the Eclipse framework and
%   
% 2) the program part in question does not include files that contain or
%   are derived from third-party work and are therefor covered by special
%   license agreements.
%   
% You should have received a copy of the GNU Lesser General Public License
% along with this program; if not, write to the Free Software Foundation,
% Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
%   
% ad 1: A program part is said to "depend, either directly or indirectly,
%   on parts of the Eclipse framework", if it cannot be compiled or cannot
%   be run without the help or presence of some part of the Eclipse
%   framework. All java classes in packages containing the "pdt" package
%   fragment in their name fall into this category.
%   
% ad 2: "Third-party code" means any code that was originaly written as
%   part of a project other than the PDT. Files that contain or are based on
%   such code contain a notice telling you so, and telling you the
%   particular conditions under which they may be used, modified and/or
%   distributed.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:-module(member_annotator,[]).

:- use_module(library('/org/cs3/pdt/annotate/pdt_annotator')).
:- use_module(library('/org/cs3/pdt/util/pdt_util_aterm')).
:- use_module(library('/org/cs3/pdt/util/pdt_util_rbtree')).
:- use_module(library('/org/cs3/pdt/util/pdt_util')).

:- pdt_annotator([term,file],[library('/org/cs3/pdt/annotate/export_annotator')]).

term_annotation_hook(_,_,FileAnos,InTerm,OutTerm):-
    (	module_definition(FileAnos,FileModule,_)
    ->	true
    ;	FileModule=user
    ),
	process_member(InTerm,FileModule,OutTerm).


process_member(InTerm,FileModule,OutTerm):-
    pdt_term_annotation(InTerm,Term,Annos),
    pdt_strip_annotation(InTerm,Stripped,_),
    has_head(Stripped,FileModule, Module, Functor, Arity),
    Functor\=(':-'),
    pdt_term_annotation(OutTerm,Term,[clause_of(Module:Functor/Arity)|Annos]).
%process_member(':-'(Term),FileModule,Annotation):-
%    property_definition(Term,Property,Signatures),
%    module_qualified_signatures(FileModule,Signatures,ModuleQualifiedSignatures),
%    Annotation=..[Property,ModuleQualifiedSignatures].

process_member(InTerm,FileModule,OutTerm):-
    pdt_term_annotation(InTerm,':-'(InBody),DirectiveAnnos),
    pdt_term_annotation(InBody,BodyTerm0,BodyAnnos),
    BodyTerm0=..[Functor,InSigs],
    property_functor(Property,Functor),
    check_signatures(InSigs,Signatures,OutSigs),
    BodyTerm1=..[Functor,OutSigs],
	module_qualified_signatures(FileModule,Signatures,ModuleQualifiedSignatures),    
    Annotation=..[Property,ModuleQualifiedSignatures],
    pdt_term_annotation(OutBody,BodyTerm1,BodyAnnos),
    pdt_term_annotation(OutTerm,':-'(OutBody),[Annotation|DirectiveAnnos]).


property_functor(defines_dynamic,dynamic).
property_functor(defines_multifile,multifile).
property_functor(defines_module_transparent,transparent).

check_signatures(InSigs,Signatures,OutSigs):-
	findall(
		ill_formed(Path,ASignature),
		(	pdt_operand((,)/2,InSigs,Path,ASignature),
			pdt_strip_annotation(ASignature,Signature,_),
			\+ well_formed_signature(Signature)
		),
		IllFormedSignatures
	),
	findall(
		Signature,
		(	pdt_operand((,)/2,InSigs,Path,ASignature),
			pdt_strip_annotation(ASignature,Signature,_),
			well_formed_signature(Signature)
		),
		Signatures
	),
	add_ill_formed_annos(InSigs,IllFormedSignatures,OutSigs).

add_ill_formed_annos(In,[],In).
add_ill_formed_annos(In,[ill_formed(Path,InExport)|IFEs],Out):-
    pdt_term_annotation(InExport,Term,Annotation),
    pdt_term_annotation(OutExport,Term,[problem(error(malformed_signature))|Annotation]),
    pdt_subst(In,Path,OutExport,Next),
    add_ill_formed_annos(Next,IFEs,Out).

	
well_formed_signature(Name/Arity):-
    atom(Name),
    integer(Arity).	
well_formed_signature(Module:Name/Arity):-
    atom(Module),
    atom(Name),
    integer(Arity).	
    
    
module_qualified_signatures(_,[],[]).
module_qualified_signatures(FileModule,[InH|InT],[OutH|OutT]):-
    module_qualified_signature(FileModule,InH,OutH),
    module_qualified_signatures(FileModule,InT,OutT).

module_qualified_signature(FileModule,Name/Arity,FileModule:Name/Arity).
module_qualified_signature(_,Module:Name/Arity,Module:Name/Arity).
module_qualified_signature(_,(Module:Name)/Arity,Module:Name/Arity).
    


file_annotation_hook(_,_,Terms,InAnnos,[defines(Definitions)|OutAnnos]):-
    collect_definitions(Terms,Definitions),
    findall(Property,property_functor(Property,_),Properties),
    collect_properties(Properties,Terms,InAnnos,OutAnnos).

collect_properties([],_,In,In).
collect_properties([Property|Properties],Terms,In,[PropTerm|PropTerms]):-
    collect_property(Property,Terms,[],Sigs),
    PropTerm=..[Property,Sigs],
    collect_properties(Properties,Terms,In,PropTerms).
    
collect_property(_,[],Sigs,Sigs).
collect_property(Property,[Term|Terms],InSigs,OutSigs):-    
    pdt_top_annotation(Term,TopAn),
    collect_property_X(Property,TopAn,MySigs),
    merge_set(InSigs,MySigs,NextSigs),  
    collect_property(Property,Terms,NextSigs,OutSigs).

collect_property_X(Property,TopAn,OutSigs):-
    PropTerm=..[Property,Sigs],
    	pdt_member(PropTerm,TopAn),
    	!,
    sort(Sigs,OutSigs).
collect_property_X(_,_,[]).

collect_definitions(Terms,Definitions):-
    pdt_rbtree_new(Tree0),
    collect_definitions(Terms,Tree0,Tree),
    findall(A,pdt_rbtree_next('',A,_,Tree),Definitions0),
    pdt_remove_duplicates(Definitions0,Definitions).
    

collect_definitions([],Tree,Tree).   
collect_definitions([Term|Terms],InTree,OutTree):-
    pdt_top_annotation(Term,TopAn),
    pdt_member(clause_of(Definition),TopAn),
    !,
    pdt_rbtree_insert(InTree,Definition,Definition,NextTree),
	collect_definitions(Terms,NextTree,OutTree).
collect_definitions([_|Terms],InTree,OutTree):-
	collect_definitions(Terms,InTree,OutTree).

    
module_definition(FileAnos,Module,Exports):-
	pdt_member(defines_module(Module),FileAnos),
	pdt_member(exports(Exports),FileAnos).
	
	
has_head(':-'(Module:HeadTerm,_),_,Module,Functor,Arity):-
    !,
    functor(HeadTerm,Functor,Arity).
    
has_head(':'(Module,':-'(HeadTerm,_)),_,Module,Functor,Arity):-
    !,
    functor(HeadTerm,Functor,Arity).
has_head(':-'(HeadTerm,_),FileModule,FileModule,Functor,Arity):-
    !,
    functor(HeadTerm,Functor,Arity).
has_head(':'(Module,HeadTerm),_,Module,Functor,Arity):-
    !,   
    functor(HeadTerm,Functor,Arity).    
has_head(HeadTerm,FileModule,FileModule,Functor,Arity):-
    !,
    functor(HeadTerm,Functor,Arity).    
    