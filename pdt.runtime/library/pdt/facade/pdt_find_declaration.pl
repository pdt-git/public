:- module(pdt_find_declaration,
	[	pdt_resolve_predicate/5,
		pdt_predicate_contribution/4
	]).


:-use_module(library('pef/pef_base')).
:-use_module(library('pef/pef_api')).



%%
% pdt_resolve_predicate(+CxFile,+CxModule,+Name,+Arity,-PredId).
% resolve a predicate reference in a given context.
%
% This predicate is intended to be used by the "Find Declaration" action of the 
% PDT's Editor.
%
% @param CxFile absolute path to the refering file.
% @param CxModule name of the context module from within which the predicate is referenced.
%		 If CxModule is a variable, the module "user" is assumed for non-module files. Otherwise 
%		 the module defined in the CxFile is assumed. 	
% @param Name The functor name of the referenced predicate.
% @param Arity The functor arity of the referenced predicate.
% @param PredId will be unified with the PEF Identifier of the resolved predicate.
pdt_resolve_predicate(CxFile,CxModule,Name,Arity,PredId):-
    

%%     
% pdt_predicate_contribution(+PredId,-File,-Start,-End).
% find source code contributing to a given predicate.
%
% As "contribution" we consider clauses and predicate property definitions.
%
% This predicate is intended to be used by the "Find Declaration" action of the 
% PDT's Editor.
%
% @param PredId the PEF identifier of the predicate.
% @param File will be unified with the absolute path of a file contributing to the predicate.
% @param Start character start offset of the contribution within the file
% @param End character end offset of the contribution within the file  
pdt_predicate_contribution(PredId,File,Start,End).	