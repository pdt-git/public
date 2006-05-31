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

:-module(export_annotator,[]).

:- use_module(library('/org/cs3/pdt/util/pdt_util_aterm')).
file_pre_annotation_hook(_,_,[H|_],In,[defines_module(Module), exports(SortedExports)|In]):-
    pdt_strip_annotation(H,':-'(module(Module,Exports)),_),    
    sort(Exports,SortedExports).
    
term_pre_annotation_hook(_,OpModule,InTerm,OutTerm):-
    %% check if term is a module declaration
    pdt_strip_annotation(InTerm,':-'(module(Module,Exports)),_),    
	%% check the exports list 
	check_exports(InTerm,OutTerm).

    
check_exports(In,Out):-
	pdt_subterm(In,[1,2],Exports),
	%% find ill-formed elements in the exports list
	findall(ill_formed(Path,Export),pdt_outer_match(Exports,Path,Export,\+ well_formed_export(Export)),IllFormedExports),
	add_ill_formed_annos(In,IllFormedExports,Out).

add_ill_formed_annos(In,[],In).
add_ill_formed_annos(In,[ill_formed(Path,InExport)|IFEs],Out):-
    pdt_term_annotation(InExport,Term,Annotation),
    pdt_term_annotation(OutExport,Term,[problem(error,malformed_export)|Annotation]),
    pdt_subst(In,Path,OutExport,Next),
    add_ill_formed_annos(Next,IFEs,Out).
    
well_formed_export(Name/Arity):-
    atom(Name),
    integer(Arity).