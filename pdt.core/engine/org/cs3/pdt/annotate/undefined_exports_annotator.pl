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

:-module(undefined_exports_annotator,[]).

:- use_module(library('/org/cs3/pdt/util/pdt_util')).
:- use_module(library('/org/cs3/pdt/util/pdt_util_aterm')).
:- use_module(library('/org/cs3/pdt/annotate/pdt_annotator')).
:- use_module(library('/org/cs3/pdt/util/pdt_util_aterm')).


:- pdt_annotator([term],[
	library('org/cs3/pdt/annotate/export_annotator'),
	library('org/cs3/pdt/annotate/member_annotator')
]).


term_annotation_hook(_,_,Annos,InTerm,OutTerm):-
    %% check if term is a module declaration
    pdt_strip_annotation(InTerm,':-'(module(_Module,_Exports)),_),    
    pdt_member(defines(StatDefs),Annos),
    pdt_member(defines_dynamic(DynDefs),Annos),
    append(StatDefs,DynDefs,Defs),
    pdt_member(defines_module(Module),Annos),
	%% check the exports list 
	check_exports(InTerm,Module,Defs,OutTerm).

    
check_exports(In,Module,Defs,Out):-
	pdt_subterm(In,[1,2],Exports),
	%% find ill-formed elements in the exports list
	findall(undefined(Path,AExport),
		(	pdt_aterm_member(Exports,Path,AExport),
			pdt_strip_annotation(AExport,Export,_),
			well_formed_export(Export),
			\+ memberchk(Module:Export,Defs)
		),
		UndefinedExports
	),
	add_undefined_annos(In,UndefinedExports,Out).

add_undefined_annos(In,[],In).
add_undefined_annos(In,[undefined(Path,InExport)|IFEs],Out):-
    pdt_term_annotation(InExport,Term,Annotation),
    pdt_term_annotation(OutExport,Term,[problem(error(undefined_export))|Annotation]),
    pdt_subst(In,[1,2|Path],OutExport,Next),
    add_undefined_annos(Next,IFEs,Out).

well_formed_export(Name/Arity):-
    atom(Name),
    integer(Arity).
    
