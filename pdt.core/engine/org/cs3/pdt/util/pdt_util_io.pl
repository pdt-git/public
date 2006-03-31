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

:- module(pdt_util_io,[
	copy_file_to_memfile/2,
	copy_memfile_to_file/2,
	pretty_print/3,
	pretty_print/2,
	pretty_print/1
]).

:- use_module(library('org/cs3/pdt/util/pdt_util')).


/**
copy_file_to_memfile(+File, +MemFile)

Copies the contents of File to the memory file MemFile.

File - a file spec. (path or aliased path)
MemFile - a handle to a Memfile. 

*/
copy_file_to_memfile(File,MemFile):-
    pdt_file_spec(File,Abs),
    open_memory_file(MemFile,write,MemStream),    
    open(Abs,read,Stream),
    copy_stream_data(Stream,MemStream),
    close(Stream),
    close(MemStream).
    
    
/**
copy_memfile_to_file(+MemFile, +File)

Copies the contents of File to the memory file MemFile.

File - a file spec. (path or aliased path)
MemFile - a handle to a Memfile. 

*/
copy_memfile_to_file(MemFile,File):-
    pdt_file_spec(File,Abs),
    open_memory_file(MemFile,read,MemStream),    
    open(Abs,write,Stream),
    copy_stream_data(MemStream,Stream),
    close(Stream),
    close(MemStream).    
    

pretty_print(Term):-
    pretty_print(current_output,'',Term).
    
pretty_print(Stream,Term):-
    pretty_print(Stream,'',Term).

pretty_print(Stream,Indent,[H|T]):-
    !,
	format(Stream,"~w[~n",[Indent]),
    atom_concat(Indent,'   ',IIndent),
    pretty_print_args(Stream,IIndent,[H|T]),
	format(Stream,"~w]",[Indent]).    

pretty_print(Stream,Indent,Module:Name/Arity):-
    atom(Module),
    atom(Name),
    number(Arity),
    !,
    format(Stream, "~w~w:~w/~w",[Indent,Module,Name,Arity]).

pretty_print(Stream,Indent,Name/Arity):-
    atom(Name),
    number(Arity),
    !,
    format(Stream, "~w~w/~w",[Indent,Name,Arity]).

	
pretty_print(Stream,Indent,aterm(A,T)):-
    compound(T),
    !,
    T=..[Functor|Args],
    format(Stream, "~waterm(~w,'~w'(~n",[Indent,A,Functor]),
    atom_concat(Indent,'   ',IIndent),
    pretty_print_args(Stream,IIndent,Args),
    format(Stream,"~w))",[Indent]).        
pretty_print(Stream,Indent,aterm(A,T)):-
    \+ compound(T),
    !,
    format(Stream, "~waterm(~w,~w)",[Indent,A,T]).

pretty_print(Stream,Indent,T):-
    compound(T),
    !,
    T=..[Functor|Args],
    format(Stream, "~w'~w'(~n",[Indent,Functor]),
    atom_concat(Indent,'   ',IIndent),
    pretty_print_args(Stream,IIndent,Args),
    format(Stream,"~w)",[Indent]).        
pretty_print(Stream,Indent,T):-
    \+ compound(T),
    !,
    format(Stream, "~w~w",[Indent,T]).

pretty_print_args(_,_,[]):-!.
pretty_print_args(Stream,Indent,[H|[]]):-
    !,
	pretty_print(Stream,Indent,H),
	nl(Stream).
pretty_print_args(Stream,Indent,[H|T]):-
    !,
	pretty_print(Stream,Indent,H),
	write(Stream,','),
	nl(Stream),
	pretty_print_args(Stream,Indent,T).