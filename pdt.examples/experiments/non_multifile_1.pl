:- module(a1,[aaa/1]).

aaa(1).                  % This definition is found by 
                         %   visible_in(Module, Name, Arity) and
                         %   visible_in(Module, Name, Arity, File, Line)
                         % in addition to the independent definition in module a2.

:- multifile bbb/1.

bbb(1).