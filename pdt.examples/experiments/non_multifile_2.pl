:- module(a2,[]).

:- dynamic blubber/0.    % This declaration cannot be located by SWI-Prolog

aaa(2).                  % This definition is found by 
                         %   visible_in_module(Module, Name, Arity) and
                         %   visible_in_module(Module, Name, Arity, File, Line)
                         % in addition to the independent definition in module a1.

a:- goal_expansion(_G8464, _G8465).

:- multifile a1:bbb/1.

:- multifile a1:ccc.


a1:bbb(2). 