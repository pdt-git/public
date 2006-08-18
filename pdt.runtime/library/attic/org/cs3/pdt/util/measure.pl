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

:- module(measure,[
    start/0,
    stop/1]).
    
:- dynamic lastTime/3.

/**
  start
 
  Starts the time measuring.
*/

start :-
    get_time(_t),
    convert_time(_t, _, _, _, _, _min, _sec, _msec),
    assert(lastTime(_min,_sec,_msec)).


/**
 stop(-Msg)
 
Stops the time measuring and binds
the resulting time difference to Msg.
*/
stop(_msg) :-
    get_time(_t),
    convert_time(_t, _, _, _, _, _min, _sec, _msec),
    lastTime(_min0,_sec0,_msec0),
    retractall(lastTime(_,_,_)),
    get_diff(0         , _msec0, _msec, _div_msec, _carryMsec, 1000),
    get_diff(_carryMsec, _sec0 , _sec , _div_sec ,_carrySec  , 60),
    get_diff(_carrySec , _min0 , _min , _div_min ,_, 0),
    writef('%w: %w min %w.%2r sec', [_msg, _div_min, _div_sec, _div_msec]).

get_diff(_carry, _start, _end, _Out,_carryOut, _add) :-
    plus(_end, _carry, _endPlus),
    plus(_start, _diff, _endPlus),
    ifNegAdd(_diff, _Out, _carryOut, _add).
    
/*
 ifNegAdd(+Val1, +Val2, -IsNeg, -Add)
 */
 
ifNegAdd(_val, _val, 0, _) :-
    _val >= 0.
ifNegAdd(_val, _Out, 1, _add) :-
    plus(_val, _add, _Out).
    