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

/**
 * PDA MAC: 00-09-2D-53-27-3A
 */
:- module(localisation, 
       [company_nearby/3,
       company_nearby/4]).

sync:depends(magicmap:location/4, localisation:company_nearby/3).
sync:depends(magicmap:location/4, localisation:company_nearby/4).

:- use_module(magicmap).

%------ BIT: --------
%magicmap:position('StandIBM', gps(50.72031111111, 7.120469444444, 0)).
%IBM: vor Eingang von 1.32
%
%magicmap:position('StandPhilips', gps(50.720494444, 7.120583333333, 0)).
%Philips: vor Tür zum Treppenhaus
%
% magicmap:position('StandSamsung', gps(50.7204027777777, 7.12040277777, 0)).
%Samsung: an der Balkontür in der Küche
%--------------------

%altbau: 50.757  7.0769
% magicmap:distance(gps(50.7511158333, 7.0971722, 0),gps(50.757,  7.0769,0),2,34445).


% IBM: Ende zum Neubau
magicmap:position('StandIBM', gps(50.7511158333, 7.0971722, 0)).
magicmap:position_attr('StandIBM', 'name', 'IBM').

% Philips: Mitte
magicmap:position('StandPhilips', gps(50.751410, 7.097026, 0)).
magicmap:position_attr('StandPhilips', 'name', 'Philips').

% Samsung: Aussenende Altbau
magicmap:position('StandSamsung', gps(50.751622, 7.0969027, 0)).
magicmap:position_attr('StandSamsung', 'name', 'Samsung').



company_nearby_tmp(MyMAC, Company, Distance) :-
%  magicmap:position(MyMAC, Gpscurr),
  magicmap:location(MyMAC,Lat,Long,Alt),
  magicmap:position(MAC, Gpscomp),
  magicmap:distance(gps(Lat,Long,Alt), Gpscomp, Distance),
  magicmap:position_attr(MAC, 'name', Company).

company_nearby(MAC,Company, Distance) :-
  setof([Distance, Company], company_nearby_tmp(MAC, Company, Distance), Cs),
  member([Distance, Company], Cs).

company_nearby(MAC, Company, Distance, MaxDistance) :-
  setof([Distance, Company], company_nearby_tmp(MAC, Company, Distance), Cs),
  member([Distance, Company], Cs),
  Distance < MaxDistance.