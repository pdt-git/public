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
 * TODO
 */

:- module(magicmap,
	   [ /*position/2,*/
	   position_attr/3]).

/** 
 * position(MAC, gps(Lat,Long,Alt))
 *
 * Lat:   -90 ...  90
 * Long: -180 ... 180
 * Alt:  ???  
 */
:- multifile position/2.
%:- dynamic position/2.

%position(MAC, gps(Lat,Long,Alt)) :-
%   location(MAC,Lat,Long,Alt).

/** 
 * TODO: Documentation
 */
:- dynamic position_attr/3.
:- multifile position_attr/3.

/** 
 * location(MAC,Lat,Long,Alt)
 *
 * FOR INTERNAL USE ONLY (data from MagicMap Server)
 */
:- dynamic location/4.
:- multifile location/4.

% Uncomment the following for a dummy entry representing the Dell X50v in the B-it building:
%location('00-09-2D-53-27-3A', 50.73, 7.12, 0).


/****************** INTERNAL ***********************/

earthRadius(6371).% km
/**
 * point_of_interest(MAC , cordinates(Latitude,Longitude,Altitude)) 
 *
 * The MAC address (if associated with an AP or mobile device)
 * or another unique identification (certificate?).
 */
:- dynamic point_of_interest/3. 

/**
 * distance(+gps(Long1,Lat1,Alt1),+gps(Long2,Lat2,Alt2),-Km)
 */

distance(gps(Long1,Lat1,_Alt1), 
         gps(Long2,Lat2,_Alt2), Km) :-
		earthRadius(Radius),

		angle_to_rad(Long1, LongRad1),
		angle_to_rad(Lat1, LatRad1),
		angle_to_rad(Long2, LongRad2),
		angle_to_rad(Lat2, LatRad2),
		
		DLong is LongRad2 - LongRad1,
		DLat is LatRad2 - LatRad1,

		A is  (sin(DLat / 2)) * (sin(DLat / 2))			+ (cos(LatRad1) * cos(LatRad2) * (sin(DLong / 2)))				* (cos(LatRad1) * cos(LatRad2) * (sin(DLong / 2))),

		C is 2* asin(min(1.0, sqrt(A))),
		Km is Radius * C.

%		double a = (Math.sin(dlat / 2)) * (Math.sin(dlat / 2))
%				+ (Math.cos(lat1) * Math.cos(lat2) * (Math.sin(dlon / 2)))
%				* (Math.cos(lat1) * Math.cos(lat2) * (Math.sin(dlon / 2)));
%		double c = 2 * Math.asin(Math.min(1.0, Math.sqrt(a)));
%		double km = earthRadius * c,
%		return (float) (km * 1000); 	

angle_to_rad(Angle,Rad):-
   Rad is (Angle / 360) * 2 * pi.
   

   
