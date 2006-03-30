/**
 * TODO
 */

:- module(magicmap,
	   [ position/2,
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
   

   
