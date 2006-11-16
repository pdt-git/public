/**
 * PDA MAC: 00-09-2D-53-27-3A
 */
:- module(localisation, 
       [company_nearby/3,
       company_nearby/4]).

depends(location/4, company_nearby/3).
depends(location/4, company_nearby/4).

:- use_module(magicmap).

%------ BIT: --------
%position('StandIBM', gps(50.72031111111, 7.120469444444, 0)).
%IBM: vor Eingang von 1.32
%
%position('StandPhilips', gps(50.720494444, 7.120583333333, 0)).
%Philips: vor Tür zum Treppenhaus
%
% position('StandSamsung', gps(50.7204027777777, 7.12040277777, 0)).
%Samsung: an der Balkontür in der Küche
%--------------------

%altbau: 50.757  7.0769
% distance(gps(50.7511158333, 7.0971722, 0),gps(50.757,  7.0769,0),2,34445).


% IBM: Ende zum Neubau
position('StandIBM', gps(50.7511158333, 7.0971722, 0)).
position_attr('StandIBM', 'name', 'IBM').

% Philips: Mitte
position('StandPhilips', gps(50.751410, 7.097026, 0)).
position_attr('StandPhilips', 'name', 'Philips').

% Samsung: Aussenende Altbau
position('StandSamsung', gps(50.751622, 7.0969027, 0)).
position_attr('StandSamsung', 'name', 'Samsung').



company_nearby_tmp(MyMAC, Company, Distance) :-
%  position(MyMAC, Gpscurr),
  location(MyMAC,Lat,Long,Alt),
  position(MAC, Gpscomp),
  distance(gps(Lat,Long,Alt), Gpscomp, Distance),
  position_attr(MAC, 'name', Company).

company_nearby(MAC,Company, Distance) :-
  setof([Distance, Company], company_nearby_tmp(MAC, Company, Distance), Cs),
  member([Distance, Company], Cs).

company_nearby(MAC, Company, Distance, MaxDistance) :-
  setof([Distance, Company], company_nearby_tmp(MAC, Company, Distance), Cs),
  member([Distance, Company], Cs),
  Distance < MaxDistance.
  
  
%  compare_max_distance_ensure_binding(Distance, MaxDistance).
%  
% compare_max_distance_ensure_binding(Distance, MaxDistance) :-
%  nonvar(MaxDistance),
%  !,
%  Distance < MaxDistance.
%  
% compare_max_distance_ensure_binding(Distance, -1).