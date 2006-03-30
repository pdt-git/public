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