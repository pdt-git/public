

 :- consult(logging).       % <- THIS BE FIRST!
  
 :- consult(compatiblitySWI).
  
 :- consult(database).      % Assert, retract, ...
 :- consult(files).         % File handling
 :- consult(database_cache). 	    % Caching
 :- consult(contains). 	    % Contains for Strings

 :- consult(general).       % Various
 :- consult(listing).       % Print clauses of preds
 :- consult(lists).         % List handling
 :- consult(count).         % Counting
 :- consult(time).          % Runtime measurement
 :- consult(utils4modules). % Module handling
 
% :- consult(pdt_xref_experimental).     % find_references, ...