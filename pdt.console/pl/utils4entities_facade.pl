% Author: Guenter Kniesel
% Date: 25.06.2006

/*
 * This file contains predicates for working with SWI-Prolog modules.
 */
 
 :- module( utils4entities_facade, [

           module_of_file/2,               % File, Module
           
           empty_module/1,                 % (Module)
           hidden_module/1,                % Module
           
           visible_in_module/3,	           % Module, Name, Arity
           declared_in_module/4,           % Module, Name, Arity, DeclaringModule 
           referenced_but_undeclared/3,    % Module, Name, Arity 
           declared_but_undefined/3,       % Module, Name, Arity 
           defined_in_module/2,            % Module, Head
           defined_in_module/3,            % Module, Name, Arity
           defined_in_module/4,            % Module, Name, Arity, DefiningModule
           defined_in_files/4,             % Module, Name, Arity, Locations
           defined_in_file/6,              % Module, Name, Arity, Nth,File,StartingLine
           declared_in_file/4,             % Module, Name, Arity, Location=[File-[Line]]          

           assert_in_module/2,             % Module, Head
           assert_in_module/3,             % Module, Head, Body
           clause_in_module/2,             % Module, Head
           clause_in_module/3,             % Module, Head, Body
           retract_in_module/2,            % Module, Head
           retract_in_module/3,            % Module, Head, Body
           retractall_in_module/2,         % Module, Head
           call_in_module/2,               % Module, Goal
           call_and_report_contex_module/1,% Goal
           report_contex_module/1,         % Module        
           listing_in_module/2,            % Module, FunctorOrHeadOrFkt/Arity
           
           copy_module_predicate/3,        % SrcMod, TargetMod, Head
           move_module_predicate/3         % SrcMod, TargetMod, Head
           ]
 ).
 
entity_of_file/2,               % File, Entity
                      
           visible_in_entity/3,	           % Entity, Name, Arity
           declared_in_entity/4,           % Entity, Name, Arity, DeclaringEntity 
           referenced_but_undeclared/3,    % Entity, Name, Arity 
           declared_but_undefined/3,       % Entity, Name, Arity 
           defined_in_entity/2,            % Entity, Head
           defined_in_entity/3,            % Entity, Name, Arity
           defined_in_entity/4,            % Entity, Name, Arity, DefiningEntity
           defined_in_files/4,             % Entity, Name, Arity, Locations
           defined_in_file/6,              % Entity, Name, Arity, Nth,File,StartingLine
           declared_in_file/4,             % Entity, Name, Arity, Location=[File-[Line]]          

           assert_in_entity/2,             % Entity, Head
           assert_in_entity/3,             % Entity, Head, Body
           clause_in_entity/2,             % Entity, Head
           clause_in_entity/3,             % Entity, Head, Body
           retract_in_entity/2,            % Entity, Head
           retract_in_entity/3,            % Entity, Head, Body
           retractall_in_entity/2,         % Entity, Head
           call_in_entity/2,               % Entity, Goal
           call_and_report_contex_entity/1,% Goal
           report_contex_entity/1,         % Entity        
           listing_in_entity/2,            % Entity, FunctorOrHeadOrFkt/Arity
           
           copy_entity_predicate/3,        % SrcEntity, TargetEntity, Head
           move_entity_predicate/3         % SrcEntity, TargetEntity, Head
           ]
 ).

