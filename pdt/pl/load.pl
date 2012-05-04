%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This file is part of the Prolog Development Tool (PDT)
%
% Authors: Lukas Degener, Tobias Rho, Günter Kniesel
% WWW: http://roots.iai.uni-bonn.de/research/pdt
%
% All rights reserved. This program is  made available under the terms
% of the Eclipse Public License v1.0 which accompanies this distribution,
% and is available at http://www.eclipse.org/legal/epl-v10.html 

%:- module(pdtplugin,[]). 

:- reexport(split_file_path).
:- reexport(pdt_search).                % find definitions, declarations (and autocompletion)
:- consult(lgt/loader.pl')).% Search, outline, etc. for Logtalk:

:- reexport(pdt_editor_reload).         % pdt_reload/1 & friends
:- reexport(pdt_editor_manual_entry).   % for quick outline (find_pred/5)
%:-reexport(pdt_editor_highlighting).   % no noeed to include here, used (directly) only in PLScanner.java


:- reexport(pdt_editor_edit_hook).    % Contribution to SWIPL's prolog_edit:edit_source(Location)
:- reexport(pdt_editor_breakpoints).  % API for pdt/.../PDTBreakpointHandler.java
:- reexport(pdt_entry_points).        % API for pdt/... 
                                      % and for pdt.console/.../GenerateLoadFileWizard.java
:- reexport(source_files).            % API for PDTConsultDecoratorContributor.java
:- consult(smell_api).

% TODO: Load here or just in pdt_search.pl???
:- use_module(pdt_runtime_builder_analyzer(properties)).
:- use_module(pdt_runtime_builder_analyzer('metafile_referencer.pl')).
:- use_module(pdt_prolog_library(utils4modules)).