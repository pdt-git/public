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

:- use_module(split_file_path).
:- use_module(pdt_search).                       % Search, outline and autocompletion
:- consult('lgt/loader.pl').                   % Search, outline etc. for Logtalk

:- use_module(editor/pdt_editor_reload).         % pdt_reload/1 & friends
:- use_module(editor/pdt_editor_manual_entry).   % for quick outline (find_pred/5)
%:-use_module(editor/pdt_editor_highlighting).   % no noeed to include here, used (directly) only in PLScanner.java

:- use_module(editor/pdt_editor_edit_hook).    % Contribution to SWIPL's prolog_edit:edit_source(Location)
:- use_module(editor/pdt_editor_breakpoints).  % API for pdt/.../PDTBreakpointHandler.java
:- use_module(pdt_entry_points).        % API for pdt/...
                                      % and for pdt.console/.../GenerateLoadFileWizard.java
:- use_module(source_files).            % API for PDTConsultDecoratorContributor.java
:- consult(smell_api).

% TODO: Load here or just in pdt_search.pl???
%:- use_module(pdt_runtime_builder_analyzer(properties)).
%:- use_module(pdt_runtime_builder_analyzer('metafile_referencer.pl')).
%:- use_module(pdt_prolog_library(utils4modules)).
