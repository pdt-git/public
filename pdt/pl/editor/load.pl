%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This file is part of the Prolog Development Tool (PDT)
%
% Author: Günter Kniesel
% Date: 26.6.2012
% WWW: http://roots.iai.uni-bonn.de/research/pdt
% 
% All rights reserved. This program is  made available under the terms
% of the Eclipse Public License v1.0, which accompanies this distribution
% and is available also at http://www.eclipse.org/legal/epl-v10.html

%%%:- module(pdteditor,[]).  
%%% <--- TODO: If everything works, turn this into an own module

:- use_module(pdt_editor_reload).         % pdt_reload/1 & friends
:- use_module(pdt_editor_manual_entry).   % for quick outline (find_pred/5)
%:-use_module(pdt_editor_highlighting).   % no noeed to include here, used (directly) only in PLScanner.java

:- use_module(pdt_editor_edit_hook).    % Contribution to SWIPL's prolog_edit:edit_source(Location)
