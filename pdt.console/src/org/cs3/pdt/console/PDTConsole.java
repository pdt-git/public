/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Lukas Degener (among others)
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2004-2012, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

package org.cs3.pdt.console;


final public class PDTConsole {
	public static final String CONSOLE_VIEW_ID="org.cs3.pdt.console.internal.views.PrologConsoleView";
    /*
     * the port on which the prolog console server is listening.
     */

	public static final String CONTRIB_PIF_SELECTOR_ID = "pdt.console.contribution.pifselector";
	public static final String PL_LIBRARY = "pdt.console.pl";
	public static final String COMMAND_PASTE_FILENAME = "pdt.console.paste_filename";
	public static final String CONTEXT_USING_CONSOLE_VIEW = "org.cs3.pdt.console";
	public static final int ERR_UNKNOWN = -1;
	public static final int ERR_PIF = -2;
	public static final int CX_CONSOLE_VIEW_ATTACH_TO_PIF = -3;
	public static final int CX_CONSOLE_SWITCH_PIF = -4;

	// Font & Color
	public static final String PREF_CONSOLE_FONT = "pdt.console.font";
	public static final String PREF_CONSOLE_COLOR_ERROR = "pdt.console.colors.error";
	public static final String PREF_CONSOLE_COLOR_WARNING = "pdt.console.colors.warning";

	public static final String PREF_CONSOLE_COLOR_INFO = "pdt.console.colors.info";

	public static final String PREF_CONSOLE_COLOR_DEBUG = "pdt.console.colors.debug";

	public static final String PREF_CONSOLE_COLORS_THREESTARS = "pdt.console.colors_threestars";

	// Main
	public static final String PREF_CONSOLE_HISTORY_FILE = "pdt.console.history.file";

	public static final String PREF_CONTEXT_TRACKERS = "pdt.console.trackers";

	public static final String PREF_RECONSULT_ON_RESTART = "pdt.reconsult.on.restart";
	

	public static final String RECONSULT_NONE = "pdt.reconsult.none";
	public static final String RECONSULT_ENTRY = "pdt.reconsult.entry.points";
	public static final String RECONSULT_ALL = "pdt.reconsult.all";
	
	
	
}


