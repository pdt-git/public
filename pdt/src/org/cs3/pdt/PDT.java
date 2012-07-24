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

package org.cs3.pdt;

/**
 * All kinds of string keys used by the pdt.
 */
public final class PDT {

	public static final String PLUGIN_ID = "org.cs3.pdt";

	/**
	 * Specifies the default level of verbosity. Valid values are "DEBUG" (VERY
	 * verbose), "INFO", "WARNING","ERROR" and "NONE" (quiet)
	 * 
	 * The property will be read out once the Debug class is loaded, and the
	 * debug level will be set accordingly. After that, the level can be changed
	 * using the static Debug.setDeubgLevel(int) method.
	 */
	public final static String PREF_DEBUG_LEVEL = "debug.level";
	public final static String PREF_DEBUG_OUTPUT_TO = "debug.output.to";
	public static final String PREF_EXTERNAL_FILE_SAVE_WARNING = "pdt.external.file.save.warning";

	/**
	 * The basename of the resource bundle to be used by the pdt ui
	 */

	public final static String RES_BUNDLE_UI = "org.cs3.pdt.ui";
	/**
	 * log file location used by the pdt plugin.
	 */
	public static final String PREF_CLIENT_LOG_FILE_DIR = "pdt.logfile";

	/**
	 * ui scope used for keybindings etc in the prolog editor.
	 */
	public static final String CONTEXT_EDITING_PROLOG_CODE = "org.cs3.pdt.editingProlog";
	public static final String PL_PARTITIONER = "pdt.pl_partitioner";
	public static final int ERR_UNKNOWN = -1;
	public static final int CX_UNKNOWN = -1;
	public static final int ERR_COMPLETION_BAD_LOCATION = -2;
	public static final int CX_COMPLETION = -2;
	public static final int ERR_PIF = -3;
	public static final int ERR_CORE_EXCEPTION = -4;
	public static final int CX_EDITOR_CONFIGURATION = -5;
	public static final int CX_GENERATING_OUTLINE_DATA = -6;
	public static final int CX_OUTLINE = -7;
	public static final int ERR_FILENAME_CONVERSION_PROBLEM = -8;
	public static final int CX_CONSULT = -9;
	public static final int ERR_WORKBENCH_UI_PROBLEM = -10;
	public static final String PREF_OUTLINE_FILTERS = "pdt.outline.filters";
	public static final String PREF_OUTLINE_SORT = "pdt.outline.sort";
	public static final String PREF_OUTLINE_FILTER_PRIVATE = "pdt.outline.filter.private";
	public static final String PREF_OUTLINE_FILTER_SYSTEM = "pdt.outline.filter.system";
	public static final int ERR_NO_ACTIVE_FILE = -11;
	public static final int CX_FIND_PREDICATE = -10;
	public static final int ERR_OUTLINE_BAD_LOCATION = -12;

	public static final String PREF_AUTO_COMPLETE_ARGLIST = "pdt.autocompletion.arglist";

}


