/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Lukas Degener (among others) 
 * E-mail: degenerl@cs.uni-bonn.de
 * WWW: http://roots.iai.uni-bonn.de/research/pdt 
 * Copyright (C): 2004-2006, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms 
 * of the Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 * In addition, you may at your option use, modify and redistribute any
 * part of this program under the terms of the GNU Lesser General Public
 * License (LGPL), version 2.1 or, at your option, any later version of the
 * same license, as long as
 * 
 * 1) The program part in question does not depend, either directly or
 *   indirectly, on parts of the Eclipse framework and
 *   
 * 2) the program part in question does not include files that contain or
 *   are derived from third-party work and are therefor covered by special
 *   license agreements.
 *   
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 *   
 * ad 1: A program part is said to "depend, either directly or indirectly,
 *   on parts of the Eclipse framework", if it cannot be compiled or cannot
 *   be run without the help or presence of some part of the Eclipse
 *   framework. All java classes in packages containing the "pdt" package
 *   fragment in their name fall into this category.
 *   
 * ad 2: "Third-party code" means any code that was originaly written as
 *   part of a project other than the PDT. Files that contain or are based on
 *   such code contain a notice telling you so, and telling you the
 *   particular conditions under which they may be used, modified and/or
 *   distributed.
 ****************************************************************************/

package org.cs3.pdt;



/**
 * All kinds of string keys used by the pdt.
 */
public final  class PDT {
	
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
    public static final String PREF_ADD_NATURE_ON_OPEN = "pdt.ask.add_nature";
    
     
    /**
     * The basename of the resource bundle to be used by the pdt ui
     */
    
    public final static String RES_BUNDLE_UI = "org.cs3.pdt.ui";
    /**
     * log file location used by the pdt plugin.
     */
    public static final String PREF_CLIENT_LOG_FILE = "pdt.logfile";

	/**
	 * ui scope used for keybindings etc in the prolog editor.
	 */
    public static final String CONTEXT_EDITING_PROLOG_CODE = "org.cs3.pdt.editingProlog";

	public static final String PREF_SWITCH_TO_DEFAULT_PIF = "pdt.ask.switch_to_default_pif";

	public static final String PL_PARTITIONER = "pdt.pl_partitioner";

	public static final int ERR_UNKNOWN = -1;
	
	public static final int CX_UNKNOWN = -1;
	
	public static final int ERR_COMPLETION_BAD_LOCATION = -2;

	public static final int CX_COMPLETION = -2;

	public static final int ERR_PIF = -3;

	public static final int ERR_CORE_EXCEPTION = -4;

	public static final int CX_EDITOR_CONFIGURATION = -5;

	public static final int CX_GENERATING_OUTLINE_DATA = -6;

	public static final int CX_UPDATING_OUTLINE = -7;

	public static final int ERR_FILENAME_CONVERSION_PROBLEM = -8;

	public static final int CX_CONSULT =-9;

	public static final int ERR_WORKBENCH_UI_PROBLEM = -10;
	
	

	

}