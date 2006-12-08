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

package org.cs3.pdt.core;

import java.util.ArrayList;

import org.cs3.pl.prolog.PrologInterface;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;

public class PDTCore {

	/**
	 * The prolog project nature.
	 * <p>
	 * To get an <code>IPrologProject</code> from an <code>IProject</code>
	 * you would do the following:
	 * <p>
	 * <code>
	 * IProject project = <i>(---some prolog project---)</i>;
	 * <p>
	 * IPrologProject prologProject = (IPrologProject)project.getNature(PDT.NATURE_ID);
	 * </code>
	 */
	public final static String NATURE_ID = "org.cs3.pdt.core.PDTProjectNature";
	
	
	public static final String PROLOG_BUILDER_ID = "org.cs3.pdt.core.PrologBuilder";
	
	/**
	 * The key to which the meta data consult sevice is bound.
	 */
	public static final String CS_METADATA = "metadata";
	/**
	 * An absolute os file system path to the directory containing the prolog
	 * files of the pdt metadata engine.
	 */
	public final static String PREF_METADATA_ENGINE_DIR = "pdt.metadata.engine.dir";
	
	/**
	 * An absolute os file system path to the directory containing metadata
	 * store
	 */
	public final static String PREF_METADATA_STORE_DIR = "pdt.metadata.store.dir";
	/**
	 * The "master-switch" for auto-consulting of files.
	 * It defaults to "false". for 0.1.1 - see PDT-23 
	 * If set to "true" , every prolog source file on the sourc path will
	 * be automaticaly consulted as long as its PROP_NO_AUTO_CONSULT
	 * flag is not set.
	 */
	public static final String PREF_AUTO_CONSULT = "pdt.auto.consult";
	/**
	 * a path.separator-separated list of project relative paths pointing to the
	 * source directories of a project.
	 */
	public static final String PROP_SOURCE_PATH = "pdt.source.path";
	/**
	 * a file for which this property is "true" will NOT  be consulted each time it
	 * is touched by the meta data builder. This option only applies when 
	 * PREF_AUTO_CONSULT is set.
	 */
	public static final String PROP_NO_AUTO_CONSULT = "pdt.no.auto.consult";
	/**
	 * the default to use for PROP_SOURCE_PATH
	 */
	public static final String PREF_SOURCE_PATH_DEFAULT = "pdt.source.path.default";
	/**
	 * a regular expression. Only files in the source folder, that 
	 * match this pattern are considered during builds.
	 */
	public static final String PROP_SOURCE_INCLUSION_PATTERN = "pdt.inclusion.pattern";
	/**
	 * a regular expression. Files in the source folder, that match thise pattern
	 * are exlcluded from the build, even if they match the inclusion pattern.
	 */
	public static final String PROP_SOURCE_EXCLUSION_PATTERN = "pdt.exlusion.pattern";
	public static final String PLUGIN_ID = "org.cs3.pdt.core";

	public static final String ENGINE_ID = "pdt.core.library";

	public static final String PROP_METADATA_PIF_KEY = "pdt.metadata.pif_key";
	public static final String PROP_RUNTIME_PIF_KEY = "pdt.runtime.pif_key";
	public static final String PROP_PARSE_COMMENTS = "pdt.parse_comments";
	public static final String PROP_ADDITIONAL_LIBRARIES = "pdt.additional_libraries";
	public static final String PROP_DEFAULT_ENCODING = "pdt.default_encoding";
	public static final String PREF_METADATA_PIF_KEY_DEFAULT = "pdt.metadata.pif_key.default";
	public static final String PREF_RUNTIME_PIF_KEY_DEFAULT = "pdt.runtime.pif_key.default";
	public static final String PREF_CONVERT_CHARACTER_OFFSETS = "pdt.convert.character.offsets";
	public static final String PREF_IGNORE_HIDDEN_LIBS = "pdt.ignore.hidden.libs";

	public static final String BUILTIN_INDEX_FILE = "builtin_predicates.idx";
	public static final String CACHE_DIR = "cache";
	
	public static final int ERR_UNKNOWN = -1;
	public static final int ERR_PIF = -2;

	public static final int CX_UNKNOWN = -1;
	public static final int CX_START_PIF = -2;


	public static final int CX_TOGGLE_SOURCE_PATH_ENTRY = -3;


	public static final int CX_REMOVE_SUBSCRIPTIONS = -4;
	public static final int CX_ADD_SUBSCRIPTIONS = -5;


	public static final int CX_CHECK_PROJECTS = -6;


	


	


	


	


	


	


	
	

}
