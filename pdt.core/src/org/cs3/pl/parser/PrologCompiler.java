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

package org.cs3.pl.parser;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.List;
import java.util.Set;


public interface PrologCompiler {

	public static final String METADATA = "meta_data";

	public static final String METADATAHELP = METADATA + "_help";

	public static final String METADATAMODULE = METADATA + "_module";

	public static final String MSG_COND_PAREN = "the condition part of the ct should be surrounded by parenthesis.";

	public static final String MSG_ACTION_PAREN = "the action part of the ct should be surrounded by parenthesis.";

	public static final String MSG_SINGLETON_VAR_PREFIX = "Singleton variable: ";

	public static final String MSG_SINGLETON_VAR_POSTFIX = " (one occurence only)";

	public abstract void compile(String content);

	public abstract void compile(String symbolicFileName, InputStream content,
			LineBreakInfoProvider lineInfo);
	
	
	public ProblemCollector getProblemCollector();
	public void setProblemCollector(ProblemCollector problemCollector);
	
	public TaskCollector getTaskCollector();
	public void setTaskCollector(TaskCollector taskCollector);
	
	
	public abstract void saveMetaDataForClauses(OutputStream stream)
			throws IOException;

	
	/**@deprecated currently only used by test cases, will be removed.*/
	public abstract List getClauses();
	/**@deprecated currently only used by test cases, will be removed.*/
	public abstract String getModuleName();
	/**@deprecated currently only used by test cases, will be removed.*/
	public abstract Set getPublicModulePredicates();

	public abstract void saveAbbaData(OutputStream out) throws IOException;

	public abstract void updateIndex(Index index);		
	

}