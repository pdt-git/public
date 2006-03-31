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

package org.cs3.pdt.internal.search;

import java.io.IOException;
import java.util.HashMap;

import org.cs3.pdt.PDT;
import org.cs3.pdt.PDTUtils;
import org.cs3.pdt.core.PDTCorePlugin;
import org.cs3.pdt.ui.util.UIUtils;
import org.cs3.pl.common.Debug;
import org.cs3.pl.metadata.IMetaInfoProvider;
import org.cs3.pl.metadata.Predicate;
import org.cs3.pl.metadata.SourceLocation;
import org.cs3.pl.prolog.PrologSession;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.Region;
import org.eclipse.search.ui.ISearchQuery;
import org.eclipse.search.ui.ISearchResult;
import org.eclipse.search.ui.text.Match;

public class PrologSearchQuery implements ISearchQuery {

	private Predicate data;

	private PrologSearchResult result;

	private HashMap fSearchViewStates = new HashMap();

	private IMetaInfoProvider metaInfoProvider;

	public PrologSearchQuery(IMetaInfoProvider provider, Predicate data) {
		this.data = data;
		this.metaInfoProvider=provider;
		result = new PrologSearchResult(this, data);
		

	}

	public IStatus run(IProgressMonitor monitor) {
		try {
			return run_impl(monitor);
		} catch (Throwable t) {
			Debug.report(t);
			return new Status(Status.ERROR,PDT.PLUGIN_ID,42,"Exception caught during search.",t);
		}
	}

	private IStatus run_impl(IProgressMonitor monitor) throws CoreException,
			BadLocationException, IOException {
		if(data==null){
			Debug.error("Data is null!");
			throw new NullPointerException();
		}
		String title = data==null?"oops, data is null?!" : data.getSignature();
		if (false/*FIXME was: data.isModule()*/){
			title += "  Search for modules not supported yet!";
		}
		else{
			PrologSession session;
			
			SourceLocation[] locations = metaInfoProvider.findReferences(data);
			if(locations==null){
				//FIXME: is it realy ok? --lu
				return Status.OK_STATUS;
			}
			for (int i = 0; i < locations.length; i++) {
				SourceLocation location = locations[i];
				if (location.isRowBased){
					String msg = "Sorry, i currently can not handle row-based locations.";
					Debug.warning(msg);
					UIUtils.setStatusErrorMessage(msg);
					continue;
				}
				IRegion resultRegion = new Region(location.offset,location.endOffset-location.offset);
				IFile file = null;
				if(location.isWorkspacePath){
					ResourcesPlugin.getWorkspace().getRoot().getFile(new Path(location.file));
				}
				else{
					file = PDTUtils.findFileForLocation(location.file);
				}
				if(file==null||! file.isAccessible()){
					String msg = "Not found in workspace: "+location.file;
					Debug.warning(msg);
					UIUtils.setStatusErrorMessage(msg);
					continue;
				}
				
				result.addMatch(new Match(file, resultRegion
						.getOffset(), resultRegion.getLength()));
				Debug.debug("Found reference: " + file + ", offset: "
						+ resultRegion.getOffset() + ", length: "
						+ resultRegion.getLength());				
			}
		}
		return Status.OK_STATUS;
	}

	public String getLabel() {
		return "Prolog Query: " + data.getSignature();
	}

	public boolean canRerun() {
		return true;
	}

	public boolean canRunInBackground() {
		return false;
	}

	public ISearchResult getSearchResult() {
		return result;
	}

}