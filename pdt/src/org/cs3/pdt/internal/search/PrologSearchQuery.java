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
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.cs3.pdt.PDT;
import org.cs3.pdt.core.PDTCorePlugin;
import org.cs3.pdt.core.PDTCoreUtils;
import org.cs3.pdt.ui.util.UIUtils;
import org.cs3.pl.common.Debug;
import org.cs3.pl.common.Util;
import org.cs3.pl.metadata.Goal;
import org.cs3.pl.metadata.GoalData;
import org.cs3.pl.metadata.IMetaInfoProvider;
import org.cs3.pl.metadata.Predicate;
import org.cs3.pl.metadata.SourceLocation;
import org.cs3.pl.prolog.PrologException;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologInterfaceException;
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

public class PrologSearchQuery implements ISearchQuery {

	private GoalData data;

	private PrologSearchResult result;

	private HashMap fSearchViewStates = new HashMap();

	private PrologInterface pif;

	

	public PrologSearchQuery(PrologInterface pif, Goal data) {
		this.data = (GoalData) data;
		this.pif=pif;
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
			BadLocationException, IOException, PrologException, PrologInterfaceException {
		result.removeAll();
		if(data==null){
			Debug.error("Data is null!");
			throw new NullPointerException();
		}
		String title = data==null?"oops, data is null?!" : data.getModule()+":"+data.getName()+"/"+data.getArity();
		if (false/*FIXME was: data.isModule()*/){
			title += "  Search for modules not supported yet!";
		}
		else{
			PrologSession session = null;
			String module=data.getModule()==null?"_":"'"+data.getModule()+"'";
			
			String query="pdt_resolve_predicate('"+data.getFile()+"',"+module+", '"+data.getName()+"',"+data.getArity()+",Pred),"
			+ "pdt_predicate_reference(Pred,File,Start,End,Caller,Type)";
			List l=null;
			try{
				session=pif.getSession(PrologInterface.NONE);
				l = session.queryAll(query);
			}
			finally{
				if(session!=null){
					session.dispose();
				}
			}
			for (Iterator iterator = l.iterator(); iterator.hasNext();) {
				Map m = (Map) iterator.next();
				
				int start = Integer.parseInt((String) m.get("Start"));
				int end = Integer.parseInt((String) m.get("End"));
				IFile file=null;
				String path =null; 
				try{
					path = Util.unquoteAtom((String) m.get("File"));
					file = PDTCoreUtils.findFileForLocation(path);
				}catch(IllegalArgumentException iae){
					//probably the file is not in the workspace. 
					//nothing to worry about here.
					Debug.report(iae);
					continue;
				}
				
				IRegion resultRegion = new Region(start,end-start);
				
				if(file==null||! file.isAccessible()){
					String msg = "Not found in workspace: "+path;
					Debug.warning(msg);
					UIUtils.setStatusErrorMessage(msg);
					continue;
				}
				String type = (String)m.get("Type");
				PredicateElement pe = new PredicateElement();
				pe.file=file;
				pe.label=(String) m.get("Caller");
				pe.type=type;
				
				PrologMatch match = new PrologMatch(pe, resultRegion
						.getOffset(), resultRegion.getLength());
				
				match.type=type;
				result.addMatch(match);
								
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