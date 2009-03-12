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

package org.cs3.pdt.internal.actions;

import java.util.List;
import java.util.Map;
import java.util.ResourceBundle;

import org.cs3.pdt.PDT;
import org.cs3.pdt.PDTPlugin;
import org.cs3.pdt.PDTUtils;
import org.cs3.pdt.core.IPrologProject;
import org.cs3.pdt.core.PDTCore;
import org.cs3.pdt.core.PDTCorePlugin;
import org.cs3.pdt.core.PDTCoreUtils;
import org.cs3.pdt.internal.editors.PLEditor;
import org.cs3.pdt.ui.util.UIUtils;
import org.cs3.pl.common.Debug;
import org.cs3.pl.common.Util;
import org.cs3.pl.metadata.Clause;
import org.cs3.pl.metadata.Goal;
import org.cs3.pl.metadata.GoalData;
import org.cs3.pl.metadata.IMetaInfoProvider;
import org.cs3.pl.metadata.Predicate;
import org.cs3.pl.metadata.SourceLocation;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologInterfaceException;
import org.cs3.pl.prolog.PrologSession;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.text.IDocument;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IWorkbenchWindowActionDelegate;
import org.eclipse.ui.texteditor.ITextEditor;
import org.eclipse.ui.texteditor.TextEditorAction;

/**
 * @see IWorkbenchWindowActionDelegate
 */
public class FindPredicateActionDelegate extends TextEditorAction {
	private ITextEditor editor;

	

	/**
	 *  
	 */
	public FindPredicateActionDelegate(ITextEditor editor) {
		super(ResourceBundle.getBundle(PDT.RES_BUNDLE_UI),
				FindPredicateActionDelegate.class.getName(), editor); //$NON-NLS-1$
		this.editor = editor;

	}

	/**
	 * @see IWorkbenchWindowActionDelegate#run
	 */
	public void run() {
		try {
			final Goal data = ((PLEditor) editor)
					.getSelectedPrologElement();
			Shell shell = editor.getEditorSite().getShell();
			if (data == null) {
				
				UIUtils.displayMessageDialog(shell,
						"PDT Plugin",
						"Cannot locate a predicate at the specified location.");
				return;
			}
			final IFile file = UIUtils.getFileInActiveEditor();
			if(file==null){
//				UIUtils.logAndDisplayError(PDTPlugin.getDefault().getErrorMessageProvider(), shell, 
//						PDT.ERR_NO_ACTIVE_FILE, PDT.CX_FIND_PREDICATE, null);
			}
			
			Job j = new Job("Searching predicate definition") {
				protected IStatus run(IProgressMonitor monitor) {
					try {
						monitor.beginTask("searching...",
								IProgressMonitor.UNKNOWN);
						
						
							run_impl(data,file);
						

					} catch (Throwable e) {
						Debug.report(e);
						
					} finally {
						monitor.done();
					}
					return Status.OK_STATUS;
				}

				
			};
			j.schedule();
		} catch (Throwable t) {
			Debug.report(t);
		}

	}

	public void dispose() {
	}

	
	
	private void run_impl(Goal goal, IFile file) throws CoreException {
		IPrologProject plprj;
		plprj = (IPrologProject) file.getProject().getNature(PDTCore.NATURE_ID);
		PrologInterface pif = plprj.getMetadataPrologInterface();
		SourceLocation loc;
		try {
			loc = findFirstClausePosition((GoalData)goal,pif);
			if(loc!=null){
				PDTUtils.showSourceLocation(loc);
			}
		} catch (PrologInterfaceException e) {
			Debug.report(e);
			Shell shell = editor.getSite().getShell();
			
			
			
			UIUtils.displayErrorDialog(shell, "PrologInterface Error", "The connection to the Prolog process was lost. ");
		}
		
		
	}

	
	private SourceLocation findFirstClausePosition(GoalData data, PrologInterface pif) throws PrologInterfaceException{
		PrologSession session = null;
		String module=data.getModule()==null?"_":"'"+data.getModule()+"'";
		
		String query="pdt_resolve_predicate('"+data.getFile()+"',"+module+", '"+data.getName()+"',"+data.getArity()+",Pred),"
		+ "pdt_predicate_contribution(Pred,File,Start,End)";
		Map m=null;
		try{
			session=pif.getSession(PrologInterface.NONE);
			m = session.queryOnce(query);
		}
		finally{
			if(session!=null){
				session.dispose();
			}
		}
		String fileName = Util.unquoteAtom((String) m.get("File"));
		SourceLocation loc=new SourceLocation(fileName,false,false);
		loc.offset=Integer.parseInt((String)m.get("Start"));
		loc.endOffset=Integer.parseInt((String)m.get("End"));
		
		return loc;
	}
	
	private SourceLocation findFirstClausePosition_old(IFile file, Goal goal) throws PrologInterfaceException {
		IPrologProject plprj;
		try {
			plprj = (IPrologProject) file.getProject().getNature(PDTCore.NATURE_ID);
		} catch (CoreException e) {
			Debug.report(e);
			throw new RuntimeException(e);
		}
		String plFile = Util.prologFileName(file.getLocation().toFile());
		PrologSession s = plprj.getMetadataPrologInterface().getSession(PrologInterface.NONE);
		SourceLocation loc =null;
		try{
			String contextModule = goal.getModule();
			String query=null;
			if(contextModule==null){
				query = "pdt_file_module('"+plFile+"',Context)," +
						"pdt_find_first_clause_position(Context,"+goal.getName()+"/"+goal.getArity()+",File,From-To)";	
			}else{
				query = "pdt_find_first_clause_position("+contextModule+","+goal.getName()+"/"+goal.getArity()+",File,From-To)";
			}
			Map map = s.queryOnce(query);
			if(map==null){
				return null;
			}
			String fileName = (String) map.get("File");
			loc=new SourceLocation(fileName,false,false);
			loc.offset=Integer.parseInt((String)map.get("From"));
			loc.endOffset=Integer.parseInt((String)map.get("To"));
		}finally{
			if(s!=null){
				s.dispose();
			}
		}
		return loc;
	}
	
	

}