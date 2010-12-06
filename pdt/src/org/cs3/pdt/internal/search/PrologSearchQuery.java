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

import java.io.File;
import java.io.IOException;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.cs3.pdt.PDT;
import org.cs3.pdt.console.PrologConsolePlugin;
import org.cs3.pdt.core.PDTCoreUtils;
import org.cs3.pdt.ui.util.UIUtils;
import org.cs3.pl.common.Debug;
import org.cs3.pl.common.Util;
import org.cs3.pl.metadata.Goal;
import org.cs3.pl.metadata.GoalData;
import org.cs3.pl.prolog.PrologException;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologInterfaceException;
import org.cs3.pl.prolog.PrologSession;
import org.eclipse.core.filebuffers.FileBuffers;
import org.eclipse.core.filebuffers.ITextFileBuffer;
import org.eclipse.core.filebuffers.ITextFileBufferManager;
import org.eclipse.core.filebuffers.LocationKind;
import org.eclipse.core.filesystem.EFS;
import org.eclipse.core.filesystem.IFileStore;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.search.ui.ISearchQuery;
import org.eclipse.search.ui.ISearchResult;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.ide.FileStoreEditorInput;
import org.eclipse.ui.part.FileEditorInput;

public class PrologSearchQuery implements ISearchQuery {

	private GoalData data;
	private PrologSearchResult result;
	private PrologInterface pif;

	public PrologSearchQuery(PrologInterface pif, Goal data) {
		this.data = (GoalData) data;
		this.pif=pif;
		result = new PrologSearchResult(this, data);
	}

	@Override
	public IStatus run(IProgressMonitor monitor) {
		try {
			return run_impl(monitor);
		} catch (Throwable t) {
			Debug.report(t);
			return new Status(IStatus.ERROR,PDT.PLUGIN_ID,42,"Exception caught during search.",t);
		}
	}
	private IStatus run_impl(IProgressMonitor monitor) throws CoreException,
			BadLocationException, IOException, PrologException, PrologInterfaceException {
		result.removeAll();
		if(data==null){
			Debug.error("Data is null!");
			throw new NullPointerException();
		}
		PrologSession session = null;
		if(pif!= null){
			String module=data.getModule()==null?"_":"'"+data.getModule()+"'";
	
			String query="pdt_resolve_predicate('"+data.getFile()+"',"+module+", '"+data.getName()+"',"+data.getArity()+",Pred),"
			+ "pdt_predicate_reference(Pred,File,Start,End,Caller,Type)";
			List<Map<String,Object>> results=null;
			try{
				session=pif.getSession(PrologInterface.NONE);
				results = session.queryAll(query);
			}
			finally{
				if(session!=null){
					session.dispose();
				}
			}
			for (Iterator<Map<String,Object>> iterator = results.iterator(); iterator.hasNext();) {
				Map<String,Object> m = iterator.next();

				int start = Integer.parseInt((String) m.get("Start"));
				int end = Integer.parseInt((String) m.get("End"));
				IFile file=null;
				String path =null; 
				try{
					path = Util.unquoteAtom((String) m.get("File"));
					file = PDTCoreUtils.findFileForLocation(path);
				}catch(IllegalArgumentException iae){
					Debug.report(iae);
					continue;
				}

				//IRegion resultRegion = new Region(start,end-start);

				if(file==null||! file.isAccessible()){
					String msg = "Not found in workspace: "+path;
					Debug.warning(msg);
					UIUtils.setStatusErrorMessage(msg);
					continue;
				}
				String type = (String)m.get("Type");
				PredicateElement pe = new PredicateElement();
				pe.setLabel((String) m.get("Caller"));
				pe.setType(type);

				PrologMatch match = new PrologMatch(pe, start, end-start);

				match.type=type;
				result.addMatch(match);

			}
		} else { // not pdt nature found
			if(PrologConsolePlugin.getDefault().getPrologConsoleService().getActivePrologConsole()!= null){
					session = PrologConsolePlugin.getDefault().getPrologConsoleService().getActivePrologConsole().getPrologInterface().getSession();
					String module = "Module";
					if(data.getModule()!=null)
						module ="'"+ data.getModule()+ "'";
					String enclFile = UIUtils.getFileFromActiveEditor();
//					FileStoreEditorInput fStoreInput = (FileStoreEditorInput)editor.getEditorInput();
//					//Path filepath = new Path(fStoreInput.getURI().getPath());

					
					String query = "get_references('"+enclFile+"','" + data.getName()+"'/" + data.getArity()+ "," + module  + ",File,Line,RefModule,Name,Arity)";
					Debug.info(query);
					List<Map<String, Object>> clauses = session.queryAll(query);
					
					if(clauses.size()==0){ 
						// a user module predicate (e.g. clause_property/2) is not-yet used in a module:
						query = "get_references(_,'" + data.getName()+"'/" + data.getArity()+ "," + module  + ",File,Line,RefModule,Name,Arity)";
						Debug.info("Look up predicate in user module: "+query); 
						clauses = session.queryAll(query);
					} 
					if(clauses.size()>0 && data.getModule()==null){
						data.setModule((String)clauses.get(0).get("Module"));
					}

					for (Iterator<Map<String,Object>> iterator = clauses.iterator(); iterator.hasNext();) {
						Map<String,Object> m = iterator.next();
						Debug.info(m.toString());
						IFile file=null;
						String path = Util.unquoteAtom((String) m.get("File"));
						try{
							file = PDTCoreUtils.findFileForLocation(path);
							
						}catch(IllegalArgumentException iae){
//							Debug.report(iae);
						}
						
						if(file==null|| !file.isAccessible()){
							Path location = new Path(path);
							file = new ExternalFile(location);
							String msg = "Not found in workspace: "+path;
//							Debug.warning(msg);
//							UIUtils.setStatusErrorMessage(msg);
//							continue;
						}
						String type = (String)m.get("RefModule");
						PredicateElement pe = new PredicateElement();

						String name = (String)m.get("Name");
						int arity = Integer.parseInt((String)m.get("Arity"));
						pe.setLabel((String) name + "/" + arity);
						pe.setType(type);
						pe.setFile(file);
						pe.setPredicateName(data.getName());
						pe.setArity(data.getArity());

						int line = Integer.parseInt((String) m.get("Line"))-1;

//						  ITextFileBufferManager iTextFileBufferManager = FileBuffers.getTextFileBufferManager();
//						    ITextFileBuffer iTextFileBuffer = null;
//						    try    {
//						    	int offset;
//								IFile[] files = ResourcesPlugin.getWorkspace().getRoot().findFilesForLocationURI(new URI("file",file,null));
//								PrologMatch match;
//								if(file.isAccessible()){
//							        iTextFileBufferManager.connect(file.getFullPath(), LocationKind.IFILE, new NullProgressMonitor());
//							        iTextFileBuffer = iTextFileBufferManager.getTextFileBuffer(file.getFullPath(), LocationKind.IFILE);
//							        IDocument doc = iTextFileBuffer.getDocument();
//							        offset= doc.getLineOffset(line);
//							        match = new PrologMatch(pe, offset,0 );
//							        iTextFileBufferManager.disconnect(file.getFullPath(), LocationKind.IFILE, new NullProgressMonitor());
//								} else {
//									iTextFileBufferManager.connect(location, LocationKind.NORMALIZE, new NullProgressMonitor());
//							      
//									IFileStore fileStore = EFS.getLocalFileSystem().getStore(location);
//									iTextFileBuffer = iTextFileBufferManager.getFileStoreTextFileBuffer(fileStore);
//							        IDocument doc = iTextFileBuffer.getDocument();
//							        offset= doc.getLineOffset(line);
//							        iTextFileBufferManager.disconnect(location, LocationKind.NORMALIZE, new NullProgressMonitor());
//								}

					    	PrologMatch match = new PrologMatch(pe, line,0 ); // the line offset is only used for sorting here
					        match.setLine(line);
								match.type=type;
								result.addMatch(match);
//						    } catch (Exception e) {
//						        e.printStackTrace();
//						    }
					}
					return Status.OK_STATUS;
			}
			return Status.CANCEL_STATUS;

		}
		
		return Status.OK_STATUS;
	}



	@Override
	public String getLabel() {
		return "Prolog Query: " + data.getSignature();
	}

	@Override
	public boolean canRerun() {
		return true;
	}

	@Override
	public boolean canRunInBackground() {
		return false;
	}

	@Override
	public ISearchResult getSearchResult() {
		return result;
	}

}