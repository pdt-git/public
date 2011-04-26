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
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.cs3.pdt.PDT;
import org.cs3.pdt.console.PrologConsolePlugin;
import org.cs3.pdt.core.PDTCoreUtils;
import org.cs3.pdt.ui.util.UIUtils;
import org.cs3.pl.common.Debug;
import org.cs3.pl.common.Util;
import org.cs3.pl.metadata.GoalData;
import org.cs3.pl.prolog.PrologException;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologInterfaceException;
import org.cs3.pl.prolog.PrologSession;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.search.ui.ISearchQuery;
import org.eclipse.search.ui.ISearchResult;

public abstract class PrologSearchQuery implements ISearchQuery {

	private static String lineKey = "Line";
	private static String arityKey = "Arity";
	private static String functorKey = "Name";
	private static String moduleKey = "RefModule";
	private static String fileKey = "File";
	private GoalData goal;
	private PrologSearchResult result;
	private PrologInterface pif;

	public PrologSearchQuery(PrologInterface pif, GoalData goal) {
		this.goal = goal;
		this.pif=pif;
		result = new PrologSearchResult(this, goal);
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
	
	// TODO: The progress monitor parameter is never used. 
	private IStatus run_impl(IProgressMonitor monitor) throws CoreException,
			BadLocationException, IOException, PrologException, PrologInterfaceException {
		result.removeAll();
		if(goal==null){
			Debug.error("Search goal data is null!");
			throw new NullPointerException();
		}

		if(pif!= null){ // The project has the PDT nature
			return searchIfPDTNatureIsSet();
		} else {                                 
			return searchIfPDTNatureIsMissing(); 
		}
	}

	/**
	 * @return
	 * @throws PrologInterfaceException
	 * @throws PrologException
	 * @throws IOException
	 * @throws NumberFormatException
	 */
	private IStatus searchIfPDTNatureIsMissing()
			throws PrologInterfaceException, PrologException, IOException,
			NumberFormatException {

		if(PrologConsolePlugin.getDefault().getPrologConsoleService().getActivePrologConsole() == null){
			return Status.CANCEL_STATUS;
		}
		else {
			PrologSession session = PrologConsolePlugin.getDefault().getPrologConsoleService()
			                        .getActivePrologConsole().getPrologInterface().getSession();

			processFoundClauses(findReferencedClauses(session));
			return Status.OK_STATUS;
		}
	}

	/**
	 * @param session
	 * @return
	 * @throws PrologException
	 * @throws PrologInterfaceException
	 */
	private List<Map<String, Object>> findReferencedClauses(PrologSession session)
			throws PrologException, PrologInterfaceException {
		String module = "Module";
		if(goal.getModule()!=null)
			module ="'"+ goal.getModule()+ "'";
		
		String enclFile = UIUtils.getFileFromActiveEditor();

		String query = buildSearchQuery(module, enclFile, goal);
		List<Map<String, Object>> clauses = getResultForQuery(session, module,
				query, goal);
		return clauses;
	}
	
	abstract protected String buildSearchQuery(String module, String enclFile, GoalData goal);

	abstract protected List<Map<String, Object>> getResultForQuery(PrologSession session,
			String module, String query, GoalData goal) throws PrologInterfaceException ;

	/**
	 * @param clauses
	 * @throws IOException
	 * @throws NumberFormatException
	 */
	private void processFoundClauses(List<Map<String, Object>> clauses)
			throws IOException, NumberFormatException {

		for (Iterator<Map<String,Object>> iterator = clauses.iterator(); iterator.hasNext();) {
			Map<String,Object> m = iterator.next();
			Debug.info(m.toString());
			
			String type = (String)m.get(getModuleKey());
			String name = (String)m.get(getFunctorKey());
			int arity = Integer.parseInt((String)m.get(getArityKey()));
			
			IFile file = getFileForString(m, getFileKey());
			int line = Integer.parseInt((String) m.get(getLineKey()))-1;
			
			PredicateElement pe = new PredicateElement(file, type, name, arity);

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

		    createPrologMatchForResult(pe, line, 0);
//						    } catch (Exception e) {
//						        e.printStackTrace();
//						    }
		}
	}

	private IFile getFileForString(Map<String, Object> m, String FileString)
			throws IOException {
		IFile file = null;
		String path = Util.unquoteAtom((String) m.get(getFileKey()));
		try{
			file = PDTCoreUtils.findFileForLocation(path);
		}catch(IllegalArgumentException iae){
//							Debug.report(iae);
		}
		if(file==null|| !file.isAccessible()){
			Path location = new Path(path);
			file = new ExternalFile(location);
//				String msg = "Not found in workspace: "+path;
//							Debug.warning(msg);
//							UIUtils.setStatusErrorMessage(msg);
//							continue;
		}
		return file;
	}

	/**
	 * @throws PrologInterfaceException
	 * @throws PrologException
	 * @throws NumberFormatException
	 * @throws IOException
	 */
	private IStatus searchIfPDTNatureIsSet() throws PrologInterfaceException,
			PrologException, NumberFormatException, IOException {
		PrologSession session=null;
		String module=goal.getModule()==null?"_":"'"+goal.getModule()+"'";

		String query="pdt_resolve_predicate('"+goal.getFile()+"',"+module+", '"+goal.getName()+"',"+goal.getArity()+",Pred),"
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
				path = Util.unquoteAtom((String) m.get(getFileKey()));
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
			
			String callerPredicate =(String) m.get("Caller");
			String[] callerParts=callerPredicate.split("/");
			String name = callerParts[0];
			int arity = Integer.getInteger(callerParts[1]);
			
			PredicateElement pe = new PredicateElement(file, type, name, arity);

			createPrologMatchForResult(pe, start, end-start);
		}
		return Status.OK_STATUS;
	}


	private void createPrologMatchForResult(PredicateElement pe, int line, int length) {
		PrologMatch match = new PrologMatch(pe, line, length); 
		match.setLine(line);
		match.type=pe.getType();
		result.addMatch(match);
	}

	protected final GoalData getGoal() {
		return goal;
	}

	@Override
	public String getLabel() {
		return "Prolog Query: " + goal.getSignature();
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

	
	
	
	public static void setLineKey(String lineKey) {
		PrologSearchQuery.lineKey = lineKey;
	}

	public static String getLineKey() {
		return lineKey;
	}

	public static void setArityKey(String arityKey) {
		PrologSearchQuery.arityKey = arityKey;
	}

	public static String getArityKey() {
		return arityKey;
	}

	public static void setFunctorKey(String functorKey) {
		PrologSearchQuery.functorKey = functorKey;
	}

	public static String getFunctorKey() {
		return functorKey;
	}

	protected static void setModuleKey(String moduleKey) {
		PrologSearchQuery.moduleKey = moduleKey;
	}

	protected static String getModuleKey() {
		return moduleKey;
	}

	protected static void setFileKey(String fileKey) {
		PrologSearchQuery.fileKey = fileKey;
	}

	protected static String getFileKey() {
		return fileKey;
	}

}