package org.cs3.pl.builders;

import java.io.IOException;
import java.util.Collection;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.NoSuchElementException;
import java.util.Vector;

import org.cs3.pl.Debug;
import org.cs3.pl.PDTPlugin;
import org.cs3.pl.astvisitor.CompilationUnitProvider;
import org.cs3.pl.astvisitor.DefaultGenerationToolbox;
import org.cs3.pl.astvisitor.FactGenerationToolBox;
import org.cs3.pl.astvisitor.FactGenerator;
import org.cs3.pl.astvisitor.ICompilationUnitProvider;
import org.cs3.pl.bytecode.ByteCodeFactGeneratorIType;
import org.cs3.pl.exceptions.ExceptionHandler;
import org.cs3.pl.extension.IJTransformerObserver;
import org.cs3.pl.fileops.MetaDataManagerFactory;
import org.cs3.pl.fileops.PrependablePrologWriter;
import org.cs3.pl.fileops.PrologMetaDataManager;
import org.cs3.pl.natures.JLMPProjectNature;
import org.cs3.pl.prolog.IPrologClient;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceDelta;
import org.eclipse.core.resources.IResourceDeltaVisitor;
import org.eclipse.core.resources.IResourceVisitor;
import org.eclipse.core.resources.WorkspaceJob;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.core.runtime.Status;
import org.eclipse.jdt.core.ICompilationUnit;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.jdt.core.dom.AST;
import org.eclipse.jdt.core.dom.ASTParser;
import org.eclipse.jdt.core.dom.CompilationUnit;
import org.eclipse.ui.progress.WorkbenchJob;


public class FactBaseBuilder {
	
	//i borrow some funktionality from the CompilationUnitProvider, This code
	// should propably be moved here.
	private ICompilationUnitProvider cuProvider;
	
	//hm. a prolog manager, what else. stupid.
	private IPrologClient prologClient;
	private IProject project;
	
	private Collection failed = new Vector();
	private Iterator nextIterator = null;
	
	private PrologMetaDataManager metaDataEXT;
	private PrologMetaDataManager metaDataPL;
	
	/**
	 * delete all previously generated project fact files.
	 */
	public final static int CLEAR_PRJ_FACTS = 1;
	/**
	 * delete all previously generated external fact files.
	 */
	public final static int CLEAR_EXT_FACTS = 2;
	/**
	 * Set this flag to indicate the build is triggered by eclipse.
	 * <p>
	 * <i>ignored for now but reserved for possible future use. </i>
	 */
	public final static int IS_ECLIPSE_BUILD = 4;
	/**
	 * don't care about project facts. neither generate project fact files, nore
	 * consult any existing project facts. Project facts that are already
	 * consulted will <b>not </b> be retracted.
	 */
	public final static int IGNORE_PRJ_FACTS = 8;
	/**
	 * don't care about external facts. neither generate external fact files,
	 * nore consult any existing extermal facts. External facts that are already
	 * consulted will <b>not </b> be retracted.
	 */
	public final static int IGNORE_EXT_FACTS = 16;

	private boolean loadedJavaFile =false;
	
	/**
	 * Default constructor.
	 * 
	 * typicaly, objects of this type are created lazily by the
	 * JLMPProjectNature.
	 */
	public FactBaseBuilder(IProject project,IPrologClient client) {
		this.project = project;
		
		//prologManager = null;
		try {
			prologClient = client;

			metaDataEXT = MetaDataManagerFactory.getPrologMetaDataManager(client, PrologMetaDataManager.EXT);
			metaDataPL = MetaDataManagerFactory.getPrologMetaDataManager(client, PrologMetaDataManager.PL);
			//JLMPProjectNature jlmpnat = (JLMPProjectNature) project.getNature(JLMPProjectNature.NATURE_ID);
//			metaDataEXT =jlmpnat.getMetaDataManager(PrologMetaDataManager.EXT);
//			metaDataPL = jlmpnat.getMetaDataManager(PrologMetaDataManager.PL);
			
		} catch (IOException e) {
			Debug.report(e);
			//uh-oh! problem! What to do here??
			ExceptionHandler.handle(e);
		} 
//			catch (CoreException ce){
//			ExceptionHandler.handle(ce);
//		}
		this.cuProvider = new CompilationUnitProvider();
	}
	
	/**
	 * Collects changed classes from the delta, and creates new facts for them. The generated facts
	 * are consulted into the Prolog System, and if necessary, binary-only type references are resolved. Once
	 * this method returns, the state of the prolog systems factbase is consistent to the state of the project.
	 * If this method is called with a null delta, it rebuilds all sourcefiles in the current JLMP project. If
	 * Monitor is null, the process will not supply progress information, but work nevertheless. Implementation
	 * may run the build process in a seperate job.
	 * 
	 * @param delta the delta containing the changed Resources
	 * @param flags a logical or of the constants defined in this class
	 * @param monitor a IProgressMonitor object
	 */
	
	public synchronized  void build(final IResourceDelta delta, final int flags, final IProgressMonitor monitor)  {
		loadedJavaFile = false;
		WorkspaceJob job = new WorkspaceJob("Buildjob") {
			public IStatus runInWorkspace(IProgressMonitor monitor)
					throws CoreException {
				
				//ld: we catch EVERYTHING, since eclipse eats any exceptions
				// otherwise :-(
				try {
					if (monitor == null)
						monitor = new NullProgressMonitor();
					
					build_impl(delta, flags, monitor);
				} catch (Throwable t) {
					if (t instanceof OperationCanceledException)
						return new Status(Status.CANCEL, PDTPlugin.PLUGIN_ID, Status.OK, "Operation Canceled", t);
						
					
					Debug.report(t);
					return new Status(Status.ERROR, PDTPlugin.PLUGIN_ID, Status.OK, 
							"Exception while rebuilding fact base", t);
				}
				
				if (failed.isEmpty()){
				    WorkbenchJob job = new WorkbenchJob("update factbase observers") {

                        public IStatus runInUIThread(IProgressMonitor monitor) {
                            if(!PDTPlugin.getDefault().updateFactbaseObservers(IJTransformerObserver.JT_FACTBASE_UPDATED,prologClient,project))
            					return new Status(Status.ERROR, PDTPlugin.PLUGIN_ID, 0, "Failed to update factbase observers", null);
                            return Status.OK_STATUS;
                        }
				        
				    };
				    job.schedule();
				    
					return Status.OK_STATUS;
				}
				else {
					
					PDTPlugin.getDefault().updateFactbaseObservers(IJTransformerObserver.JT_BUILD_ERROR,prologClient,project);
					String msg = "Failed to load some classes: ";
					for (Iterator iter = failed.iterator(); iter.hasNext();) {
					    String typename = iter.next().toString();
						msg += typename + "\n";
						
						prologClient.query("assert(ignore_unresolved_type('"+typename + "'))");
					}
					Debug.error(msg);
					return Status.OK_STATUS;
					//return new Status(Status.ERROR, PDTPlugin.PLUGIN_ID, 0, "Failed to load some classes", null);
				}
			}

            
		};
		
		job.schedule();
	}
	
	synchronized private void build_impl(IResourceDelta delta, int flags, IProgressMonitor monitor) {
		boolean delete;
		
		
		try {
			final Collection toProcess = new Vector();
			
			if (project == null) {
				Debug.warning("JLMPProjectBuilder called on null project. Aborted.");
				return;
			}
			
			Debug.info("Resource delta recieved: " + delta);
			
			
			if ((flags & CLEAR_PRJ_FACTS) != 0) {
				metaDataPL.deleteAll();
			}
			
			if ((flags & CLEAR_EXT_FACTS) != 0) {
				metaDataEXT.deleteAll();
			}			

			monitor.beginTask("Collecting Files to update", IProgressMonitor.UNKNOWN);
			
			if (delta == null) {
				delete = false;
				
				/* forces reload of all java files in the JLMP-Nature project. First we collect
				 * them all, then we build them one by one.
				 */
				
				project.accept(new IResourceVisitor() {

					public boolean visit(IResource resource) throws CoreException {
						
						Debug.debug("Visiting: " + resource);
						
						if (resource.getType() == IResource.ROOT)
							return true;
						if (resource.getType() == IResource.PROJECT)
							return resource.getProject().hasNature(JLMPProjectNature.NATURE_ID);
						/* since we only enter Projects that have the JLMP Nature set, this should
						 * be safe...
						 */
						if (resource.getType() == IResource.FOLDER)
							return true;
						if (resource.getType() == IResource.FILE)  {
							if (resource.getFileExtension() != null && resource.getFileExtension().equals("java")){
								Debug.debug("Adding " + resource + " to toProcess");
								toProcess.add(resource);
							}
						}
						return false;
					}
				});				
			} else {
				delete = true;
				delta.accept(new IResourceDeltaVisitor() {

					public boolean visit(IResourceDelta delta) throws CoreException {
						IResource resource = delta.getResource();
						
						if (resource.getType() == IResource.ROOT)
							return true;
						if (resource.getType() == IResource.PROJECT)
							return resource.getProject().hasNature(JLMPProjectNature.NATURE_ID);
						/* since we only enter Projects that have the JLMP Nature set, this should
						 * be safe...
						 */
						if (resource.getType() == IResource.FOLDER)
							return true;
						if (resource.getType() == IResource.FILE){
							String fext = resource.getFileExtension();
							if (fext != null && fext.equals("java")){
								Debug.debug("Adding " + resource + " to toProcess");
								toProcess.add(resource);
							}
						}
							
						return false;
					}
				});

			}
			
			monitor.done();
						
			if ((flags & IGNORE_PRJ_FACTS) == 0){			
				
			
				for (Iterator i = toProcess.iterator(); i.hasNext(); ){	
					
					if (monitor.isCanceled())
						throw new OperationCanceledException("Canceled");

					IFile file = (IFile) i.next();
					
					monitor.beginTask("Generating Facts for source file " + file, IProgressMonitor.UNKNOWN);
				
					forgetFacts(file, delete);
					buildFacts(file);
					
					monitor.done();
				}
			}
			
			monitor.done();
			
			if ((flags & IGNORE_EXT_FACTS) == 0 && loadedJavaFile){ 
						loadExternalFacts(monitor);
			}
			
			//ld:  this should be finer grained!
			//StS: Nope, it is ok, since there can be changes all over pl. and ext
			
			//JT-66 FIX: removed the following line
			//ResourcesPlugin.getWorkspace().getRoot().refreshLocal(IResource.DEPTH_INFINITE, null);
		} catch (CoreException e) {
			Debug.report(e);
		} catch (IOException e) {
			Debug.report(e);
		}		
	}
	
	private void forgetFacts(IFile file, boolean delete) throws IOException {
		Debug.debug("Forgetting (possible) previous version of " + file);
		String path = file.getFullPath().toString();
				
		/* if there is no tree with that id, the retraction fails, but no harm done */
		
		prologClient.query("rollback, toplevelT(ID, _, '" + path +	"', _), deepRetract(ID)");
		if (delete) {
			Debug.debug("Deleting resource " + path);
			metaDataPL.delete(path);
		}
	}
	
	private void buildFacts(IFile resource) throws IOException, CoreException {
	    loadedJavaFile = true;
		if (!resource.exists())
			/* the file seems to have been deleted */
			return;
		
		ICompilationUnit icu = cuProvider.createICompilationUnit(resource);
		CompilationUnit cu = null;
		
		try {
			icu.becomeWorkingCopy(null, null);
		} catch (JavaModelException jme){
			// if this exception occurs, the java file is in some way bad. Thats ok,
			// since it is also dead (deleted) in the Prolog System.
			ExceptionHandler.handle(jme);
			return;
		}
		
		
		
		String path = resource.getFullPath().toString();
		
		if (metaDataPL.getUpdateTime(path) >= resource.getModificationStamp()){
			Debug.debug("Using old version of " + path);
			metaDataPL.consult(path);
			return;
		} else 
			Debug.debug("Rebuilding " + path + " from scratch");
		
		PrependablePrologWriter plw = metaDataPL.getPrependablePrologWriter(path);
		FactGenerationToolBox box = new DefaultGenerationToolbox(prologClient);
		FactGenerator visitor = new FactGenerator(icu, resource.getFullPath().toString(), box, plw);
		
		ASTParser parser = ASTParser.newParser(AST.JLS2);
		parser.setSource(icu);
		parser.setResolveBindings(true);
		cu = (CompilationUnit) parser.createAST(null);
		
		cu.accept(visitor);
		
		plw.prependActions(box.getFQNTranslator().getFQNMapping());
		plw.writeQuery("retractLocalSymtab");
		
		plw.flush();
		plw.close();
		
		metaDataPL.consult(path);
	}
	
	protected void loadExternalFacts(IProgressMonitor submon) throws IOException, CoreException {		
		String next;
		
		while (true){
			next = getNext(submon);
			
			if (next == null)
				break;
			
			if (submon.isCanceled())
				throw new OperationCanceledException("Canceled");
					
			String nextFile = next.replace('.', '/');
			submon.beginTask("Loading fact file for " + next, IProgressMonitor.UNKNOWN);
			
			if (metaDataEXT.consult(nextFile)){
				//ld: check wether if the type is REALLY known.
				//     the file could have been empty or similar.
				
				/* StS: Fixed the Logic of your test. */
				
				if(prologClient.query("unresolved_types('"+next+"')") != null){
					Debug.info("Failed to load external type " + next);
					
					metaDataEXT.delete(nextFile);
					Debug.debug("Deleted faulty fact file");
				} else {
					Debug.debug("Loaded external type from old facts: " + next);
					submon.done();
					continue;
				}
			}
			
			PrependablePrologWriter pwriter = metaDataEXT.getPrependablePrologWriter(nextFile);
			FactGenerationToolBox box = new DefaultGenerationToolbox(prologClient);
			
			try {
				ByteCodeFactGeneratorIType bcfg = new ByteCodeFactGeneratorIType(project, pwriter, next, box);
				bcfg.writeAllFacts();
			} catch (ClassNotFoundException cnfe){
				Debug.report(cnfe);
				failed.add(next);
				continue;
			}
			
			pwriter.prependActions(box.getFQNTranslator().getFQNMapping());
			pwriter.writeQuery("retractLocalSymtab");
			
			pwriter.flush();
			pwriter.close();
			
			if (metaDataEXT.consult(nextFile) == false)
				failed.add(nextFile);
			
			Debug.debug("Created facts for " + next);
			submon.done();
		}
	}
	
	private String getNext(IProgressMonitor mon) throws IOException {
		/*
		 * Why use this method? Because we do not need all unresolved types, but only
		 * the next. So we save a lot of overhead by only asking for the first, and dropping
		 * it if it is known to fail. (Otherwise, we'd still be going through it all the time,
		 * with nothing gained
		 */
					
		if (nextIterator == null || !nextIterator.hasNext()){
			Hashtable answer;
			LinkedList l = new LinkedList();
			
			mon.beginTask("Fetching new unresolved types", IProgressMonitor.UNKNOWN);
			
			answer = prologClient.query("unresolved_types(X)");

			while (answer != null){
				if (mon.isCanceled())
					throw new OperationCanceledException();
				
				String toAdd = answer.get("X").toString();
				
				if (!failed.contains(toAdd)){
					l.add(toAdd);
					Debug.debug("Added " + toAdd + 
						" to loadlist");
				}
				
				answer = prologClient.next();
			}
			
			mon.done();
			
			nextIterator = l.iterator();
		}
		
		try {
			return nextIterator.next().toString();
		} catch (NoSuchElementException e){
			return null;
		}
		
	}
}
