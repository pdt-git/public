package org.cs3.jtransformer.internal.builders;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Vector;

import org.cs3.jtransformer.JTransformer;
import org.cs3.jtransformer.JTransformerPlugin;
import org.cs3.jtransformer.JTransformerProject;
import org.cs3.jtransformer.JTransformerProjectEvent;
import org.cs3.jtransformer.JTransformerProjectListener;
import org.cs3.jtransformer.internal.astvisitor.DefaultGenerationToolbox;
import org.cs3.jtransformer.internal.astvisitor.FactGenerationToolBox;
import org.cs3.jtransformer.internal.astvisitor.FactGenerator;
import org.cs3.jtransformer.internal.astvisitor.PrologWriter;
import org.cs3.jtransformer.internal.bytecode.ByteCodeFactGeneratorIType;
import org.cs3.jtransformer.util.JTUtils;
import org.cs3.pl.common.Debug;
import org.cs3.pl.prolog.AsyncPrologSession;
import org.cs3.pl.prolog.DefaultAsyncPrologSessionListener;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologInterface2;
import org.cs3.pl.prolog.PrologInterfaceException;
import org.cs3.pl.prolog.PrologSession;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceDelta;
import org.eclipse.core.resources.IResourceDeltaVisitor;
import org.eclipse.core.resources.IResourceStatus;
import org.eclipse.core.resources.IResourceVisitor;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.eclipse.jdt.core.IClasspathEntry;
import org.eclipse.jdt.core.ICompilationUnit;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.IType;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.jdt.core.dom.AST;
import org.eclipse.jdt.core.dom.ASTParser;
import org.eclipse.jdt.core.dom.CompilationUnit;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.ui.texteditor.MarkerUtilities;

public class FactBaseBuilder {

    /**
     * Set this flag to indicate the build is triggered by eclipse.
     * <p>
     * <i>ignored for now but reserved for possible future use. </i>
     */
    public final static int IS_ECLIPSE_BUILD = 4;

	private static final Object TICKET = "TICKET";

    private JTransformerProject jtransformerProject;

    private IProject project;

//    private PrologInterface pif;

    private Vector listeners = new Vector();

    private boolean building = false;

    private long storeTimeStamp;
    
    /**
     *  
     */
    public FactBaseBuilder(JTransformerProject jtransformerProject) {
        this.jtransformerProject = jtransformerProject;
        this.project = jtransformerProject.getProject();
//        this.setPif(jtransformerProject.getPrologInterface());
    }

    /**
     * Collects changed classes from the delta, and creates new facts for them.
     * The generated facts are consulted into the Prolog System, and if
     * necessary, binary-only type references are resolved. Once this method
     * returns, the state of the prolog systems factbase is consistent to the
     * state of the project. If this method is called with a null delta, it
     * rebuilds all sourcefiles in the current JTransformer project. If Monitor is null,
     * the process will not supply progress information, but work nevertheless.
     * Implementation may run the build process in a seperate job.
     * 
     * @param delta
     *                the delta containing the changed Resources, or null to
     *                indicate a full rebuild
     * @param flags
     *                a logical or of the constants defined in this class
     * @param monitor
     *                a IProgressMonitor object
     * @throws CoreException
     */

    public synchronized void build(final IResourceDelta delta, final int flags,
            IProgressMonitor monitor) throws CoreException {
    	// FIXME: Expensive?
    	
        try {
   			JTransformerPlugin.getDefault().setNonPersistantPreferenceValue(project,JTransformer.FACTBASE_STATE_KEY, JTransformer.FACTBASE_STATE_IN_PROCESS);
        	JTUtils.clearAllMarkersWithJTransformerFlag(project);

            if (building) {
                Debug.warning("skipping build");
                return;
            } else {
                Debug.warning("doing build");
            }
            building = true;
            if (monitor == null) {
                monitor = new NullProgressMonitor();
            }
			//jobManager.beginRule(JTransformer.JTransformer_BUILDER_SCHEDULING_RULE,monitor);
            build_impl(delta, flags, monitor);

        } catch (OperationCanceledException e) {
            throw e;
        } catch (Throwable t) {
			HashMap attributes = new HashMap();

			attributes.put(IMarker.MESSAGE, "Could not create PEFs for project: " + project.getName() + " - " +t.getLocalizedMessage());
			attributes.put(IMarker.SEVERITY, new Integer(IMarker.SEVERITY_ERROR));
			attributes.put(IMarker.LOCATION, project.getLocation());
//			attributes.put(IMarker.LINE_NUMBER, new Integer(line));

//			// attributes.put(IMarker.CHAR_START, new Integer(startOffset));
			// attributes.put(IMarker.CHAR_END, new Integer(startOffset + 1));
			attributes.put(JTransformer.PROBLEM_MARKER_ID, "1");

			MarkerUtilities.createMarker(project, attributes, IMarker.PROBLEM);
        	
            //IMarker marker = project.createMarker(IMarker.SEVERITY_ERROR);//JTransformer.PROBLEM_MARKER_ID);
//            marker.setAttribute(IMarker.MESSAGE,
//                    "Could not create PEFs for Project.");
            Debug.report(t);

        } finally {
			//jobManager.endRule(JTransformer.JTransformer_BUILDER_SCHEDULING_RULE);

            building = false;
            monitor.done();
            fireFactBaseUpdated();
        }
    }


    
    synchronized private void build_impl(IResourceDelta delta, int flags,
            IProgressMonitor monitor) throws IOException, CoreException, PrologInterfaceException {
        PrologSession session = null;
        storeTimeStamp = -1;
        try {

            // ld: if pif is not currently up, this is no problem:
            // the reload hook will trigger another build in its afterInit
            // method.
            // Question is wether we should throw some exception, or
            // just quietly return? XXX: simple return for now.
            if (!getPif().isUp()) {
                return;
            }

//            getMetaDataEXT();
//            getMetaDataSRC();

            monitor.beginTask(project.getName() + " - building PEFs", 100);
            session = getPif().getSession();
            final Collection toProcess = new Vector();
            final Collection toDelete = new Vector();

            session.queryOnce("retractall(errors_in_java_code)");

            Debug.info("Resource delta recieved: " + delta);
            SubProgressMonitor submon = new SubProgressMonitor(monitor, 10,
                    SubProgressMonitor.PREPEND_MAIN_LABEL_TO_SUBTASK);
            submon.beginTask("Collecting Files", IProgressMonitor.UNKNOWN);
            project.refreshLocal(IResource.DEPTH_INFINITE, null);
            if (delta == null) {
                //session.queryOnce("delete_source_facts");
                collectAll(toProcess);
            } else {
                collectDelta(delta, toProcess, toDelete);
            }
            submon.done();

            submon = new SubProgressMonitor(monitor, 10,
                    SubProgressMonitor.PREPEND_MAIN_LABEL_TO_SUBTASK);
            forgetFacts(submon, toDelete,session);

            submon = new SubProgressMonitor(monitor, 40,
                    SubProgressMonitor.PREPEND_MAIN_LABEL_TO_SUBTASK);
             buildFacts(submon, toProcess);
            
            submon = new SubProgressMonitor(monitor, 40,
                    SubProgressMonitor.PREPEND_MAIN_LABEL_TO_SUBTASK);
            loadExternalFacts(submon);
   			JTransformerPlugin.getDefault().setNonPersistantPreferenceValue(project,JTransformer.FACTBASE_STATE_KEY, JTransformer.FACTBASE_STATE_READY);

//   			JTransformerPlugin plugin = JTransformerPlugin.getDefault();
//            String v = plugin.getPreferenceValue(JTransformer.PREF_USE_PEF_STORE, "false");
//            if (Boolean.valueOf(v).booleanValue()) {
//                JTransformerPlugin.getDefault().save(session);
//            }
        } finally {
            if (session != null) {
                session.dispose();
            }
        }
    }

    private void forgetFacts(IProgressMonitor monitor, final Collection toDelete,PrologSession session)
            throws PrologInterfaceException {
        monitor.beginTask(project.getName() + " - deleting obsolete PEFs", toDelete.size());
        for (Iterator i = toDelete.iterator(); i.hasNext();) {
            if (monitor.isCanceled()) {
                throw new OperationCanceledException("Canceled");
            }
            IFile file = (IFile) i.next();
            forgetFacts(file, true,session);
            monitor.worked(1);
        }
        monitor.done();
    }

    private void buildFacts(IProgressMonitor monitor, final Collection toProcess)
            throws IOException, CoreException, PrologInterfaceException {
        monitor.beginTask(project.getName() + " - generating new PEFs", toProcess.size());
        AsyncPrologSession asyncSession = ((PrologInterface2)getPif()).getAsyncSession();
        asyncSession.addBatchListener(new DefaultAsyncPrologSessionListener());
        PrologSession syncSession = getPif().getSession(); 
        try {
	        for (Iterator i = toProcess.iterator(); i.hasNext();) {
	            if (monitor.isCanceled()) {
	                throw new OperationCanceledException("Canceled");
	            }
	            IFile file = (IFile) i.next();
	            forgetFacts(file, false,syncSession);
	            monitor.subTask(" - processing " + file.getName());
	            buildFacts(asyncSession, file);
	            monitor.worked(1);
	        }
        } finally {
            monitor.subTask(" - write source file facts");
        	
        	if(asyncSession != null)
        		asyncSession.dispose();
        	if(syncSession != null)
        		syncSession.dispose();
        	monitor.done();
        }
	        
    }

    private void collectDelta(IResourceDelta delta, final Collection toProcess,
            final Collection toDelete) throws CoreException {
        /*
         * INCREMENTAL_BUILD
         */
        final IJavaProject javaProject = (IJavaProject) project
                .getNature(JavaCore.NATURE_ID);
        delta.accept(new IResourceDeltaVisitor() {

            public boolean visit(IResourceDelta delta) throws CoreException {
                IResource resource = delta.getResource();

                if (resource.getType() == IResource.ROOT) {
                    return true;
                }
                if (resource.getType() == IResource.PROJECT) {
                    return resource.getProject().hasNature(JTransformer.NATURE_ID);
                }
                if (resource.getType() == IResource.FOLDER) {
                    return javaProject.isOnClasspath(resource);
                }
                if (resource.getType() == IResource.FILE) {
                    String fext = resource.getFileExtension();
                    if (fext != null && fext.equals("java")) {
						
						if(!inExclusionPattern(resource))
                        switch (delta.getKind()) {
                        case IResourceDelta.REMOVED:
                            toDelete.add(resource);
                            break;
                        case IResourceDelta.CHANGED:
                            if (!isStoreUpToDate((IFile) (resource))) { // added,...???
                                Debug.debug("Adding " + resource
                                        + " to toProcess");
									toProcess.add(resource);
                            }
                            break;
                        default://added, moved, etc

							toProcess.add(resource);
                            break;
                        }

                    }
                }

                return false;
            }
        });
    }

    private void collectAll(final Collection toProcess) throws CoreException {
        /*
         * FULL_BUILD
         */
        final IJavaProject javaProject = (IJavaProject) project
                .getNature(JavaCore.NATURE_ID);

        project.accept(new IResourceVisitor() {

            public boolean visit(IResource resource) throws CoreException {

                Debug.debug("Visiting: " + resource);

                if (resource.getType() == IResource.ROOT)
                    return true;
                if (resource.getType() == IResource.PROJECT)
                    return resource.getProject().hasNature(JTransformer.NATURE_ID);
                /*
                 * since we only enter Projects that have the JTransformer Nature set,
                 * this should be safe...
                 */

                if (resource.getType() == IResource.FOLDER) {
                    return javaProject.isOnClasspath(resource);
                }
                if (resource.getType() == IResource.FILE) {
                    if (resource.getFileExtension() != null
                            && resource.getFileExtension().equals("java")
                            && !isStoreUpToDate((IFile) resource)) {
                        Debug.debug("Adding " + resource + " to toProcess");
						
						if(!inExclusionPattern(resource))
							toProcess.add(resource);
                    }
                }
                return false;
            }
        });
    }

    private void forgetFacts(IFile file, boolean removeGlobalIdsFacts, PrologSession session)
            throws PrologInterfaceException {
        Debug.debug("Forgetting (possible) previous version of " + file);

        String path = file.getFullPath().toString();
        // ld: an unconsult is not enough due to the multifile-ness of the
        // predicates in question.
        /*
         * if there is no tree with that id, the retraction fails, but no harm
         * done
         */

            if (removeGlobalIdsFacts)
                session
                        .queryOnce("remove_contained_global_ids('" + path
                                + "')");


            String retractKind;
            if(removeGlobalIdsFacts) { //delete finally, a delete_file(File) fact will be added
            	retractKind = "deepRetractToplevel(Toplevel)";

            } else {
            	retractKind = "deepRetract(Toplevel)";
            }
            
            Map ret = session.queryOnce("toplevelT(Toplevel, _,'" + path +  "', _)," + retractKind);	

            if( ret == null) { // TRHO: TODO: When does this happen?
	            IJavaProject javaProject;
				try {
					javaProject = (IJavaProject) project.getNature(JavaCore.NATURE_ID);
		            ICompilationUnit compUnit = (ICompilationUnit)javaProject.findElement(file.getProjectRelativePath());
		            if(compUnit != null) {
			            IType[] types = compUnit.getAllTypes();
			            for (int i = 0; i < types.length; i++) {
			                session
		                    .queryOnce("globalIds(" +
		                    		"'"+ types[i].getFullyQualifiedName('.')+ "'" +
		                    		", CID), externT(CID), deepRetractToplevel(CID)");
						}
		            }
				} catch (CoreException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
            }
    }

    /**
     * 
     * @param file
     * @return false if the building failed
     * @throws IOException
     * @throws CoreException
     */
    private boolean isStoreUpToDate(IFile file) {
        long recordTS = getStoreTimeStamp();
        long fileTS = file.getLocalTimeStamp();
        Debug.debug("store ts: " + recordTS);
        Debug.debug("file ts: " + fileTS);
        // ld:no need to create facts that are already known
        /*
         * FIXME: currently we only ensure that the record is up-to-date. in
         * theory, so should be the consulted facts, but it might be better to
         * check. otoh, this would be quite expensive if there are many
         * resources to check.
         * 
         * 
         * ld: note that the up-to-date check HAS to be pessimistic if both time
         * stamps are equal - this is a more commen the case as one might think -
         * e.g. on my system, time stamps are as coarse grained as one whole
         * second, which is literaly nothing during test runs.
         */
        if (recordTS > fileTS) {
            Debug.debug("store is up to date");
            return true;
        }
        Debug.debug("store is outdated");
        return false;
    }

    /**
     * @return
     */
    private long getStoreTimeStamp() {
        JTransformerPlugin plugin = JTransformerPlugin.getDefault();
        String v = plugin.getPreferenceValue(JTransformer.PREF_USE_PEF_STORE, "false");
        if (!Boolean.valueOf(v).booleanValue()) {
            return -1;
        }
        if (storeTimeStamp == -1) {
            String filename = jtransformerProject.getPreferenceValue(
			        JTransformer.PROP_PEF_STORE_FILE, "");
			File file = new File(filename);
			if (file.canRead()) {
			    storeTimeStamp = file.lastModified();
			} else {
			    storeTimeStamp = -1;
			}

        }

        return storeTimeStamp;
    }

    private void buildFacts(AsyncPrologSession session, IFile file) throws IOException, CoreException, PrologInterfaceException {

        /* the file seems to have been deleted */
        if (!file.exists()) {
            return;
        }
//        ConsultService cs = getMetaDataSRC();
        String recordPath = file.getFullPath().addFileExtension("pl")
                .toString();

        ICompilationUnit icu = JavaCore.createCompilationUnitFrom(file);

        //PrintStream out = cs.getOutputStream(recordPath);
        try {

            icu.becomeWorkingCopy(null, null);

            writeFacts(session, icu);
        } catch (JavaModelException jme) {
            IMarker marker = file.createMarker(JTransformer.PROBLEM_MARKER_ID);
            marker.setAttribute(IMarker.MESSAGE,
                    "There seem to be problems in the java code.");
            session.queryOnce(TICKET,"assert(errors_in_java_code)");
            throw new CoreException(new Status(IStatus.CANCEL, JTransformer.PLUGIN_ID,
                    IResourceStatus.BUILD_FAILED,
                    "build canceled due to problems in the java code.", jme));
        } finally {
            icu.discardWorkingCopy();
        }
    }

    public List getUnresolvedTypes() throws PrologInterfaceException {
        PrologSession session = getPif().getSession();
        try {
            return getUnresolvedTypes(session, new HashSet());
        } finally {
            session.dispose();
        }
    }

    protected List getUnresolvedTypes(PrologSession session, HashSet failed) throws PrologInterfaceException {
        List list = session.queryAll("unresolved_types(T)");
        List typeNames = new Vector();
        for (Iterator iter = list.iterator(); iter.hasNext();) {
            Map m = (Map) iter.next();
            String typeName = (String) m.get("T");
            if (typeName.startsWith("'")) {
                typeName = typeName.substring(1, typeName.length() - 1);
            }
            if (!failed.contains(typeName)) {
                typeNames.add(typeName);
            }
        }
        return typeNames;
    }

 public void loadExternalFacts(IProgressMonitor monitor) throws IOException,
            CoreException, PrologInterfaceException {
        Debug.debug("enter loadExternalFacts");
        monitor.beginTask(project.getName() + " - creating external PEFs", IProgressMonitor.UNKNOWN);
        HashSet failed = new HashSet();
        PrologSession session = getPif().getSession();
		AsyncPrologSession asyncSession = ((PrologInterface2)getPif()).getAsyncSession();
        try {
            List unresolved = getUnresolvedTypes(session, failed);

            Set seen = new HashSet();
            while (!unresolved.isEmpty()) {

                    for (Iterator iter = unresolved.iterator(); iter.hasNext();) {
                        String typeName = (String) iter.next();
//                        if( typeName.equals("java.lang.Class")) {
//                        	System.err.println("DEBUG");
//                        }
                        if (seen.contains(typeName)) {
                            throw new RuntimeException(
                                    "saw type before, so something seems broken. "
                                            + typeName);
                        }
                        seen.add(typeName);
                        try {
                            SubProgressMonitor submon = new SubProgressMonitor(
                                    monitor,
                                    IProgressMonitor.UNKNOWN,
                                    SubProgressMonitor.PREPEND_MAIN_LABEL_TO_SUBTASK);
                            submon.subTask(" - processing " + typeName);
                            //submon.subTask(" - processing " + typeName);
                            if (submon.isCanceled()) {
                                throw new OperationCanceledException();
                            }
                            writeFacts(asyncSession, typeName);
                            submon.done();
                        } catch (ClassNotFoundException e) {
                            failed.add(typeName);
                        }
                    }
//                System.err.println("BEFORE DISPOSE");
                asyncSession.join();
//                System.err.println("BEFORE DISPOSE");
                unresolved = getUnresolvedTypes(session, failed);
            }
        } finally {
            session.dispose();
            asyncSession.dispose();
            monitor.done();
            Debug.debug("exit loadExternalFacts");
        }
    }

    public void writeFacts(AsyncPrologSession session, IProject project, final ICompilationUnit icu) throws IOException, CoreException, PrologInterfaceException {

        IResource resource = icu.getResource();
        String path = resource.getFullPath().removeFileExtension()
                .addFileExtension("pl").toString();
        
        List clauses  = new ArrayList();
        PrologWriter plw = new PrologWriter(clauses, true);// metaDataSRC.getPrependablePrologWriter(path);
        FactGenerationToolBox box = new DefaultGenerationToolbox();
        FactGenerator visitor = new FactGenerator(icu, resource.getFullPath()
                .toString(), box, plw);

        ASTParser parser = getJavaParser();

        parser.setSource(icu);
        parser.setResolveBindings(true);
		final CompilationUnit cu = (CompilationUnit) parser.createAST(null);
		try {
			cu.accept(visitor);
		} catch(IllegalArgumentException iae){
			JTransformerPlugin.getDefault().getWorkbench().getDisplay().asyncExec(new Runnable() {
				public void run() {
					MessageDialog.openError(JTransformerPlugin.getDefault().getWorkbench().getActiveWorkbenchWindow().getShell(),
							"Build Error", "Could not update factbase, compile (?) error in file: "+icu.getResource().getFullPath()+". " +
					 				"\nIf the class name equals the package name you found an Eclipse bug.");

				}
			});
			throw iae;
		}
		
        writeSymtabAndClauses(session, project, clauses, plw, box);
    }
    
// PrologSession session
	private void assertClauses(AsyncPrologSession session, List clauses) throws PrologInterfaceException
	{
		if (clauses.size() == 0)
		{
			return;
		}

		// final PrologSession as = prologInterface.getSession();

		// monitor.beginTask(taskname, size);
		// as.addBatchListener(new DefaultAsyncPrologSessionListener(){
		// public void goalSucceeded(AsyncPrologSessionEvent e) {
		// monitor.worked(1);
		// if(monitor.isCanceled()){
		// new Thread(){
		// public void run() {
		// as.abort();
		// Debug.debug("JTransformer builder aborted ");
		// }
		// }.start();
		//					
		// }
		// }
		// });
		StringBuffer buf = new StringBuffer();
		//		"inTeArray(term(" + 

		session.queryOnce( TICKET, "retractLocalSymtab");

		int num = 0;
		for (Iterator iter = clauses.iterator(); iter.hasNext();)
		{
			if (buf.length() > 0)
			{
				buf.append(",");
			}
			buf.append(iter.next());
			num++;
			if(num == 1000) {
				String out = buf.toString();
				session.queryOnce( TICKET, out);
				buf = new StringBuffer();
				num = 0;
			}

		}
		//writeToFile(buf.toString());
		if(buf.length() > 0) {
			session.queryOnce( TICKET, buf.toString());
		}
		session.queryOnce( TICKET, "retractLocalSymtab");

	}

	private void writeToFile(String str)
	{
		try
		{
			BufferedWriter writer = new BufferedWriter(new FileWriter("c:\\temp\\asq.pl",true));
			writer.write(str);
			writer.write("\n");
			writer.close();
		} catch (IOException e)
		{
			e.printStackTrace();
		}
	}

	private static ASTParser getJavaParser()
	{
		ASTParser parser = ASTParser.newParser(AST.JLS3);
        //setJavaSourceVersion(parser);

		return parser;
	}

	private static void setJavaSourceVersion(ASTParser parser)
	{
		Map options = new HashMap();
        options.put("org.eclipse.jdt.core.compiler.compliance" /* "Source Compatibility Mode" */,
    		    JavaCore.VERSION_1_5);
        options.put("org.eclipse.jdt.core.compiler.source" /* "Source Compatibility Mode" */,
    		    JavaCore.VERSION_1_5);
        parser.setCompilerOptions(options);
	}

    public void writeFacts(AsyncPrologSession session, IProject project, String typeName) throws JavaModelException, CoreException,
            ClassNotFoundException, PrologInterfaceException {
        List clauses = new ArrayList();
        PrologWriter plw = new PrologWriter(clauses, true);
        	
        FactGenerationToolBox box = new DefaultGenerationToolbox();
        new ByteCodeFactGeneratorIType(project, plw, typeName, box)
                .writeAllFacts();
        writeSymtabAndClauses(session, project, clauses, plw, box);
    }

	private void writeSymtabAndClauses(AsyncPrologSession session, IProject project, List clauses, PrologWriter plw, FactGenerationToolBox box) throws CoreException, PrologInterfaceException
	{
		plw.writeQuery("retractLocalSymtab");
        
        List symtab  = new ArrayList();
        PrologWriter plwSymtab = new PrologWriter(symtab, true);
        Map mapping = box.getFQNTranslator().getFQNMapping();
        writeSymTab(plwSymtab, mapping);
//      System.err.println("\n--------------------\n\n"+path+"\n\n------------------\n");
//      System.err.println(header + "\n\n");
//      System.err.println(data + "\n\n");
//		writeToFile("% write file ");
		symtab.addAll(clauses);
		assertClauses(session,symtab);
//		writeToFile("% before join ");
      //as.join();
//		writeToFile("% after join ");
      
	}

    private static void writeSymTab(PrologWriter plw, Map mapping) {
        Set set = mapping.keySet();
        boolean temp = plw.getInterpretMode();
        plw.setInterpretMode(false);
        try {
            for (Iterator it = set.iterator(); it.hasNext();) {

                String fqn = (String) it.next();
                String id = (String) mapping.get(fqn);
                plw.writeFact("local2FQN", new String[] { id, fqn });
            }
        } finally {
            plw.setInterpretMode(temp);
        }
    }

    /**
     * generate prolog element facts for a given compilation unit.
     * <p>
     * More than a convenience method, this should become <b>The Way(tm) </b> to
     * generate prolog representation of java source code on the facade level.
     * 
     * @param an
     *                compilation unit in working copy mode.
     * @throws PrologInterfaceException 
     */
    public void writeFacts(AsyncPrologSession session, ICompilationUnit icu)
            throws IOException, CoreException, PrologInterfaceException {
        writeFacts(session, project, icu);
    }

    public void writeFacts(AsyncPrologSession session, String typeName)
            throws JavaModelException, CoreException, ClassNotFoundException, PrologInterfaceException {
        writeFacts(session, project, typeName);
    }


    /**
     * @param monitor
     * @throws CoreException
     * @throws PrologInterfaceException 
     */
    public void clean(IProgressMonitor monitor) throws CoreException, PrologInterfaceException {
        Debug.info("clean called on project " + project);
        project.deleteMarkers(JTransformer.PROBLEM_MARKER_ID, true,
                IResource.DEPTH_INFINITE);
        PrologSession session = getPif().getSession();
        try {
            session.queryOnce("clearTreeFactbase('" + project.getName() + "')");
            // getMetaDataSRC().clearRecords();
            String storeName = jtransformerProject.getPreferenceValue(
                    JTransformer.PROP_PEF_STORE_FILE, null);
            new File(storeName).delete();
            storeTimeStamp = -1;
            if (monitor != null) {
                monitor.done();
            }
        } catch(Throwable t) {
        	Debug.error(t.getLocalizedMessage());
        	Debug.dumpStackTrace();
        	Debug.rethrow(t);
        } finally {
            session.dispose();
        } 
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.jtransformer.JTransformerProject#addJTransformerProjectListener(org.cs3.jtransformer.JTransformerProjectListener)
     */
    public void addJTransformerProjectListener(JTransformerProjectListener l) {
        synchronized (listeners) {
            if (!listeners.contains(l)) {
                listeners.add(l);
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.jtransformer.JTransformerProject#removeJTransformerProjectListener(org.cs3.jtransformer.JTransformerProjectListener)
     */
    public void removeJTransformerProjectListener(JTransformerProjectListener l) {
        synchronized (listeners) {
            if (listeners.contains(l)) {
                listeners.remove(l);
            }
        }
    }

    protected void fireFactBaseUpdated() {
        JTransformerProjectEvent e = new JTransformerProjectEvent(this);
        Vector cloned = null;
        synchronized (listeners) {
            cloned = (Vector) listeners.clone();
        }
        for (Iterator it = cloned.iterator(); it.hasNext();) {
            JTransformerProjectListener l = (JTransformerProjectListener) it.next();
            l.factBaseUpdated(e);
        }

    }

	/**
	 * JT-146
     * FIXME: Extend this check to packages / patterns(?).
	 * @param resource
	 * @throws CoreException
	 * @throws JavaModelException
	 */
	private boolean inExclusionPattern(IResource resource) throws CoreException, JavaModelException {
		final IJavaProject javaProject = (IJavaProject) project
		.getNature(JavaCore.NATURE_ID);
		IClasspathEntry[] rawcp = javaProject.getRawClasspath();
		IFile file = (IFile)resource;
		for (int i = 0; i < rawcp.length; i++) {
			IPath[] ep = rawcp[i].getExclusionPatterns();
			IPath path = file.getFullPath();
			for (int j = 0; j < ep.length; j++) {
				IPath epPath =rawcp[i].getPath().append(ep[j]);
				if (epPath.equals(path)) 
					return true;
			}
		}
		return false;
	}


	/**
	 * 
	 * @return
	 */
	private PrologInterface getPif() {
		try {
			return JTransformerPlugin.getNature(project).getPrologInterface();
		} catch (CoreException e) {
			Debug.report(e);
		}
		throw new RuntimeException("Prolog Interface ca not be resolved.");
	}

}
