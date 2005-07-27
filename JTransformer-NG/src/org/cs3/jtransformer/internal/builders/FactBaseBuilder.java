package org.cs3.jtransformer.internal.builders;

import java.io.File;
import java.io.IOException;
import java.io.PrintStream;
import java.io.StringWriter;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Vector;

import org.cs3.jtransformer.JTransformer;
import org.cs3.jtransformer.JTransformerPlugin;
import org.cs3.jtransformer.JTransformerProject;
import org.cs3.jtransformer.JTransformerProjcetEvent;
import org.cs3.jtransformer.JTransformerProjectListener;
import org.cs3.jtransformer.internal.astvisitor.DefaultGenerationToolbox;
import org.cs3.jtransformer.internal.astvisitor.FactGenerationToolBox;
import org.cs3.jtransformer.internal.astvisitor.FactGenerator;
import org.cs3.jtransformer.internal.astvisitor.PrologWriter;
import org.cs3.jtransformer.internal.bytecode.ByteCodeFactGeneratorIType;
import org.cs3.pl.common.Debug;
import org.cs3.pl.prolog.ConsultService;
import org.cs3.pl.prolog.PrologInterface;
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
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.eclipse.core.runtime.jobs.IJobManager;
import org.eclipse.jdt.core.IClasspathEntry;
import org.eclipse.jdt.core.ICompilationUnit;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.jdt.core.dom.AST;
import org.eclipse.jdt.core.dom.ASTParser;
import org.eclipse.jdt.core.dom.CompilationUnit;
import org.eclipse.jface.dialogs.MessageDialog;

public class FactBaseBuilder {

    /**
     * Set this flag to indicate the build is triggered by eclipse.
     * <p>
     * <i>ignored for now but reserved for possible future use. </i>
     */
    public final static int IS_ECLIPSE_BUILD = 4;

    private JTransformerProject jtransformerProject;

    private IProject project;

    private PrologInterface pif;

    private Vector listeners = new Vector();

    private boolean building = false;

    private long storeTimeStamp;

    /**
     *  
     */
    public FactBaseBuilder(JTransformerProject jtransformerProject) {
        this.jtransformerProject = jtransformerProject;
        this.project = jtransformerProject.getProject();
        this.pif = jtransformerProject.getPrologInterface();
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
		IJobManager jobManager = Platform.getJobManager();
        try {
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
            IMarker marker = project.createMarker(JTransformer.PROBLEM_MARKER_ID);
            marker.setAttribute(IMarker.MESSAGE,
                    "Could not create PEFs for Project.");
            Debug.report(t);
        } finally {
			//jobManager.endRule(JTransformer.JTransformer_BUILDER_SCHEDULING_RULE);
            building = false;
            monitor.done();
            fireFactBaseUpdated();
        }
    }

    synchronized private void build_impl(IResourceDelta delta, int flags,
            IProgressMonitor monitor) throws IOException, CoreException {
        PrologSession session = null;
        storeTimeStamp = -1;
        try {

            // ld: if pif is not currently up, this is no problem:
            // the reload hook will trigger another build in its afterInit
            // method.
            // Question is wether we should throw some exception, or
            // just quietly return? XXX: simple return for now.
            if (!pif.isUp()) {
                return;
            }

            getMetaDataEXT();
            getMetaDataSRC();

            monitor.beginTask("building PEFs", 100);
            session = pif.getSession();
            final Collection toProcess = new Vector();
            final Collection toDelete = new Vector();

            boolean loadedJavaFile = false;
            session.queryOnce("retractall(errors_in_java_code)");

            Debug.info("Resource delta recieved: " + delta);
            SubProgressMonitor submon = new SubProgressMonitor(monitor, 10,
                    SubProgressMonitor.PREPEND_MAIN_LABEL_TO_SUBTASK);
            submon.beginTask("Collecting Files", IProgressMonitor.UNKNOWN);
            if (delta == null) {
                //session.queryOnce("delete_source_facts");
                collectAll(toProcess);
            } else {
                collectDelta(delta, toProcess, toDelete);
            }
            submon.done();

            submon = new SubProgressMonitor(monitor, 10,
                    SubProgressMonitor.PREPEND_MAIN_LABEL_TO_SUBTASK);
            forgetFacts(submon, toDelete);

            submon = new SubProgressMonitor(monitor, 40,
                    SubProgressMonitor.PREPEND_MAIN_LABEL_TO_SUBTASK);
            buildFacts(submon, toProcess);

            submon = new SubProgressMonitor(monitor, 40,
                    SubProgressMonitor.PREPEND_MAIN_LABEL_TO_SUBTASK);
            loadExternalFacts(submon);
            JTransformerPlugin plugin = JTransformerPlugin.getDefault();
            String v = plugin.getPreferenceValue(JTransformer.PREF_USE_PEF_STORE,
                    "false");
//            if (Boolean.valueOf(v).booleanValue()) {
//                JTransformerPlugin.getDefault().save(session);
//            }
        } finally {
            if (session != null) {
                session.dispose();
            }
        }
    }

    private void forgetFacts(IProgressMonitor monitor, final Collection toDelete)
            throws IOException {
        monitor.beginTask("deleting obsolete PEFs ", toDelete.size());
        for (Iterator i = toDelete.iterator(); i.hasNext();) {
            if (monitor.isCanceled()) {
                throw new OperationCanceledException("Canceled");
            }
            IFile file = (IFile) i.next();
            forgetFacts(file, true);
            monitor.worked(1);
        }
        monitor.done();
    }

    private void buildFacts(IProgressMonitor monitor, final Collection toProcess)
            throws IOException, CoreException {
        monitor.beginTask("Generating new PEFs ", toProcess.size());
        for (Iterator i = toProcess.iterator(); i.hasNext();) {
            if (monitor.isCanceled()) {
                throw new OperationCanceledException("Canceled");
            }
            IFile file = (IFile) i.next();
            forgetFacts(file, false);
            buildFacts(file);
            monitor.worked(1);
        }
        monitor.done();
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
						
                        if ("OinkOinkOink.java".equals(resource.getName())) {
                            Debug.debug("debug");
                        }
						if(!inExclusionPattern(resource))
                        switch (delta.getKind()) {
                        case IResourceDelta.REMOVED:
                            toDelete.add(resource);
                            break;
                        case IResourceDelta.CHANGED:
                            if (!isStoreUpToDate((IFile) (resource))) { // added,...???
                                Debug.debug("Adding " + resource
                                        + " to toProcess");
								final IJavaProject javaProject = (IJavaProject) project
				                .getNature(JavaCore.NATURE_ID);
								
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

    private void forgetFacts(IFile file, boolean removeGlobalIdsFacts)
            throws IOException {
        Debug.debug("Forgetting (possible) previous version of " + file);

        String path = file.getFullPath().toString();
        // ld: an unconsult is not enough due to the multifile-ness of the
        // predicates in question.
        /*
         * if there is no tree with that id, the retraction fails, but no harm
         * done
         */

        PrologSession session = pif.getSession();
        try {
            if (removeGlobalIdsFacts)
                session
                        .queryOnce("remove_contained_global_ids('" + path
                                + "')");

            session.queryOnce("delete_toplevel('" + path + "')");

        } finally {
            session.dispose();
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
        ConsultService cs = getMetaDataSRC();
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

    private void buildFacts(IFile file) throws IOException, CoreException {

        /* the file seems to have been deleted */
        if (!file.exists()) {
            return;
        }
        ConsultService cs = getMetaDataSRC();
        String recordPath = file.getFullPath().addFileExtension("pl")
                .toString();

        ICompilationUnit icu = JavaCore.createCompilationUnitFrom(file);

        PrintStream out = cs.getOutputStream(recordPath);
        try {

            icu.becomeWorkingCopy(null, null);

            writeFacts(icu, out);
        } catch (JavaModelException jme) {
            IMarker marker = file.createMarker(JTransformer.PROBLEM_MARKER_ID);
            marker.setAttribute(IMarker.MESSAGE,
                    "There seem to be problems in the java code.");
            PrologSession session = pif.getSession();
            session.queryOnce("assert(errors_in_java_code)");
            session.dispose();
            throw new CoreException(new Status(IStatus.CANCEL, JTransformer.PLUGIN_ID,
                    IResourceStatus.BUILD_FAILED,
                    "build canceled due to problems in the java code.", jme));
        } finally {
            icu.discardWorkingCopy();
            out.close();
        }
    }

    public List getUnresolvedTypes() {
        PrologSession session = pif.getSession();
        try {
            return getUnresolvedTypes(session, new HashSet());
        } finally {
            session.dispose();
        }
    }

    protected List getUnresolvedTypes(PrologSession session, HashSet failed) {
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
            CoreException {
        Debug.debug("enter loadExternalFacts");
        monitor.beginTask("creating external PEFs.", IProgressMonitor.UNKNOWN);
        HashSet failed = new HashSet();
        PrologSession session = pif.getSession();
        try {
            List unresolved = getUnresolvedTypes(session, failed);

            Set seen = new HashSet();
            while (!unresolved.isEmpty()) {
                PrintStream out = getMetaDataEXT().getOutputStream("flat.pl");
                try {
                    for (Iterator iter = unresolved.iterator(); iter.hasNext();) {
                        String typeName = (String) iter.next();
                        if (seen.contains(typeName)) {
                            throw new RuntimeException(
                                    "saw type before, so something seems broken. "
                                            + typeName);
                        }
                        // ---<DEBUG>--------------------------8<------------------------------------------for
                        // trapping JT-113
                        /*
                         * if("java.lang.Object".equals(typeName)){
                         * Debug.warning("ok, so you want java.lang.Object,
                         * huh?"); Debug.warning("let's see...this is our stack:
                         * "); Debug.dumpStackTrace(); Debug.warning("Using
                         * divine knowledge to find the record file for external
                         * PEFs..."); File flat =
                         * pif.getFactory().getResourceLocator().subLocator(JTransformer.EXT).resolve("flat.pl");
                         * Debug.warning("\t-->\t should be here:
                         * "+flat.toString()); if(flat.canRead()){
                         * Debug.warning("\t-->\t exists and is readable.");
                         * String prologFileName = Util.prologFileName(flat);
                         * Map map =
                         * session.queryOnce("source_file('"+prologFileName+"')");
                         * if(map!=null){ Debug.warning("\t-->\t is known to
                         * prolog (source_file/1)"); BufferedReader reader = new
                         * BufferedReader(new FileReader(flat)); boolean
                         * doomed=false; try{ String line = reader.readLine();
                         * while(null!=line){ if(line.indexOf("'Object'")!=-1){
                         * doomed=true; break; } line = reader.readLine(); } }
                         * finally{ reader.close(); } if(doomed){
                         * Debug.warning("\t-->\t does contain magic string
                         * 'Object'."); Debug.warning("\t-->\t realy realy
                         * unresolved?
                         * "+getUnresolvedTypes().contains("java.lang.Object"));
                         * Debug.error("this MUST lead to an error, so i can
                         * quit right away. see you."); System.exit(-42); }
                         * else{ Debug.warning("\t-->\t does NOT contain magic
                         * string 'Object'."); Debug.warning("\t-->Ok, no prob,
                         * you may pass."); } } else{ Debug.warning("\t-->\t is
                         * NOT known to prolog (source_file/1)");
                         * Debug.error("this MUST lead to an error, so i can
                         * quit right away. see you."); System.exit(-42); } }
                         * else { Debug.warning("\t-->\t is NOT readable or does
                         * not exist."); Debug.warning("\t-->Ok, no prob, you
                         * may pass."); } } //
                         * -------------------------------------------------------------------------------------------
                         * </DEBUG>
                         *  
                         */
                        seen.add(typeName);
                        try {
                            SubProgressMonitor submon = new SubProgressMonitor(
                                    monitor,
                                    IProgressMonitor.UNKNOWN,
                                    SubProgressMonitor.PREPEND_MAIN_LABEL_TO_SUBTASK);
                            submon
                                    .beginTask(typeName,
                                            IProgressMonitor.UNKNOWN);
                            if (submon.isCanceled()) {
                                throw new OperationCanceledException();
                            }
                            writeFacts(typeName, out);
                            submon.done();
                        } catch (ClassNotFoundException e) {
                            failed.add(typeName);
                        }
                    }
                } finally {
                    out.close();
                }
                unresolved = getUnresolvedTypes(session, failed);
            }
        } finally {
            session.dispose();
            monitor.done();
            Debug.debug("exit loadExternalFacts");
        }
    }

    public static void writeFacts(IProject project, final ICompilationUnit icu,
            PrintStream out) throws IOException, CoreException {

        IResource resource = icu.getResource();
        String path = resource.getFullPath().removeFileExtension()
                .addFileExtension("pl").toString();

        StringWriter sw = new StringWriter();
        PrologWriter plw = new PrologWriter(sw, true);// metaDataSRC.getPrependablePrologWriter(path);
        FactGenerationToolBox box = new DefaultGenerationToolbox();
        FactGenerator visitor = new FactGenerator(icu, resource.getFullPath()
                .toString(), box, plw);

        ASTParser parser = ASTParser.newParser(AST.JLS2);
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
		plw.writeQuery("retractLocalSymtab");
        plw.flush();
        String data = sw.getBuffer().toString();
        sw.getBuffer().setLength(0);
        Map mapping = box.getFQNTranslator().getFQNMapping();
        writeSymTab(plw, mapping);
        plw.flush();
        plw.close();
        String header = sw.getBuffer().toString();
        out.println(header);
        out.println(data);

    }

    public static void writeFacts(IProject project, String typeName,
            PrintStream out) throws JavaModelException, CoreException,
            ClassNotFoundException {
        StringWriter sw = new StringWriter();
        PrologWriter plw = new PrologWriter(sw, true);

        FactGenerationToolBox box = new DefaultGenerationToolbox();
        new ByteCodeFactGeneratorIType(project, plw, typeName, box)
                .writeAllFacts();
        plw.writeQuery("retractLocalSymtab");
        plw.flush();
        String data = sw.toString();
        sw.getBuffer().setLength(0);
        Map mapping = box.getFQNTranslator().getFQNMapping();
        writeSymTab(plw, mapping);
        plw.flush();
        String header = sw.toString();
        sw.getBuffer().setLength(0);
        String fileName = typeName.replace('.', '/') + ".pl";
        // out = metaDataEXT.getOutputStream(fileName);
        out.println(header);
        out.println(data);
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
     * @param out
     *                a writer to which the facts should be written. It is a good
     *                idea to use a buffered writer. The writer will not be closed.
     */
    public void writeFacts(ICompilationUnit icu, PrintStream out)
            throws IOException, CoreException {
        writeFacts(project, icu, out);
    }

    public void writeFacts(String typeName, PrintStream out)
            throws JavaModelException, CoreException, ClassNotFoundException {
        writeFacts(project, typeName, out);
    }

    private ConsultService getMetaDataSRC() {

        return pif.getConsultService(JTransformer.SRC);
    }

    private ConsultService getMetaDataEXT() {
        return pif.getConsultService(JTransformer.EXT);

    }

    /**
     * @param monitor
     * @throws CoreException
     */
    public void clean(IProgressMonitor monitor) throws CoreException {
        Debug.info("clean called on project " + project);
        project.deleteMarkers(JTransformer.PROBLEM_MARKER_ID, true,
                IResource.DEPTH_INFINITE);
        PrologSession session = pif.getSession();
        try {
            session.queryOnce("clearTreeFactbase");
            // getMetaDataSRC().clearRecords();
            String storeName = jtransformerProject.getPreferenceValue(
                    JTransformer.PROP_PEF_STORE_FILE, null);
            new File(storeName).delete();
            storeTimeStamp = -1;
            if (monitor != null) {
                monitor.done();
            }
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
        JTransformerProjcetEvent e = new JTransformerProjcetEvent(this);
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

}
