package org.cs3.jlmp.builders;

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

import org.cs3.jlmp.JLMP;
import org.cs3.jlmp.JLMPPlugin;
import org.cs3.jlmp.astvisitor.DefaultGenerationToolbox;
import org.cs3.jlmp.astvisitor.FactGenerationToolBox;
import org.cs3.jlmp.astvisitor.FactGenerator;
import org.cs3.jlmp.astvisitor.PrologWriter;
import org.cs3.jlmp.bytecode.ByteCodeFactGeneratorIType;
import org.cs3.jlmp.natures.JLMPProjectNature;
import org.cs3.pl.common.Debug;
import org.cs3.pl.common.Util;
import org.cs3.pl.prolog.ConsultService;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologSession;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceDelta;
import org.eclipse.core.resources.IResourceDeltaVisitor;
import org.eclipse.core.resources.IResourceVisitor;
import org.eclipse.core.resources.WorkspaceJob;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.core.runtime.Status;
import org.eclipse.jdt.core.ICompilationUnit;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.jdt.core.dom.AST;
import org.eclipse.jdt.core.dom.ASTParser;
import org.eclipse.jdt.core.dom.CompilationUnit;

public class FactBaseBuilder {
    /*
     * ld: please note: in principle, this builder should be stateless. The
     * non-final fields will be reconstructed on every call o build()
     * 
     * this is a temporary solution, i'm in the middle of a refactoring.
     */

    private Collection failed = new HashSet();

    private Iterator nextIterator = null;

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

    private boolean loadedJavaFile = false;

    private boolean errorsInCode = false;

    private PrologSession prologClient;

    private JLMPProjectNature jlmpProject;

    private ConsultService metaDataSRC;

    private ConsultService metaDataEXT;

    private PrologInterface pif;

    private IProject project;

    private PrintStream out;

    /**
     *  
     */
    public FactBaseBuilder() {

        // TODO Auto-generated constructor stub
    }

    /**
     * Collects changed classes from the delta, and creates new facts for them.
     * The generated facts are consulted into the Prolog System, and if
     * necessary, binary-only type references are resolved. Once this method
     * returns, the state of the prolog systems factbase is consistent to the
     * state of the project. If this method is called with a null delta, it
     * rebuilds all sourcefiles in the current JLMP project. If Monitor is null,
     * the process will not supply progress information, but work nevertheless.
     * Implementation may run the build process in a seperate job.
     * 
     * @param delta
     *                  the delta containing the changed Resources
     * @param flags
     *                  a logical or of the constants defined in this class
     * @param monitor
     *                  a IProgressMonitor object
     */

    public synchronized void build(final IResourceDelta delta, final int flags,
            final IProgressMonitor monitor) {
        loadedJavaFile = false;
        WorkspaceJob job = new WorkspaceJob("JLMP Buildjob") {
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
                        return new Status(Status.CANCEL, JLMPPlugin.PLUGIN_ID,
                                Status.OK, "Operation Canceled", t);

                    Debug.report(t);
                    return new Status(Status.ERROR, JLMPPlugin.PLUGIN_ID,
                            Status.OK, "Exception while rebuilding fact base",
                            t);
                }

                //				
                //					if(!errorsInCode) {
                //					    WorkbenchJob job = new WorkbenchJob("update factbase
                // observers") {
                //	
                //	                        public IStatus runInUIThread(IProgressMonitor monitor) {
                //	                            if(!JLMPPlugin.getDefault().updateFactbaseObservers(IJTransformerObserver.JT_FACTBASE_UPDATED,prologClient,project))
                //	            					return new Status(Status.ERROR, JLMPPlugin.PLUGIN_ID, 0,
                // "Failed to update factbase observers", null);
                //	                            return Status.OK_STATUS;
                //	                        }
                //					        
                //					    };
                //					    job.schedule();
                //					}
                return Status.OK_STATUS;

                //					PDTPlugin.getDefault().updateFactbaseObservers(IJTransformerObserver.JT_BUILD_ERROR,prologClient,project);
                //					String msg = "Failed to load some classes: ";
                //					for (Iterator iter = failed.iterator(); iter.hasNext();) {
                //					    String typename = iter.next().toString();
                //						msg += typename + "\n";
                //						
                //						prologClient.query("assert(ignore_unresolved_type('"+typename
                // + "'))");
                //					}
                //					Debug.error(msg);
                //					return Status.OK_STATUS;
            }

        };

        job.schedule();
    }

    synchronized private void build_impl(IResourceDelta delta, int flags,
            IProgressMonitor monitor) {
        boolean delete;

        try {
            if (jlmpProject == null) {
                Debug
                        .warning("JLMPProjectBuilder called on null project. Aborted.");
                return;
            }

            pif = jlmpProject.getPrologInterface();
            prologClient = pif.getSession();
            final Collection toProcess = new Vector();
            final Collection toDelete = new Vector();
            failed.clear();
            errorsInCode = false;
            prologClient.queryOnce("retractall(errors_in_java_code)");
            project = jlmpProject.getProject();

            metaDataEXT = pif.getConsultService(JLMP.EXT);
            metaDataEXT.setRecording(false);
            metaDataSRC = pif.getConsultService(JLMP.SRC);
            metaDataSRC.setRecording(false);

            Debug.info("Resource delta recieved: " + delta);

            if ((flags & CLEAR_PRJ_FACTS) != 0) {
                metaDataSRC.clearRecords();
            }

            if ((flags & CLEAR_EXT_FACTS) != 0) {
                metaDataEXT.clearRecords();
            }

            monitor.beginTask("Collecting Files to update",
                    IProgressMonitor.UNKNOWN);

            if (delta == null) {
                delete = false;

                /*
                 * forces reload of all java files in the JLMP-Nature project.
                 * First we collect them all, then we build them one by one.
                 */

                project.accept(new IResourceVisitor() {

                    public boolean visit(IResource resource)
                            throws CoreException {

                        Debug.debug("Visiting: " + resource);

                        if (resource.getType() == IResource.ROOT)
                            return true;
                        if (resource.getType() == IResource.PROJECT)
                            return resource.getProject().hasNature(
                                    JLMPProjectNature.NATURE_ID);
                        /*
                         * since we only enter Projects that have the JLMP
                         * Nature set, this should be safe...
                         */
                        if (resource.getType() == IResource.FOLDER)
                            return true;
                        if (resource.getType() == IResource.FILE) {
                            if (resource.getFileExtension() != null
                                    && resource.getFileExtension().equals(
                                            "java")) {
                                Debug.debug("Adding " + resource
                                        + " to toProcess");
                                toProcess.add(resource);
                            }
                        }
                        return false;
                    }
                });
            } else {
                delete = true;
                delta.accept(new IResourceDeltaVisitor() {

                    public boolean visit(IResourceDelta delta)
                            throws CoreException {
                        IResource resource = delta.getResource();

                        if (resource.getType() == IResource.ROOT)
                            return true;
                        if (resource.getType() == IResource.PROJECT)
                            return resource.getProject().hasNature(
                                    JLMPProjectNature.NATURE_ID);
                        /*
                         * since we only enter Projects that have the JLMP
                         * Nature set, this should be safe...
                         */
                        if (resource.getType() == IResource.FOLDER)
                            return true;
                        if (resource.getType() == IResource.FILE) {
                            String fext = resource.getFileExtension();
                            if (fext != null && fext.equals("java")) {
                                if (delta.getKind() == IResourceDelta.REMOVED) {
                                    toDelete.add(resource);
                                } else { // added,...???
                                    Debug.debug("Adding " + resource
                                            + " to toProcess");
                                    toProcess.add(resource);
                                }
                            }
                        }

                        return false;
                    }
                });

            }

            monitor.done();

            if ((flags & IGNORE_PRJ_FACTS) == 0) {

                for (Iterator i = toProcess.iterator(); i.hasNext();) {

                    if (monitor.isCanceled())
                        throw new OperationCanceledException("Canceled");

                    IFile file = (IFile) i.next();

                    monitor.beginTask("Generating Facts for source file "
                            + file, IProgressMonitor.UNKNOWN);

                    forgetFacts(file, delete, false);
                    out = metaDataSRC.getOutputStream("flat.pl");
                    try {
                        if (!buildFacts(file)) {
                            // if an exception in thrown while building,
                            // the system may look for unresolved types and
                            // fail in loadExternalFacts(monitor)
                            errorsInCode = true;
                            prologClient.query("assert(errors_in_java_code)");
                        }
                    } finally {
                        out.close();
                        monitor.done();
                    }

                }
                for (Iterator i = toDelete.iterator(); i.hasNext();) {

                    if (monitor.isCanceled())
                        throw new OperationCanceledException("Canceled");

                    IFile file = (IFile) i.next();

                    monitor.beginTask("Delete Facts of deleted files " + file,
                            IProgressMonitor.UNKNOWN);

                    forgetFacts(file, delete, true);

                    monitor.done();
                }

            }

            monitor.done();

            if ((flags & IGNORE_EXT_FACTS) == 0 && loadedJavaFile
                    && !errorsInCode) {

                loadExternalFacts(monitor);

            }

            //ld: this should be finer grained!
            //StS: Nope, it is ok, since there can be changes all over pl. and
            // ext

            //JT-66 FIX: removed the following line
            //ResourcesPlugin.getWorkspace().getRoot().refreshLocal(IResource.DEPTH_INFINITE,
            // null);
        } catch (CoreException e) {
            Debug.report(e);
        } catch (IOException e) {
            Debug.report(e);
        } finally {
            prologClient.dispose();
        }
    }

    private void forgetFacts(IFile file, boolean delete,
            boolean removeGlobalIdsFacts) throws IOException {
        Debug.debug("Forgetting (possible) previous version of " + file);

        //ld: an unconsult is not enough due to the multifile-ness of the
        // predicates in question.
        String path = file.getFullPath().toString();

        /*
         * if there is no tree with that id, the retraction fails, but no harm
         * done
         */

        if (removeGlobalIdsFacts)
            prologClient.queryOnce("remove_contained_global_ids('" + path
                    + "')");

        prologClient.queryOnce("delete_toplevel('" + path + "')");
        if (delete) {
            Debug.debug("Deleting resource " + path);
            // metaDataSRC.unconsult(path);
        }
    }

    /**
     * 
     * @param resource
     * @return false if the building failed
     * @throws IOException
     * @throws CoreException
     */

    private boolean buildFacts(IFile resource) throws IOException,
            CoreException {
        loadedJavaFile = true;
        if (!resource.exists())
            /* the file seems to have been deleted */
            return true;

        ICompilationUnit icu = JavaCore.createCompilationUnitFrom(resource);
        CompilationUnit cu = null;

        try {
            icu.becomeWorkingCopy(null, null);
            jlmpProject.writeFacts(icu, out);
        } catch (JavaModelException jme) {
            // if this exception occurs, the java file is in some way bad. Thats
            // ok,
            // since it is also dead (deleted) in the Prolog System.
            // will return false to ensure that the loading of external
            // classes will not be triggert
            Debug.report(jme);
            return false;
        }
        return true;
    }

    protected List getUnresolvedTypes() {
        List list = prologClient.queryAll("unresolved_types(T)");
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

    protected void loadExternalFacts(IProgressMonitor submon)
            throws IOException, CoreException {
        List unresolved = getUnresolvedTypes();

        Set seen = new HashSet();
        while (!unresolved.isEmpty()) {
            out = metaDataEXT.getOutputStream("flat.pl");
            try {
                for (Iterator iter = unresolved.iterator(); iter.hasNext();) {
                    String typeName = (String) iter.next();
                    if (seen.contains(typeName)) {
                        throw new RuntimeException(
                                "saw type before, so something seems broken. "
                                        + typeName);
                    }
                    seen.add(typeName);
                    try {
                        jlmpProject.writeFacts(typeName, out);
                    } catch (ClassNotFoundException e) {
                        failed.add(typeName);
                    }
                }
            } finally {
                out.close();
            }
            unresolved = getUnresolvedTypes();
        }

    }

    /**
     * @return Returns the jlmpProject.
     */
    public JLMPProjectNature getJlmpProject() {
        return jlmpProject;
    }

    /**
     * @param jlmpProject
     *                  The jlmpProject to set.
     */
    public void setJlmpProject(JLMPProjectNature jlmpProject) {
        this.jlmpProject = jlmpProject;
    }
}
