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

import org.cs3.pl.common.Debug;
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

    private ConsultService metaDataSRC;

    private ConsultService metaDataEXT;

    private IProject project;

    private PrologInterface pif;

    /**
     *  
     */
    public FactBaseBuilder(IProject project, PrologInterface pif) {

        this.project = project;
        this.pif = pif;
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
            IProgressMonitor monitor) {

        //ld: we catch EVERYTHING, since eclipse eats any exceptions
        // otherwise :-(
        try {
            if (monitor == null)
                monitor = new NullProgressMonitor();

            build_impl(delta, flags, monitor);
        } catch (Throwable t) {
            Debug.report(t);
            throw new RuntimeException(t);
        }

    }

    synchronized private void build_impl(IResourceDelta delta, int flags,
            IProgressMonitor monitor) {
        boolean delete;

        PrologSession session = null;
        try {

            session = pif.getSession();
            final Collection toProcess = new Vector();
            final Collection toDelete = new Vector();

            boolean errorsInCode = false;
            boolean loadedJavaFile = false;
            session.queryOnce("retractall(errors_in_java_code)");

            Debug.info("Resource delta recieved: " + delta);

            if ((flags & CLEAR_PRJ_FACTS) != 0) {
                getMetaDataSRC().clearRecords();
            }

            if ((flags & CLEAR_EXT_FACTS) != 0) {
                getMetaDataEXT().clearRecords();
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
                                    JLMP.NATURE_ID);
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
                                    JLMP.NATURE_ID);
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

                    try {
                        if (!buildFacts(file)) {
                            // if an exception in thrown while building,
                            // the system may look for unresolved types and
                            // fail in loadExternalFacts(monitor)
                            errorsInCode = true;
                            session.queryOnce("assert(errors_in_java_code)");
                        } else {
                            loadedJavaFile = true;
                        }
                    } finally {

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
            session.dispose();
        }
    }

    private void forgetFacts(IFile file, boolean delete,
            boolean removeGlobalIdsFacts) throws IOException {
        Debug.debug("Forgetting (possible) previous version of " + file);

        String path = file.getFullPath().toString();
        getMetaDataSRC().unconsult(path);
        //ld: an unconsult is not enough due to the multifile-ness of the
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
            if (delete) {
                Debug.debug("Deleting resource " + path);
                // metaDataSRC.unconsult(path);
            }
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

    private boolean buildFacts(IFile file) throws IOException,
            CoreException {

        /* the file seems to have been deleted */
        if (!file.exists())        
        {
            return true;
        }
        ConsultService cs = getMetaDataSRC();
        String recordPath =file.getFullPath().addFileExtension("pl").toString();
        long recordTS = cs.getTimeStamp(recordPath);
        long fileTS = file.getModificationStamp();
        //ld:no need to create facts that are already known
        /*FIXME: currently we only ensure that the record is up-to-date.
         * in theory, so should be the consulted facts, but it might be better to check.
         * otoh, this would be quite expensive if there are many resources to check.  
         * */
        if(recordTS>=fileTS){
            return true;
        }
        ICompilationUnit icu = JavaCore.createCompilationUnitFrom(file);
        CompilationUnit cu = null;

        
        PrintStream out = cs.getOutputStream(recordPath);
        try {

            icu.becomeWorkingCopy(null, null);

            writeFacts(icu, out);
        } catch (JavaModelException jme) {
            // if this exception occurs, the java file is in some way bad. Thats
            // ok,
            // since it is also dead (deleted) in the Prolog System.
            // will return false to ensure that the loading of external
            // classes will not be triggert
            Debug.report(jme);
            return false;
        } finally {
            icu.discardWorkingCopy();
            out.close();
        }
        return true;
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

    public void loadExternalFacts(IProgressMonitor submon)

    throws IOException, CoreException {
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
                        seen.add(typeName);
                        try {
                            writeFacts(typeName, out);
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
        }
    }

    public static void writeFacts(IProject project, ICompilationUnit icu,
            PrintStream out) throws IOException, CoreException {
        CompilationUnit cu = null;

        IResource resource = icu.getResource();
        String path = resource.getFullPath().removeFileExtension()
                .addFileExtension("pl").toString();

        StringWriter sw = new StringWriter();
        PrologWriter plw = new PrologWriter(sw, true);//metaDataSRC.getPrependablePrologWriter(path);
        FactGenerationToolBox box = new DefaultGenerationToolbox();
        FactGenerator visitor = new FactGenerator(icu, resource.getFullPath()
                .toString(), box, plw);

        ASTParser parser = ASTParser.newParser(AST.JLS2);
        parser.setSource(icu);
        parser.setResolveBindings(true);
        cu = (CompilationUnit) parser.createAST(null);

        cu.accept(visitor);
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
        //out = metaDataEXT.getOutputStream(fileName);
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
     *                  compilation unit in working copy mode.
     * @param out
     *                  a writer to which the facts should be written. It is a good
     *                  idea to use a buffered writer. The writer will not be closed.
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
        if (metaDataSRC == null) {
            metaDataSRC = pif.getConsultService(JLMP.SRC);
            metaDataSRC.setRecording(true);
            metaDataSRC.setAppendingRecords(false);
        }
        return metaDataSRC;
    }

    private ConsultService getMetaDataEXT() {
        if (metaDataEXT == null) {
            metaDataEXT = pif.getConsultService(JLMP.EXT);
            metaDataEXT.setRecording(true);
            metaDataEXT.setAppendingRecords(true);
        }
        return metaDataEXT;
    }

    /**
     * @param monitor
     */
    public void clean(IProgressMonitor monitor) {
        Debug.info("clean called on project "+project);
        PrologSession session = pif.getSession();
       try{
        session.queryOnce("delete_source_facts");
        getMetaDataSRC().clearRecords();        
        monitor.done();
       }
       finally{
           session.dispose();
       }
    }

}
