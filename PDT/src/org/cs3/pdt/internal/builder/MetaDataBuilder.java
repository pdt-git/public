/*
 */
package org.cs3.pdt.internal.builder;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

import org.cs3.pdt.IPrologProject;
import org.cs3.pdt.PDT;
import org.cs3.pdt.PDTPlugin;
import org.cs3.pdt.internal.editors.MarkerProblemCollector;
import org.cs3.pdt.internal.views.IFileLineBreakInfoProvider;
import org.cs3.pl.common.Debug;
import org.cs3.pl.common.Util;
import org.cs3.pl.parser.PrologCompiler;
import org.cs3.pl.prolog.ConsultService;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceDelta;
import org.eclipse.core.resources.IResourceDeltaVisitor;
import org.eclipse.core.resources.IResourceVisitor;
import org.eclipse.core.resources.IncrementalProjectBuilder;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.core.runtime.SubProgressMonitor;

/**
 */
public class MetaDataBuilder extends IncrementalProjectBuilder {

    /**
     *  
     */
    public MetaDataBuilder() {
        super();
    }

    private void build(IFile file) throws CoreException, IOException {
        file.deleteMarkers(IMarker.PROBLEM, true, 0);
        IFileLineBreakInfoProvider lineInfo = new IFileLineBreakInfoProvider(
                file);
        MarkerProblemCollector collector = new MarkerProblemCollector(file,
                lineInfo);
        final String fileName = file.getFullPath().toString();

        PrologCompiler checker = new PrologCompiler();
        checker.setProblemCollector(collector);
        checker.compile(fileName, file.getContents(), lineInfo);
        PDTPlugin plugin = PDTPlugin.getDefault();
        ConsultService meta = plugin.getConsultService(PDT.CS_METADATA);
        checker.saveMetaDataForClauses(meta.getOutputStream(fileName));
        if(collector.getMaxSeverity()<IMarker.SEVERITY_ERROR){
            autoConsult(file);
        }
    }

    private void autoConsult(IFile file) throws CoreException {
        IPrologProject plProject=(IPrologProject) file.getProject().getNature(PDT.NATURE_ID);
        if(!plProject.isAutoConsulted(file)){
            return;
        }
        IMarker[] markers = file.findMarkers(IMarker.PROBLEM, true,
                IResource.DEPTH_ZERO);
        for (int i = 0; i < markers.length; i++) {
            int val = markers[i].getAttribute(IMarker.SEVERITY,
                    IMarker.SEVERITY_INFO);
            
            if (val == IMarker.SEVERITY_ERROR) {                
                return;
            }
        }
        try {
            PDTPlugin plugin = PDTPlugin.getDefault();
            ConsultService consultService = plugin.getConsultService(PDT.CS_WORKSPACE);
            InputStream in = file.getContents();
            OutputStream out=consultService.getOutputStream(file.getFullPath().toString());
            Util.copy(in,out);
        } catch (IOException e) {
            Debug.error("could not consult.");
            Debug.report(e);
        }        
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.core.internal.events.InternalBuilder#build(int,
     *           java.util.Map, org.eclipse.core.runtime.IProgressMonitor)
     */
    protected IProject[] build(int kind, Map args, IProgressMonitor monitor)
            throws CoreException {
        try {
            String taskname = "building prolog metadata";
            ;
            Set forgetList = new HashSet();
            Set buildList = new HashSet();
            switch (kind) {
            case IncrementalProjectBuilder.AUTO_BUILD:
            case IncrementalProjectBuilder.INCREMENTAL_BUILD:

                collect(getDelta(getProject()), buildList, forgetList);
                break;
            case IncrementalProjectBuilder.FULL_BUILD:
                collect(getProject(), buildList);
                break;
            case IncrementalProjectBuilder.CLEAN_BUILD:

                collect(getProject(), forgetList);
                buildList.addAll(forgetList);
                break;
            default:
                Debug.error("Wasn das für ein Buil kind jetzt?");
                return null;
            }

            monitor.beginTask(taskname, forgetList.size() + buildList.size());
            forget(forgetList, new SubProgressMonitor(monitor, forgetList
                    .size()));
            build(buildList, new SubProgressMonitor(monitor, buildList.size()));
            monitor.done();
            return null;
        } catch (OperationCanceledException e) {
            throw e;
        } catch (Throwable t) {
            Debug.report(t);
            return null;
        }
    }

    /**
     * @param v
     * @param monitor
     * @throws IOException
     * @throws CoreException
     */
    private void build(Set v, IProgressMonitor monitor) throws CoreException,
            IOException {
        monitor.beginTask("building prolog metadata", v.size());
        for (Iterator it = v.iterator(); it.hasNext();) {
            IFile file = (IFile) it.next();
            build(file);
            monitor.worked(1);
        }
        monitor.done();
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.core.resources.IncrementalProjectBuilder#clean(org.eclipse.core.runtime.IProgressMonitor)
     */
    protected void clean(IProgressMonitor monitor) throws CoreException {
        Set forgetList = new HashSet();
        collect(getProject(), forgetList);
        forget(forgetList, monitor);
    }

    /**
     * @param project
     * @param buildList
     * @return
     * @throws CoreException
     */
    private void collect(IProject project, final Set buildList)
            throws CoreException {
        final IPrologProject plProject = (IPrologProject) project
                .getNature(PDT.NATURE_ID);
        Set roots = plProject.getExistingSourcePathEntries();
        for (Iterator it = roots.iterator(); it.hasNext();) {
            IResource root = (IResource) it.next();
            root.accept(new IResourceVisitor() {
                public boolean visit(IResource resource) throws CoreException {
                    try {
                        if (resource.getType() == IResource.FILE
                                && plProject.isPrologSource(resource)) {
                            buildList.add(resource);
                        }
                        return true;
                    } catch (Throwable t) {
                        Debug.report(t);
                        throw new RuntimeException(t);
                    }
                }
            });
        }
    }

    /**
     * @param delta
     * @param forgetList
     * @param buildList
     * @return
     * @throws CoreException
     */
    private void collect(IResourceDelta delta, final Set buildList,
            final Set forgetList) throws CoreException {
        final IPrologProject plProject = (IPrologProject) getProject()
                .getNature(PDT.NATURE_ID);

        delta.accept(new IResourceDeltaVisitor() {
            public boolean visit(IResourceDelta delta) throws CoreException {
                try {
                    IResource resource = delta.getResource();
                    boolean isCanidate = isCanidate(resource);
                    if (isCanidate && resource.getType() == IResource.FILE) {
                        if (delta.getKind() == IResourceDelta.REMOVED) {
                            forgetList.add(resource);
                        } else {
                            buildList.add(resource);
                        }
                        return false;
                    }
                    return isCanidate;
                } catch (Throwable t) {
                    Debug.report(t);
                    throw new RuntimeException(t);
                }
            }
        });
    }

    private void forget(IFile file) {
        ConsultService meta = PDTPlugin.getDefault().getConsultService(
                PDT.CS_METADATA);
        
        String s = file.getFullPath().toString();
        if(meta.isConsulted(s)){
            meta.unconsult(s);
        }
    }

    /**
     * @param forgetList
     * @param monitor
     */
    private void forget(Set v, IProgressMonitor monitor) {
        monitor.beginTask("forgetting prolog metadata", v.size());
        for (Iterator it = v.iterator(); it.hasNext();) {
            IFile file = (IFile) it.next();
            forget(file);
            monitor.worked(1);
        }
        monitor.done();

    }

    private boolean isCanidate(IResource r) throws CoreException {
        final IPrologProject plProject = (IPrologProject) getProject()
                .getNature(PDT.NATURE_ID);
        if (plProject.isPrologSource(r)) {
            return true;
        }
        Set srcDirs = plProject.getExistingSourcePathEntries();
        for (Iterator it = srcDirs.iterator(); it.hasNext();) {
            IResource srcDir = (IResource) it.next();
            if (r.getFullPath().isPrefixOf(srcDir.getFullPath())) {
                return true;
            }
        }
        return false;
    }
}
