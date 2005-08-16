/*
 */
package org.cs3.pdt.internal.builder;

import java.io.BufferedOutputStream;
import java.io.IOException;
import java.io.PrintStream;
import java.util.Arrays;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

import org.cs3.pdt.IPrologProject;
import org.cs3.pdt.PDT;
import org.cs3.pdt.PDTPlugin;
import org.cs3.pdt.internal.editors.MarkerProblemCollector;
import org.cs3.pdt.internal.views.IFileLineBreakInfoProvider;
import org.cs3.pdt.runtime.PrologRuntimePlugin;
import org.cs3.pl.common.Debug;
import org.cs3.pl.common.Util;
import org.cs3.pl.parser.PrologCompiler;
import org.cs3.pl.parser.PrologCompilerFactory;
import org.cs3.pl.prolog.ConsultService;
import org.cs3.pl.prolog.PrologSession;
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

    private void build(IFile file,PrintStream outputStream) throws CoreException, IOException {
        
        IFileLineBreakInfoProvider lineInfo = new IFileLineBreakInfoProvider(
                file);
        MarkerProblemCollector collector = new MarkerProblemCollector(file,
                lineInfo);
        final String fileName = file.getFullPath().toString();

        PrologCompiler checker = PrologCompilerFactory.create();
        checker.setProblemCollector(collector);
        checker.compile(fileName, file.getContents(), lineInfo);
        PDTPlugin plugin = PDTPlugin.getDefault();
        ConsultService meta = PrologRuntimePlugin.getDefault().getConsultService(PDT.CS_METADATA);
        
			checker.saveMetaDataForClauses(outputStream);
			checker.saveAbbaData(new BufferedOutputStream(outputStream));
			
        if (collector.getMaxSeverity() < IMarker.SEVERITY_ERROR) {
            autoConsult(file);
        }
    }

    private void autoConsult(IFile file) throws CoreException {
        IPrologProject plProject = (IPrologProject) file.getProject()
                .getNature(PDT.NATURE_ID);
        if (!plProject.isAutoConsulted(file)) {
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

        PDTPlugin plugin = PDTPlugin.getDefault();
        String prologName = Util.normalizeOnWindoze(file.getLocation()
                .toOSString());
        PrologSession s = PrologRuntimePlugin.getDefault().getPrologInterface().getSession();
        try {
            s.queryOnce("['" + prologName + "']");
        } finally {
            s.dispose();
        }
        //            ConsultService consultService =
        // plugin.getConsultService(PDT.CS_WORKSPACE);
        //            InputStream in = file.getContents();
        //            OutputStream
        // out=consultService.getOutputStream(file.getFullPath().toString());
        //            Util.copy(in,out);
        //            in.close();
        //            out.close();

    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.core.internal.events.InternalBuilder#build(int,
     *         java.util.Map, org.eclipse.core.runtime.IProgressMonitor)
     */
    protected IProject[] build(int kind, Map args, IProgressMonitor monitor)
            throws CoreException {
        try {
			Debug.debug("MetaDataBuilder.build(...) was triggered");
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
				getProject().deleteMarkers(IMarker.PROBLEM,true,IResource.DEPTH_INFINITE);
                collect(getProject(), buildList);
                break;
            case IncrementalProjectBuilder.CLEAN_BUILD:
				getProject().deleteMarkers(IMarker.PROBLEM,true,IResource.DEPTH_INFINITE);
                collect(getProject(), forgetList);
                break;
            default:
                Debug.error("Wasn das f�r ein Buil kind jetzt?");
                return null;
            }
			forgetList.addAll(buildList);
			Debug.debug("MetaDataBuilder.build(...) wants to forget: "+forgetList.toString());
			Debug.debug("MetaDataBuilder.build(...) wants to build: "+buildList.toString());
            monitor.beginTask(taskname, forgetList.size() + buildList.size());
			PDTPlugin r = PDTPlugin.getDefault();
            PrintStream out = PrologRuntimePlugin.getDefault().getPrologInterface().getConsultService(PDT.CS_METADATA).getOutputStream("flat_pl_metadata.pl");
			//PrintStream out = new PrintStream(new FileOutputStream("c:\\temp\\consulted.pl"));
			try{
			forget(forgetList, out, new SubProgressMonitor(monitor, forgetList
                    .size()));
            build(buildList, out, new SubProgressMonitor(monitor, buildList.size()));
			}
			finally{
				out.close();
			}
			Debug.debug("MetaDataBuilder.build(...) is done.");
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
    private void build(Set v, PrintStream out,IProgressMonitor monitor) throws CoreException,
            IOException {
        monitor.beginTask("building prolog metadata", v.size());
        final HashMap timings = new HashMap();
        String[] filenames = new String[v.size()];
        int i=0;
        for (Iterator it = v.iterator(); it.hasNext();) {
            IFile file = (IFile) it.next();
            filenames[i] = file.getFullPath().toOSString();
             
            try{
            	long time = System.currentTimeMillis();
            	build(file,out);
            	time = System.currentTimeMillis()-time;
            	timings.put(filenames[i++],new Long(time));
            }catch (Throwable e) {
            	timings.put(filenames[i++],new Long(Long.MAX_VALUE));
			}
            
            monitor.worked(1);
        }
        Arrays.sort(filenames,new Comparator() {
		
			public int compare(Object o1, Object o2) {
				Long l1 = (Long) timings.get(o1);
				Long l2 = (Long) timings.get(o2);
				return - l1.compareTo(l2);
			}
		
		});
        Debug.info("TOP 20 Build Times:");
        StringBuffer sb = new StringBuffer();
        for (int j = 0; j < Math.min(20,filenames.length); j++) {
			String filename= filenames[j];
			sb.append("\n");
			sb.append(""+j+" "+filename+":  "+timings.get(filename));
		}
        Debug.info(sb.toString());
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
		getProject().deleteMarkers(IMarker.PROBLEM,true,IResource.DEPTH_INFINITE);
		PDTPlugin r = PDTPlugin.getDefault();
		PrintStream out = PrologRuntimePlugin.getDefault().getPrologInterface().getConsultService(PDT.CS_METADATA).getOutputStream("flat_pl_metadata.pl");
		try{		
			forget(forgetList,out, monitor);
		}
		finally{
			out.close();
		}
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
                                && isCanidate(resource)) {
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

    private void forget(IFile file, PrintStream out) throws CoreException {
        file.deleteMarkers(IMarker.PROBLEM, true, 0);
        String s = file.getFullPath().toString();
		out.println(":- retractall(meta_data_module('"+s+"',_,_)).");
		out.println(":- retractall(meta_data('"+s+"',_,_,_,_,_,_,_,_)).");

    }

    /**
     * @param out 
     * @param forgetList
     * @param monitor
     * @throws CoreException 
     */
    private void forget(Set v, PrintStream out, IProgressMonitor monitor) throws CoreException {
        monitor.beginTask("forgetting prolog metadata", v.size());
        for (Iterator it = v.iterator(); it.hasNext();) {
            IFile file = (IFile) it.next();
            forget(file,out);
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
