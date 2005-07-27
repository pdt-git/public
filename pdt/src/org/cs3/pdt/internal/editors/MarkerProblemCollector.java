/*
 */
package org.cs3.pdt.internal.editors;

import java.util.ArrayList;
import java.util.HashMap;

import org.cs3.pdt.UIUtils;
import org.cs3.pl.common.Debug;
import org.cs3.pl.parser.LineBreakInfoProvider;
import org.cs3.pl.parser.Problem;
import org.cs3.pl.parser.ProblemCollector;
import org.cs3.pl.parser.internal.classic.Token;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.IWorkspaceRunnable;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.ui.IPageLayout;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.texteditor.MarkerUtilities;

/**
 */
public class MarkerProblemCollector implements ProblemCollector {
    private ArrayList markers = new ArrayList();
    private IFile file;
    private LineBreakInfoProvider lineInfo;
    private int maxSeverity=-1;
    
    /**
     * @param file
     * @param lineInfo
     */
    public MarkerProblemCollector(IFile file, LineBreakInfoProvider lineInfo) {
        this.file = file;
        this.lineInfo = lineInfo;
    }
    protected int getLineOffset(int line) {
        return lineInfo==null? 0 : lineInfo.getOffsetAtLine(line);
    }
    public int mapSeverity(int x){
        switch(x){
        	case Problem.INFO:
        	    return IMarker.SEVERITY_INFO;
        	 case Problem.WARNING:
        	     return IMarker.SEVERITY_WARNING;
        	case Problem.ERROR:
        	    return IMarker.SEVERITY_ERROR; 
        }
        return IMarker.SEVERITY_INFO;
    }
    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.parser.ProblemCollector#reportProblem(org.cs3.pl.parser.Token,
     *           java.lang.String, int)
     */
    public void reportProblem(Problem p) {
        int severity=mapSeverity(p.severity);
        maxSeverity=Math.max(maxSeverity,severity);
        
        HashMap attributes = new HashMap();
        MarkerUtilities.setMessage(attributes, p.message);
        MarkerUtilities.setLineNumber(attributes, p.firstRow);
        
        MarkerUtilities.setCharStart(attributes, p.beginOffset);
        MarkerUtilities.setCharEnd(attributes, p.endOffset);
        attributes.put(IMarker.SEVERITY, new Integer(severity));

        markers.add(attributes);       

    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.parser.ProblemCollector#reset()
     */
    public void reset() {
        maxSeverity=Integer.MIN_VALUE;
       resetProblems();
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.parser.ProblemCollector#done()
     */
    public void done() {
        createMarkers();
    }

    private void createMarkers() {
        if (markers.size() > 0) {

            IWorkspaceRunnable r = new IWorkspaceRunnable() {
                public void run(IProgressMonitor monitor) throws CoreException {
                    for (int i = 0; i < markers.size(); i++) {
                        IMarker marker = file.createMarker(IMarker.PROBLEM);
                        marker.setAttributes((HashMap) markers.get(i));
                        //				MarkerAnnotation annotation = new
                        // MarkerAnnotation(marker);
                        //				annotation.setText("ahahahaha");
                    }
                }
            };
            try {
                file.getWorkspace().run(r, null, IWorkspace.AVOID_UPDATE, null);
            } catch (CoreException e1) {
                Debug.error("problem creating markers");
                Debug.report(e1);
            }
            UIUtils.getDisplay().syncExec(new Runnable() {
                public void run() {
                    try {
                        UIUtils.getActivePage().showView(
                                IPageLayout.ID_PROBLEM_VIEW);
                        UIUtils.getActiveEditor()
                                .getEditorSite().getPage().activate(
                                        UIUtils
                                                .getActiveEditor());
                    } catch (PartInitException e) {
                        Debug.report(e);
                    }
                }
            });
        }
    }

    /**
     *  
     */
    protected void resetProblems() {
        try {
            file.deleteMarkers(IMarker.PROBLEM, true, IResource.DEPTH_INFINITE);
        } catch (CoreException e) {
            Debug.error("Problem deleting markers");
            Debug.report(e);
        }
    }

    public int getMaxSeverity() {
        return maxSeverity;
    }
}

