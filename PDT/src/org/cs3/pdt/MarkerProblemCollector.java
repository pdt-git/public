/*
 */
package org.cs3.pdt;

import java.util.ArrayList;
import java.util.HashMap;

import org.cs3.pl.common.Debug;
import org.cs3.pdt.PDTPlugin;
import org.cs3.pl.parser.LineBreakInfoProvider;
import org.cs3.pl.parser.ProblemCollector;
import org.cs3.pl.parser.PrologCompiler;
import org.cs3.pl.parser.Token;
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
        	case PrologCompiler.INFO:
        	    return IMarker.SEVERITY_INFO;
        	 case PrologCompiler.WARNING:
        	     return IMarker.SEVERITY_WARNING;
        	case PrologCompiler.ERROR:
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
    public void reportProblem(Token token, String msg, int severity) {
        severity=mapSeverity(severity);
        int offset = getLineOffset(token.beginLine);
        HashMap attributes = new HashMap();
        MarkerUtilities.setMessage(attributes, msg);
        MarkerUtilities.setLineNumber(attributes, token.beginLine);
        int begin = offset + token.beginColumn - 1;
        if (begin < 0)
            begin = 0;
        //int add = 0;
        //if (severity == IMarker.SEVERITY_WARNING) //TODO: clean solution
        // needed
        //	add =1;
        MarkerUtilities.setCharStart(attributes, begin);
        MarkerUtilities.setCharEnd(attributes, offset + token.endColumn);
        attributes.put(IMarker.SEVERITY, new Integer(severity));

        markers.add(attributes);
        //createMarker(file,attributes,IMarker.PROBLEM);
        //MarkerUtilities.createMarker(file, attributes, IMarker.PROBLEM);
        //		marker.setAttribute(IMarker.CHAR_START, token.beginColumn); //
        // CHAR_START not relative to line !
        //		marker.setAttribute(IMarker.CHAR_END, token.endColumn);

    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.parser.ProblemCollector#reset()
     */
    public void reset() {
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
            PDTPlugin.getDefault().getDisplay().syncExec(new Runnable() {
                public void run() {
                    try {
                        PDTPlugin.getDefault().getActivePage().showView(
                                IPageLayout.ID_PROBLEM_VIEW);
                        PDTPlugin.getDefault().getActiveEditor()
                                .getEditorSite().getPage().activate(
                                        PDTPlugin.getDefault()
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

}

