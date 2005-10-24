/*
 */
package org.cs3.pdt.core.internal.builder;

import java.util.ArrayList;
import java.util.HashMap;

import org.cs3.pl.common.Debug;
import org.cs3.pl.parser.Task;
import org.cs3.pl.parser.TaskCollector;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.IWorkspaceRunnable;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.ui.texteditor.MarkerUtilities;

/**
 */
public class MarkerTaskCollector implements TaskCollector {
    private ArrayList markers = new ArrayList();
    private IFile file;
    
    
    /**
     * @param file
     * @param lineInfo
     */
    public MarkerTaskCollector(IFile file) {
        this.file = file;
    
    }
    
    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.parser.ProblemCollector#reportProblem(org.cs3.pl.parser.Token,
     *           java.lang.String, int)
     */
    public void reportTask(Task p){
        
        HashMap attributes = new HashMap();
        
        MarkerUtilities.setMessage(attributes, p.message);
        MarkerUtilities.setLineNumber(attributes, p.firstRow);
        
        MarkerUtilities.setCharStart(attributes, p.beginOffset);
        MarkerUtilities.setCharEnd(attributes, p.endOffset);
        

        markers.add(attributes);       

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
                        IMarker marker = file.createMarker(IMarker.TASK);
                        marker.setAttributes((HashMap) markers.get(i));
                       
                    }
                }
            };
            try {
                file.getWorkspace().run(r, null, IWorkspace.AVOID_UPDATE, null);
            } catch (CoreException e1) {
                Debug.error("problem creating markers");
                Debug.report(e1);
            }

        }
    }

    /**
     *  
     */
    protected void resetProblems() {
        try {
        	markers.clear();
            file.deleteMarkers(IMarker.TASK, true, IResource.DEPTH_INFINITE);
        } catch (CoreException e) {
            Debug.error("Problem deleting markers");
            Debug.report(e);
        }
    }

    
}

