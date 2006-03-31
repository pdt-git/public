/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Lukas Degener (among others) 
 * E-mail: degenerl@cs.uni-bonn.de
 * WWW: http://roots.iai.uni-bonn.de/research/pdt 
 * Copyright (C): 2004-2006, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms 
 * of the Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 * In addition, you may at your option use, modify and redistribute any
 * part of this program under the terms of the GNU Lesser General Public
 * License (LGPL), version 2.1 or, at your option, any later version of the
 * same license, as long as
 * 
 * 1) The program part in question does not depend, either directly or
 *   indirectly, on parts of the Eclipse framework and
 *   
 * 2) the program part in question does not include files that contain or
 *   are derived from third-party work and are therefor covered by special
 *   license agreements.
 *   
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 *   
 * ad 1: A program part is said to "depend, either directly or indirectly,
 *   on parts of the Eclipse framework", if it cannot be compiled or cannot
 *   be run without the help or presence of some part of the Eclipse
 *   framework. All java classes in packages containing the "pdt" package
 *   fragment in their name fall into this category.
 *   
 * ad 2: "Third-party code" means any code that was originaly written as
 *   part of a project other than the PDT. Files that contain or are based on
 *   such code contain a notice telling you so, and telling you the
 *   particular conditions under which they may be used, modified and/or
 *   distributed.
 ****************************************************************************/

/*
 */
package org.cs3.pdt.core.internal.builder;

import java.util.ArrayList;
import java.util.HashMap;


import org.cs3.pl.common.Debug;
import org.cs3.pl.parser.LineBreakInfoProvider;
import org.cs3.pl.parser.Problem;
import org.cs3.pl.parser.ProblemCollector;
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
//            UIUtils.getDisplay().syncExec(new Runnable() {
//                public void run() {
//                    try {
//                        UIUtils.getActivePage().showView(
//                                IPageLayout.ID_PROBLEM_VIEW);
//                        UIUtils.getActiveEditor()
//                                .getEditorSite().getPage().activate(
//                                        UIUtils
//                                                .getActiveEditor());
//                    } catch (PartInitException e) {
//                        Debug.report(e);
//                    }
//                }
//            });
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

