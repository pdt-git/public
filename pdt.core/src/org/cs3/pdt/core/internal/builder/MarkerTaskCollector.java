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
    private ArrayList<HashMap<String, Integer>> markers = new ArrayList<HashMap<String, Integer>>();
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
    @Override
	public void reportTask(Task p){
        
        HashMap<String, Integer> attributes = new HashMap<String, Integer>();
        
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
    @Override
	public void reset() {        
       resetProblems();
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.parser.ProblemCollector#done()
     */
    @Override
	public void done() {
        createMarkers();
    }

    private void createMarkers() {
        if (markers.size() > 0) {

            IWorkspaceRunnable r = new IWorkspaceRunnable() {
                @Override
				public void run(IProgressMonitor monitor) throws CoreException {
                    for (int i = 0; i < markers.size(); i++) {
                        IMarker marker = file.createMarker(IMarker.TASK);
                        marker.setAttributes(markers.get(i));
                       
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

