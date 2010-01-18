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
package org.cs3.pdt.core.internal.actions;


import org.cs3.pdt.core.IPrologProject;
import org.cs3.pdt.core.PDTCore;
import org.cs3.pdt.core.PDTCorePlugin;
import org.cs3.pl.common.Debug;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.IObjectActionDelegate;
import org.eclipse.ui.IWorkbenchPart;

public class AutoConsultAction implements IObjectActionDelegate {
    private IFile file;

    private IPrologProject project;

    protected boolean error;

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.IObjectActionDelegate#setActivePart(org.eclipse.jface.action.IAction,
     *           org.eclipse.ui.IWorkbenchPart)
     */
    public void setActivePart(IAction action, IWorkbenchPart targetPart) {
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.IActionDelegate#run(org.eclipse.jface.action.IAction)
     */
    public void run(IAction action) {
        try {
            if (file == null||project==null) {
                action.setChecked(false);
                action.setEnabled(false);
                return;
            }
            if(project.isAutoConsulted(file)){
                project.setAutoConsulted(file,false);
                action.setChecked(false);
            }
            else{
                project.setAutoConsulted(file,true);
                action.setChecked(true);
            }
            
        } catch (Throwable t) {
            Debug.report(t);
            throw new RuntimeException(t);
        }
    }

    /*
     * @see org.eclipse.ui.IActionDelegate#selectionChanged(org.eclipse.jface.action.IAction,
     *           org.eclipse.jface.viewers.ISelection)
     */
    public void selectionChanged(IAction action, ISelection selection) {
    	file = null;
        project=null;
    	try {
            if (selection instanceof IStructuredSelection) {
                Object obj = ((IStructuredSelection) selection)
                        .getFirstElement();
                if (obj instanceof IFile) {
              	  file = (IFile) obj;                  
                }
                else if (obj instanceof IAdaptable){
    				IAdaptable a = (IAdaptable) obj;
    				IResource r = (IResource) a.getAdapter(IResource.class);
    				if (r != null && IResource.FILE == r.getType()) {
    					file= (IFile) r;
    				}
                }
            }
            if(file!=null){

                  project = (IPrologProject) file.getProject()
                                              .getNature(PDTCore.NATURE_ID);
                  
                
          		String masterSwitch = PDTCorePlugin.getDefault().getPreferenceValue(PDTCore.PREF_AUTO_CONSULT,"false");
          		  
                  action.setEnabled(project != null
                          && project.isPrologSource(file)
                          && ! "false".equalsIgnoreCase(masterSwitch));

                  action.setChecked(project!=null&&project.isAutoConsulted(file));

            }
        } catch (Throwable t) {
            Debug.report(t);
            throw new RuntimeException(t);
        }

    }

}
