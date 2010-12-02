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

package org.cs3.pdt.internal.editors;

import org.cs3.pdt.core.IPrologProject;
import org.cs3.pdt.core.PDTCore;
import org.cs3.pdt.runtime.ui.AbstractPrologContextTracker;
import org.cs3.pdt.ui.util.UIUtils;
import org.cs3.pl.common.Debug;
import org.cs3.pl.prolog.PrologInterface;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IEditorReference;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.IPartListener2;
import org.eclipse.ui.IPartService;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPartReference;
import org.eclipse.ui.IWorkbenchWindow;


public class PLEditorTracker extends AbstractPrologContextTracker implements IPartListener2 {

	

	

	@Override
	public PrologInterface getCurrentPrologInterface(){
		IEditorPart editor = UIUtils.getActiveEditor();
		
		PLEditor plEditor=null;
		if (editor instanceof PLEditor) {			
			plEditor = (PLEditor) editor;
		}
		if(plEditor==null){
			return null;
		}
		IEditorInput input = plEditor.getEditorInput();
		IFileEditorInput fileInput=null;
		if (input instanceof IFileEditorInput) {			
			fileInput = (IFileEditorInput) input;			
		}
		if(fileInput==null){
			return null;
		}
		IProject project = fileInput.getFile().getProject();
		IPrologProject plProject=null;
		try {
			if(project.hasNature(PDTCore.NATURE_ID)){
				plProject=(IPrologProject) project.getNature(PDTCore.NATURE_ID);
			}
		} catch (CoreException e) {
			Debug.warning(e.getLocalizedMessage() + " - the project was removed from the workspace");
			//throw new RuntimeException(e);
		}
		if(plProject==null){
			return null;
		}
		
		return plProject.getRuntimePrologInterface();
	}

	@Override
	public void init(IWorkbench workbench) {
		IWorkbenchWindow activeWorkbenchWindow = workbench.getActiveWorkbenchWindow();
		IPartService partService = activeWorkbenchWindow.getPartService();
		partService.addPartListener(this);
		//FIXME: TRHO: Do not fire context changed on startup. Results in an unavailable prolog interface although another tracker was successful! 
		//fireContextChanged();
	}
	
	private void check(IWorkbenchPartReference partRef)  {
		if(partRef instanceof IEditorReference){
			if(getCurrentPrologInterface()!=null){
				fireContextChanged();
			}
		}
	}
	
	@Override
	public void partActivated(IWorkbenchPartReference partRef) {		
		check(partRef);		
	}	

	@Override
	public void partBroughtToTop(IWorkbenchPartReference partRef) {
		check(partRef);	
	}

	@Override
	public void partClosed(IWorkbenchPartReference partRef) {
		;//check(partRef);
	}

	@Override
	public void partDeactivated(IWorkbenchPartReference partRef) {
		;//check(partRef);
	}

	@Override
	public void partOpened(IWorkbenchPartReference partRef) {
		check(partRef);
	}

	@Override
	public void partHidden(IWorkbenchPartReference partRef) {
		;//check(partRef);
	}

	@Override
	public void partVisible(IWorkbenchPartReference partRef) {
		check(partRef);
	}

	@Override
	public void partInputChanged(IWorkbenchPartReference partRef) {
		check(partRef);
	}

	

}
