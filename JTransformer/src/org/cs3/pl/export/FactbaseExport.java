/*
 * Created on 08.09.2004
 *
 * TODO To change the template for this generated file go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
package org.cs3.pl.export;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.Hashtable;

import org.cs3.pl.Debug;
import org.cs3.pl.PDTPlugin;
import org.cs3.pl.prolog.IPrologClient;
import org.cs3.pl.prolog.PrologClient;
import org.cs3.pl.prolog.PrologManager;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.ui.IObjectActionDelegate;
import org.eclipse.ui.IWorkbenchPart;


/**
 * @author linder
 *
 * TODO To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
public class FactbaseExport implements IObjectActionDelegate {
	
	/**
	 * @author linder
	 *
	 * TODO To change the template for this generated type comment go to
	 * Window - Preferences - Java - Code Style - Code Templates
	 */

	public IJavaProject currentSelection = null;
	private IStructuredSelection structuredSelection;

	/**
	 * 
	 */
	public FactbaseExport() {
	}

	/* (non-Javadoc)
	 * @see org.eclipse.ui.IObjectActionDelegate#setActivePart(org.eclipse.jface.action.IAction, org.eclipse.ui.IWorkbenchPart)
	 */
	public void setActivePart(IAction action, IWorkbenchPart targetPart) {}
	


	/* (non-Javadoc)
	 * @see org.eclipse.ui.IActionDelegate#run(org.eclipse.jface.action.IAction)
	 */
	public void run(IAction action) {
			
		FileDialog input = null;
			
			input = new FileDialog(PDTPlugin.getShell(),0);
			
			String projectName  = currentSelection.getElementName();
			String filterPath;
			try {
				filterPath = PDTPlugin.getDefault().getLocation();
		} catch (FileNotFoundException e2) {
			filterPath = PDTPlugin.getDefault().getStateLocation().toOSString();
			//e2.printStackTrace();
			//MessageDialog.openError(CultivatePlugin.getDefault().getShell(),"Cultivate","Coud not find the cultivate project location.");
		}
		input.setFilterPath(filterPath /*"src"+File.separator+"test"+File.separator+"factbases"+File.separator*/);
			input.setFileName(projectName+".pl");
			input.setText("Please provide a filename, where to save the factbase of the project "+ projectName);
			input.setFilterExtensions(new String[] {".pl"});
		
		final String filename = input.open();
		if(filename == null) {
			return;
		} 
		try {
		    ExportProject export = new ExportProject(currentSelection.getProject());
		    export.export(filename);
		} catch(CoreException ce){
		    Debug.report(ce);
		}
	}

	/* (non-Javadoc)
	 * @see org.eclipse.ui.IActionDelegate#selectionChanged(org.eclipse.jface.action.IAction, org.eclipse.jface.viewers.ISelection)
	 */
	public void selectionChanged(IAction action, ISelection selection) {
		structuredSelection = (IStructuredSelection) selection;
		if (structuredSelection.size() > 1) 
		{
			MessageDialog.openError(PDTPlugin.getShell(),
					"Warning",
					"More than one Java Project selected.");
			return;
		}
		currentSelection = (IJavaProject) structuredSelection.getFirstElement();
	}

}
