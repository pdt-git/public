package org.cs3.jtransformer.internal.actions;

import java.util.ArrayList;
import java.util.List;

import org.cs3.jtransformer.JTransformer;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.IWorkbenchWindowActionDelegate;
import org.eclipse.ui.PlatformUI;

public class RemoveAllNaturesAction implements IWorkbenchWindowActionDelegate {

	public void dispose() {
		// TODO Auto-generated method stub

	}

	public void init(IWorkbenchWindow window) {
		// TODO Auto-generated method stub

	}

	public void run(IAction action) {
		IProject[] projects = ResourcesPlugin.getWorkspace().getRoot()
				.getProjects();
		List projectsWithNature = new ArrayList();
		try {
			for (int i = 0; i < projects.length; i++) {
				if (projects[i].isOpen() && projects[i].hasNature(JTransformer.NATURE_ID)) {
					projectsWithNature.add(projects[i]);
				}
			}

			if (projectsWithNature.size() == 0) {
				MessageBox message = new MessageBox(PlatformUI.getWorkbench()
						.getDisplay().getActiveShell(), SWT.ICON_INFORMATION);
				message.setMessage("No project with JTransformer nature found");
				message.setText("Remove all natures");
				message.open();
				return;
			}
			JTransformerNatureAssigner assigner = new JTransformerNatureAssigner(
					projectsWithNature);
			assigner.askAndRemoveNatures();
		} catch (CoreException e) {
			e.printStackTrace();
		}
	}

	public void selectionChanged(IAction action, ISelection selection) {
		// TODO Auto-generated method stub

	}

}
