package org.cs3.pdt.internal.actions;

import org.cs3.pdt.console.PrologConsolePlugin;
import org.cs3.pdt.ui.util.UIUtils;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.IWorkbenchWindowActionDelegate;

public class HelpActionDelegate extends QueryAction implements IWorkbenchWindowActionDelegate {

	public HelpActionDelegate() {
		super(PrologConsolePlugin.getDefault().getPrologConsoleService().getActivePrologConsole().getPrologInterface(),"help", "opening prolog help","Show SWI-Prolog's built-in help.",null);
	}

	public void dispose() {
		// TODO Auto-generated method stub
		
	}

	public void init(IWorkbenchWindow window) {
		// TODO Auto-generated method stub
		
	}

	public void run(IAction action) {
		run();
		
	}

	public void selectionChanged(IAction action, ISelection selection) {
		// TODO Auto-generated method stub
		
	}
}
