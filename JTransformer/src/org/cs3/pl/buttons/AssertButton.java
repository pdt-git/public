package org.cs3.pl.buttons;

import java.io.IOException;

import org.cs3.pl.Debug;
import org.cs3.pl.PDTPlugin;
import org.cs3.pl.prolog.PrologHelper;
import org.cs3.pl.prolog.PrologManager;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.dialogs.IInputValidator;
import org.eclipse.jface.dialogs.InputDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.window.Window;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.IWorkbenchWindowActionDelegate;

/**
 * @see IWorkbenchWindowActionDelegate
 */
public class AssertButton implements IWorkbenchWindowActionDelegate {
	public static boolean tracemode = false;

	/**
	 *
	 */
	public AssertButton() {
	}

	/**
	 * @see IWorkbenchWindowActionDelegate#run
	 */
	public void run(IAction action)  {
		IInputValidator inputValidator = new IInputValidator() {
			public String isValid(String newText) {
				if (newText == null)
					return "Can not assert empty string!";
				if (newText == null)
					return "Can not assert empty string!";
				if (newText.endsWith("."))
					return "A point at the end of the term is not valid!";
				
				return null;
			}
		};
		InputDialog dialog= new InputDialog(PDTPlugin.getShell(), 
				"PDTPlugin", "assert the fact", null, inputValidator);
		if (dialog.open() != Window.CANCEL) {
			final String value= dialog.getValue();
			String msg;
			if ((msg = isValid(value)) == null) {
				Thread thread = new Thread() {
					public void run() {
						try {
							boolean ret = new PrologHelper(PrologManager.getInstance().getHiddenClient()).assertFact(value);
						} catch (IOException e) {
							Debug.report(e);
						}
						PDTPlugin.getDefault().getPrologConsole().appendToPrologConsole("\n?- assert("+value+").");
					}
				};
				thread.start();
				
			}
			else
				MessageDialog.openInformation(PDTPlugin.getShell(),"PDT Plugin", msg);
		}
	}

	public String isValid(String newText) {
		if (newText == null)
			return "Can not assert empty string!";
		if (newText.length() == 0)
			return "Can not assert empty string!";
		if (newText.endsWith("."))
			return "A point at the end of the term is not valid!";
		
		return null;
	}
	
	/**
	 * @see IWorkbenchWindowActionDelegate#selectionChanged
	 */
	public void selectionChanged(IAction action, ISelection selection)  {
	}

	/**
	 * @see IWorkbenchWindowActionDelegate#dispose
	 */
	public void dispose()  {
	}

	/**
	 * @see IWorkbenchWindowActionDelegate#init
	 */
	public void init(IWorkbenchWindow window)  {
	}
}
