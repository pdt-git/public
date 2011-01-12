package org.cs3.pdt.internal.wizards;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.INewWizard;
import org.eclipse.ui.IWorkbench;


public class NewModuleCreationWizard extends Wizard implements INewWizard {

	private NewModuleCreationWizardPage fPage = null;

	public NewModuleCreationWizard() {
		super();
//		setDefaultPageImageDescriptor(AspectJImages.W_NEW_ASPECT
//				.getImageDescriptor());
//		setDialogSettings(AspectJUIPlugin.getDefault().getDialogSettings());
		setWindowTitle("Prolog Module Wizard");
	}

	/**
	 * Adds the NewAspectWizardCreationPage. If the page has already been added,
	 * this method does nothing (extra pages can be added by calling
	 * <code> addPage()
	 * </code>
	 */
	public void addPages() {
		setNeedsProgressMonitor(true);

		// Only initialse if the member is currently null - necessary for
		// testing - spyoung
		if (fPage == null) {
			fPage = new NewModuleCreationWizardPage();
			addPage(fPage);
			//fPage.init(getSelection());
		}
	}

	/**
	 * Complete generation of the new file, open it in the associated editor,
	 * and open the Cross References view, if desired.
	 */
	public boolean performFinish() {
		return true;
	}

	protected void finishPage(IProgressMonitor monitor)
			throws InterruptedException, CoreException {
		fPage.createModule(monitor); // use the full progress monitor
	}

	@Override
	public void init(IWorkbench workbench, IStructuredSelection selection) {
		// TODO Auto-generated method stub
		
	}

}
