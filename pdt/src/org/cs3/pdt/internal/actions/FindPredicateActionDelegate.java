package org.cs3.pdt.internal.actions;

import java.util.ResourceBundle;

import org.cs3.pdt.PDT;
import org.cs3.pdt.UIUtils;
import org.cs3.pdt.core.IPrologProject;
import org.cs3.pdt.core.PDTCore;
import org.cs3.pdt.core.PDTCorePlugin;
import org.cs3.pdt.internal.editors.PLEditor;
import org.cs3.pl.common.Debug;
import org.cs3.pl.metadata.Clause;
import org.cs3.pl.metadata.Goal;
import org.cs3.pl.metadata.IMetaInfoProvider;
import org.cs3.pl.metadata.Predicate;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.ui.IWorkbenchWindowActionDelegate;
import org.eclipse.ui.texteditor.ITextEditor;
import org.eclipse.ui.texteditor.TextEditorAction;

/**
 * @see IWorkbenchWindowActionDelegate
 */
public class FindPredicateActionDelegate extends TextEditorAction {
	private ITextEditor editor;

	

	/**
	 *  
	 */
	public FindPredicateActionDelegate(ITextEditor editor) {
		super(ResourceBundle.getBundle(PDT.RES_BUNDLE_UI),
				FindPredicateActionDelegate.class.getName(), editor); //$NON-NLS-1$
		this.editor = editor;

	}

	/**
	 * @see IWorkbenchWindowActionDelegate#run
	 */
	public void run() {
		try {
			final Goal data = ((PLEditor) editor)
					.getSelectedPrologElement();
			if (data == null) {
				UIUtils.displayMessageDialog(editor.getEditorSite().getShell(),
						"PDT Plugin",
						"Cannot locate a predicate at the specified location.");
				return;
			}
			final IFile file = UIUtils.getFileInActiveEditor();
			
			
			Job j = new Job("Searching predicate definition") {
				protected IStatus run(IProgressMonitor monitor) {
					try {
						monitor.beginTask("searching...",
								IProgressMonitor.UNKNOWN);
						run_impl(data,file);

					} catch (Throwable e) {
						Debug.report(e);
						return Status.CANCEL_STATUS;
					} finally {
						monitor.done();
					}
					return Status.OK_STATUS;
				}

				
			};
			j.schedule();
		} catch (Throwable t) {
			Debug.report(t);
		}

	}

	public void dispose() {
	}

	private void run_impl(Goal goal, IFile file) {
		IPrologProject plprj;
		try {
			plprj = (IPrologProject) file.getProject().getNature(PDTCore.NATURE_ID);
		} catch (CoreException e) {
			Debug.report(e);
			throw new RuntimeException(e);
		}
		IMetaInfoProvider mip = plprj.getMetaInfoProvider();
		Predicate[] predicates = mip.findPredicates(goal);
		//FIXME: what about alternatives?
		if(predicates==null||predicates.length==0){
			UIUtils.displayMessageDialog(editor.getEditorSite().getShell(),
					"PDT Plugin", "Can't find predicate: " + goal.getLabel()
							+ "/" //$NON-NLS-1$
							+ goal.getArity()
							+ ".\nSorry, Code analysis is still work in progress.");
			return;
		}
		if(predicates.length>1){
			UIUtils.displayMessageDialog(editor.getEditorSite().getShell(),
					"PDT Plugin", "Note: I found more than one predicate matching the signature \n" 
					+ goal.getLabel()+"/"+ goal.getArity()
							+ ".\nSorry, Code analysis is still work in progress. " +
									"For now i will just take you " +
									"to the first match found.");
		}
		Clause[] clauses = mip.findClauses(predicates[0]);
		if(clauses==null || clauses.length==0){
			UIUtils.displayMessageDialog(editor.getEditorSite().getShell(),
					"PDT Plugin", "Can't find clauses for predicate: " + goal.getLabel()
							+ "/" //$NON-NLS-1$
							+ goal.getArity()
							+ ".\nSorry, Code analysis is still work in progress.");
			return;
		}
		UIUtils.showSourceLocation(clauses[0].getKnownDefinition());
	}
	
	

}