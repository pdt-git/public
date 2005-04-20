package org.cs3.pdt.internal.actions;

import java.util.ResourceBundle;

import org.cs3.pdt.PDT;
import org.cs3.pdt.PDTPlugin;
import org.cs3.pdt.UIUtils;
import org.cs3.pdt.internal.editors.PLEditor;
import org.cs3.pl.common.Debug;
import org.cs3.pl.metadata.PrologElementData;
import org.cs3.pl.metadata.SourceLocation;
import org.cs3.pl.prolog.PrologSession;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.ui.IWorkbenchWindowActionDelegate;
import org.eclipse.ui.texteditor.ITextEditor;
import org.eclipse.ui.texteditor.TextEditorAction;

/**
 * @see IWorkbenchWindowActionDelegate
 */
public class FindPredicateActionDelegate extends TextEditorAction {
	private ITextEditor editor;

	final PDTPlugin plugin;

	/**
	 *  
	 */
	public FindPredicateActionDelegate(ITextEditor editor) {
		super(ResourceBundle.getBundle(PDT.RES_BUNDLE_UI),
				FindPredicateActionDelegate.class.getName(), editor); //$NON-NLS-1$
		this.editor = editor;
		plugin = PDTPlugin.getDefault();
	}

	/**
	 * @see IWorkbenchWindowActionDelegate#run
	 */
	public void run() {
		try {
			final PrologElementData data = ((PLEditor) editor)
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

	private void run_impl(PrologElementData data,IFile file) throws BadLocationException {

		PrologSession session = plugin.getPrologInterface().getSession();
		

		final SourceLocation location = plugin.getMetaInfoProvider()
				.getLocation(data.getLabel(), data.getArity(),
						file.getFullPath().toString());
		if (location == null) {
			UIUtils.displayMessageDialog(editor.getEditorSite().getShell(),
					"PDT Plugin", "Can't find predicate: " + data.getLabel()
							+ "/" //$NON-NLS-1$
							+ data.getArity()
							+ ".\nProbably it is a build in(TODO).");
			return;
		}
		UIUtils.showSourceLocation(location);

	}

}