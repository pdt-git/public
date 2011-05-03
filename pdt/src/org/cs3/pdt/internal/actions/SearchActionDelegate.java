package org.cs3.pdt.internal.actions;

import java.util.ResourceBundle;

import org.cs3.pdt.core.IPrologProject;
import org.cs3.pdt.core.PDTCore;
import org.cs3.pdt.internal.editors.PLEditor;
import org.cs3.pdt.ui.util.UIUtils;
import org.cs3.pl.common.Debug;
import org.cs3.pl.metadata.Goal;
import org.eclipse.search.ui.ISearchQuery;
import org.eclipse.search.ui.NewSearchUI;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.IWorkbenchWindowActionDelegate;
import org.eclipse.ui.texteditor.ITextEditor;
import org.eclipse.ui.texteditor.TextEditorAction;

public abstract class SearchActionDelegate extends TextEditorAction {

	public SearchActionDelegate(ResourceBundle bundle, String prefix,
			ITextEditor editor) {
		super(bundle, prefix, editor);
	}

	public SearchActionDelegate(ResourceBundle bundle, String prefix,
			ITextEditor editor, int style) {
		super(bundle, prefix, editor, style);
	}

	/**
	 * @see IWorkbenchWindowActionDelegate#run
	 */
	@Override
	public void run() {
	
			UIUtils.getDisplay().asyncExec(new Runnable() {
				@Override
				public void run() {
					try {
						PLEditor editor = (PLEditor) UIUtils
						.getActiveEditor();
						IPrologProject plProject =null;
						if(editor.getEditorInput() instanceof IFileEditorInput){
							IFileEditorInput editorInput = (IFileEditorInput) editor.getEditorInput();
							plProject = (IPrologProject) editorInput.getFile().getProject().getNature(PDTCore.NATURE_ID);
						} 
	//					else {
	//						FileStoreEditorInput fStoreInput = (FileStoreEditorInput)editor.getEditorInput();
	//						//Path filepath = new Path(fStoreInput.getURI().getPath());
	//						IFile[] file = ResourcesPlugin.getWorkspace().getRoot().findFilesForLocationURI(fStoreInput.getURI());
	//					}
						Goal data = editor.getSelectedPrologElement();
						if(data == null){
							Debug.warning("data is null");
							return;
						}
						ISearchQuery query = connectSearchQuery(plProject, data);
						NewSearchUI.activateSearchResultView();
						NewSearchUI.runQueryInForeground(null,query);
						if(plProject!=null) plProject.updateMarkers();
					} catch (Exception e) {
						Debug.report(e);
					}
				}
	
			});
		}

	abstract protected ISearchQuery connectSearchQuery(IPrologProject plProject,
			Goal data);
	
	public void dispose() {
	}

}