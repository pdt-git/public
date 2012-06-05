package org.cs3.pdt.internal.actions;

import java.util.ResourceBundle;

import org.cs3.pdt.internal.editors.PLEditor;
import org.cs3.prolog.common.logging.Debug;
import org.cs3.prolog.metadata.Goal;
import org.cs3.prolog.ui.util.UIUtils;
import org.eclipse.search.ui.ISearchQuery;
import org.eclipse.search.ui.NewSearchUI;
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
						PLEditor editor = (PLEditor) UIUtils.getActiveEditor();
						Goal data = editor.getSelectedPrologElement();
						if(data == null){
							Debug.warning("data is null");
							return;
						}
						ISearchQuery query = connectSearchQuery(data);
						NewSearchUI.activateSearchResultView();
						NewSearchUI.runQueryInForeground(null,query);
					} catch (Exception e) {
						Debug.report(e);
					}
				}
	
			});
		}

	protected abstract ISearchQuery connectSearchQuery(Goal data);
	
	public void dispose() {
	}

}