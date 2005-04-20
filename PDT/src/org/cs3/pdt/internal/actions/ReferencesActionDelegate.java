package org.cs3.pdt.internal.actions;

import java.util.ResourceBundle;

import org.cs3.pdt.PDT;
import org.cs3.pdt.PDTPlugin;
import org.cs3.pdt.UIUtils;
import org.cs3.pdt.internal.editors.PLEditor;
import org.cs3.pdt.internal.search.PrologSearchQuery;
import org.cs3.pl.common.Debug;
import org.cs3.pl.metadata.PrologElementData;
import org.eclipse.search.ui.ISearchQuery;
import org.eclipse.search.ui.NewSearchUI;
import org.eclipse.ui.texteditor.ITextEditor;
import org.eclipse.ui.texteditor.TextEditorAction;


/**
 * @see IWorkbenchWindowActionDelegate
 */
public class ReferencesActionDelegate extends TextEditorAction {
	public ReferencesActionDelegate(ITextEditor editor) {
		super(ResourceBundle.getBundle(PDT.RES_BUNDLE_UI),ReferencesActionDelegate.class.getName(), editor);
	}
	
	/**
	 * @see IWorkbenchWindowActionDelegate#run
	 */
public void run() {
		final PDTPlugin plugin = PDTPlugin.getDefault();
		UIUtils.getDisplay().asyncExec(new Runnable() {
			public void run() {
				try {
						PrologElementData data = ((PLEditor) UIUtils
								.getActiveEditor()).getSelectedPrologElement();
						if(data == null){
							Debug.warning("data is null");
							return;
						}
						
						ISearchQuery query = new PrologSearchQuery(data);
						NewSearchUI.activateSearchResultView();
						NewSearchUI.runQuery(query);

				} catch (Exception e) {
					Debug.report(e);
				}
			}
		});
	}
	
	public void dispose() {
	}
	
	
}
