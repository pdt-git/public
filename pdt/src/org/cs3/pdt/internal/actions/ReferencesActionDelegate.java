package org.cs3.pdt.internal.actions;

import java.util.ResourceBundle;

import org.cs3.pdt.PDT;
import org.cs3.pdt.UIUtils;
import org.cs3.pdt.core.IPrologProject;
import org.cs3.pdt.core.PDTCore;
import org.cs3.pdt.core.PDTCorePlugin;
import org.cs3.pdt.internal.editors.PLEditor;
import org.cs3.pdt.internal.search.PrologSearchQuery;
import org.cs3.pl.common.Debug;
import org.cs3.pl.metadata.Goal;
import org.cs3.pl.metadata.Predicate;
import org.eclipse.core.resources.IProjectNature;
import org.eclipse.search.ui.ISearchQuery;
import org.eclipse.search.ui.NewSearchUI;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.IWorkbenchWindowActionDelegate;
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
		
		UIUtils.getDisplay().asyncExec(new Runnable() {
			public void run() {
				try {
						PLEditor editor = (PLEditor) UIUtils
								.getActiveEditor();
						IFileEditorInput editorInput = (IFileEditorInput) editor.getEditorInput();
						IPrologProject plProject = (IPrologProject) editorInput.getFile().getProject().getNature(PDTCore.NATURE_ID);
						Goal data = editor.getSelectedPrologElement();
						if(data == null){
							Debug.warning("data is null");
							return;
						}
						
						Predicate[] p = plProject.getMetaInfoProvider().findPredicates(data);
						//FIXME: what about alternatives?
						if(p==null||p.length==0){
							return;
						}
						if(p.length>1){
							UIUtils.displayMessageDialog(UIUtils.getActiveEditor().getEditorSite().getShell(),
									"PDT Plugin", "Note: I found more than one predicate matching the signature \n" 
									+ data.getLabel()+"/"+ data.getArity()
											+ ".\nSorry, Code analysis is still work in progress. " +
													"For now i will ignore all but the first match.");
						}
						ISearchQuery query = new PrologSearchQuery(plProject.getMetaInfoProvider(),p[0]);
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
