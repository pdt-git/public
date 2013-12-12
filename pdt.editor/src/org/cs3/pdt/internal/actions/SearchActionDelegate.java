/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2004-2012, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

package org.cs3.pdt.internal.actions;

import java.util.ResourceBundle;

import org.cs3.pdt.common.metadata.Goal;
import org.cs3.pdt.internal.editors.PLEditor;
import org.cs3.prolog.common.logging.Debug;
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
						PLEditor editor = (PLEditor) getTextEditor();
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


