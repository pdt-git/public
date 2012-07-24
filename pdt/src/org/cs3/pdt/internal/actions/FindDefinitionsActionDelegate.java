/* $LICENSE_MSG$(ld) */

package org.cs3.pdt.internal.actions;

import java.util.ResourceBundle;

import org.cs3.pdt.PDT;
import org.cs3.pdt.internal.queries.DefinitionsSearchQuery;
import org.cs3.pdt.metadata.Goal;
import org.eclipse.search.ui.ISearchQuery;
import org.eclipse.ui.IWorkbenchWindowActionDelegate;
import org.eclipse.ui.texteditor.ITextEditor;


/**
 * @see IWorkbenchWindowActionDelegate
 */
public class FindDefinitionsActionDelegate extends SearchActionDelegate {
	public FindDefinitionsActionDelegate(ITextEditor editor) {
		super(ResourceBundle.getBundle(PDT.RES_BUNDLE_UI),FindDefinitionsActionDelegate.class.getName(), editor);
	}

	@Override
	protected ISearchQuery connectSearchQuery(Goal data) {
		ISearchQuery query = new DefinitionsSearchQuery(data);
		return query;
	}
	
}

