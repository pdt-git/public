/* $LICENSE_MSG$(ld) */

package org.cs3.pdt.internal.actions;

import java.util.ResourceBundle;

import org.cs3.pdt.PDT;
import org.cs3.pdt.internal.queries.ReferencesSearchQueryDirect;
import org.cs3.pdt.metadata.Goal;
import org.eclipse.search.ui.ISearchQuery;
import org.eclipse.ui.IWorkbenchWindowActionDelegate;
import org.eclipse.ui.texteditor.ITextEditor;


/**
 * @see IWorkbenchWindowActionDelegate
 */
public class FindReferencesActionDelegate extends SearchActionDelegate {
	public FindReferencesActionDelegate(ITextEditor editor) {
		super(ResourceBundle.getBundle(PDT.RES_BUNDLE_UI),FindReferencesActionDelegate.class.getName(), editor);
	}

	@Override
	protected ISearchQuery connectSearchQuery(Goal data) {
		ISearchQuery query = new ReferencesSearchQueryDirect(data);
		return query;
	}
}

