package org.cs3.jtransformer.internal.actions;

import java.util.HashSet;
import java.util.Set;

import org.cs3.jtransformer.internal.astvisitor.VariableIdResolver;
import org.cs3.jtransformer.internal.astvisitor.VariableTypeResolver;
import org.cs3.jtransformer.internal.bytecode.ITypeFQNManager;
import org.eclipse.ui.IWorkbenchWindowActionDelegate;

/**
 * @see IWorkbenchWindowActionDelegate
 */
public class FactsToClipboard extends AbstractSelectionToClipboard  {
	/**
	 *
	 */
	public FactsToClipboard() {
	}

	AbstractStringBufferWriter getStringBufferWriter() {
		Set filter = new HashSet();
		filter.add("slT");
		return new FilteredStringBufferWriter(filter, ',');
	}

	VariableTypeResolver getTypeResolver(VariableIdResolver idResolver) {
		return new VariableTypeResolver(new ITypeFQNManager(idResolver), idResolver);
	}


}
