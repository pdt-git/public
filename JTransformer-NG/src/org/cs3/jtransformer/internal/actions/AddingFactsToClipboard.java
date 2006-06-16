package org.cs3.jtransformer.internal.actions;

import java.util.HashSet;
import java.util.Set;

import org.cs3.jtransformer.internal.astvisitor.VariableIdResolver;
import org.cs3.jtransformer.internal.astvisitor.VariableTypeResolver;
import org.cs3.jtransformer.internal.bytecode.ITypeFQNManager;

public class AddingFactsToClipboard extends AbstractSelectionToClipboard {

	AbstractStringBufferWriter getStringBufferWriter() {
		Set filter = new HashSet();
		filter.add("slT");
		return new AddingFactsWriter(filter, ',');
	}

	VariableTypeResolver getTypeResolver(VariableIdResolver idResolver) {
		return new AddingVariableTypeResolver(new ITypeFQNManager(idResolver), idResolver);
	}

}
