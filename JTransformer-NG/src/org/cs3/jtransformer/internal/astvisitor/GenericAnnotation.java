package org.cs3.jtransformer.internal.astvisitor;

import java.util.List;

public interface GenericAnnotation {

	String getName();
	List getArgs();
	String getPredicate();
	void setInline(boolean isInlineComment);
	/**
	 * The annotation is inlined. This means it
	 * is a annotation on a statement or an expression.
	 * 
	 * @return
	 */
	boolean isInline();
}
