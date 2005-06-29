package org.cs3.jlmp.internal.astvisitor;

import java.util.List;

public interface GenericAnnotation {

	String getName();
	List getArgs();
	String getPredicate();
}
