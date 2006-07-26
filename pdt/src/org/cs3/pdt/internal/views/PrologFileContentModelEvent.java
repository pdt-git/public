package org.cs3.pdt.internal.views;

import java.util.EventObject;

public class PrologFileContentModelEvent extends EventObject {

	private static final long serialVersionUID = 1L;
	
	public ContentModel model;
	public Object parent;
	public Object[] children;

	public PrologFileContentModelEvent(ContentModel model, Object parent, Object[] children) {
		super(model);
		this.model=model;
		this.parent=parent;
		this.children=children;
	}

}
