package org.cs3.pdt.internal.views;

import java.util.EventObject;

public class PrologFileContentModelEvent extends EventObject {

	private static final long serialVersionUID = 1L;
	
	public PrologFileContentModel model;
	public Object parent;
	public Object[] children;

	private long timestamp=Long.MAX_VALUE;

	public PrologFileContentModelEvent(PrologFileContentModel model, Object parent, Object[] children) {
		super(model);
		this.model=model;
		this.parent=parent;
		this.children=children;
	}
	public PrologFileContentModelEvent(PrologFileContentModel model, Object parent, Object[] children,long timestamp) {
		super(model);
		this.model=model;
		this.parent=parent;
		this.children=children;
		this.timestamp=timestamp;
	}

	public PrologFileContentModelEvent(PrologFileContentModel model) {
		super(model);
		this.model=model;
	}
	public PrologFileContentModelEvent(PrologFileContentModel model,long timestamp) {
		super(model);
		this.model=model;
		this.timestamp=timestamp;
	}

	public boolean isObsolet() {
		return timestamp<model.getLastResetTime();
	}

}
