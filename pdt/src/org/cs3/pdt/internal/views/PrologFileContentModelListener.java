package org.cs3.pdt.internal.views;

public interface PrologFileContentModelListener {

	/*
	 * TODO: to make this more generic, we should drop the assumption
	 * that the affected nodes are sibblings. 
	 */

	public void childrenAdded(PrologFileContentModelEvent e);
	public void childrenRemoved(PrologFileContentModelEvent e);
	public void childrenChanged(PrologFileContentModelEvent e);

}
