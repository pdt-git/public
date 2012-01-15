package org.cs3.pdt.internal.editors.breakpoints;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;

public class MarkerBackup {

	private IFile file;
	private int lineNumber;
	private int offset;
	private String id;

	public MarkerBackup(IResource file, int lineNumber, String id, int offset) {
		this.file = (IFile) file;
		this.lineNumber = lineNumber;
		this.id = id;
		this.offset = offset;
	}

	/**
	 * @return the file
	 */
	public IFile getFile() {
		return file;
	}

	/**
	 * @return the current line number
	 */
	public int getLineNumber() {
		return lineNumber;
	}

	/**
	 * @return the offset
	 */
	public int getOffset() {
		return offset;
	}

	/**
	 * @return the id
	 */
	public String getId() {
		return id;
	}
}
