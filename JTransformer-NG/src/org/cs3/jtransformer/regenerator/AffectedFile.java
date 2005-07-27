/*
 * Created on 10.08.2004
 *
 */
package org.cs3.jtransformer.regenerator;

/**
 * @author Tobias Rho
 *
 */
public class AffectedFile implements IAffectedFile {

	private int status;
	private String filename;
	private ITextChange[] changes;

	/**
	 * @param object
	 * @param removed
	 */
	public AffectedFile(String filename, int status) {
		this(filename,status,null);
		
	}

	/**
	 * @param filename2
	 * @param src
	 * @param removed
	 */
	public AffectedFile(String filename, int status, ITextChange[] changes) {
		this.filename =filename;
		this.status = status;
		this.changes = changes;
	}

	/* 
	 * @see org.cs3.jtransformer.regenerator.IAffectedType#getChangesStatus()
	 */
	public int getStatus() {
		return status;
	}

	public void setStatus(int status) {
		this.status = status;
	}
	
	/* 
	 * @see org.cs3.jtransformer.regenerator.IAffectedType#getFullyQualifiedName()
	 */
	public String getFilename() {
		// TODO Auto-generated method stub
		return filename;
	}

	public void setFilename(String name) {
		this.filename = name;
	}
	/* 
	 * @see org.cs3.jtransformer.regenerator.IAffectedType#getTextChanges()
	 */
	public ITextChange[] getTextChanges() {
		// TODO Auto-generated method stub
		return changes;
	}

}
