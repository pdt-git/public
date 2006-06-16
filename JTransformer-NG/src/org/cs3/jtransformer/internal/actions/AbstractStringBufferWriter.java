package org.cs3.jtransformer.internal.actions;

import java.util.Set;

import org.cs3.jtransformer.internal.astvisitor.IPrologWriter;

/**
 * @author schulzs1
 *
 * An implementation of the IPrologWriter interface to enable looking into the
 * generated Stream. This is mainly useful for the implementation of the 
 * FactGenerator tests.
 */
public abstract class AbstractStringBufferWriter implements IPrologWriter {
	
	int indention = 0;
	boolean indent = true;
	StringBuffer buf = new StringBuffer();
	private boolean interpret=false;
	private char clauseSeparator;
	private Set filter;
	
	public AbstractStringBufferWriter(Set filter,char clauseSeparator){
		this.filter = filter;
		this.clauseSeparator = clauseSeparator;
	}

	/**
	 * @see IPrologWriter#setIndentionLevel(int)
	 */
	
	public void disableIndention(){
		indent = false;
	}
	
	public void setIndentionLevel(int i) {
		if (i < 0)
			throw new IllegalArgumentException("Negative Indention");
		indention = i;
	}
	
	/**
	 * @see IPrologWriter#addIndention()
	 */
	
	public void addIndention() {
		indention++;
	}
	
	/**
	 * @see IPrologWriter#reduceIndention()
	 */
	
	public void reduceIndention() {
		if (indention - 1 < 0)
			throw new IllegalStateException("Negative Indention");
		indention--;
	}
	
	/**
	 * @see IPrologWriter#writeFact(String, String[])
	 */
	
	public void writeFact(String string, String[] param) {
		if(!filter.contains(string))
			writeRule(string, param, null);
	}
	
	/**
	 * @see IPrologWriter#writeRule(String, String[], String[])
	 */
	
	public abstract void writeRule(String string, String[] param, String[] condi);
	
	/**
	 * This method removes any saved information from the buffer, and resets it
	 * to an empty string.
	 */
	
	public void reset(){
		buf = new StringBuffer();
	}
	
	/**
	 * This method returns the facts and rules written to the Interface since
	 * the creation of the Object, or the last call to reset(). This does <b>
	 * not</b> reset the buffer!
	 * @return the last string, including any newlines.
	 */
	
	public String getLast(){
		return buf.toString();
	}
	
	/**
	 * @see IPrologWriter#close()
	 */
	
	public void close() {
		
	}

	/* (non-Javadoc)
	 * @see org.cs3.pl.fileops.IPrologWriter#flush()
	 */
	public void flush() {
		//nothing to do :-)
		
	}

	/* (non-Javadoc)
	 * @see org.cs3.pl.fileops.IPrologWriter#setInterpretMode(boolean)
	 */
	public void setInterpretMode(boolean interpret) {
		this.interpret = interpret;
		
	}

	/* (non-Javadoc)
	 * @see org.cs3.pl.fileops.IPrologWriter#getInterpretMode()
	 */
	public boolean getInterpretMode() {
		return interpret;
	}

	/* (non-Javadoc)
	 * @see org.cs3.pl.fileops.IPrologWriter#writeQuery(java.lang.String)
	 */
	public void writeQuery(String query) {
		buf.append(":- " + query +".");
	}
}
