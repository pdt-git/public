package org.cs3.pl.fileops;

/**
 * @author schulzs1
 *
 * An implementation of the IPrologWriter interface to enable looking into the
 * generated Stream. This is mainly useful for the implementation of the 
 * FactGenerator tests.
 */
public class MockPrologWriter implements IPrologWriter {
	
	int indention = 0;
	boolean indent = true;
	StringBuffer buf = new StringBuffer();
	private boolean interpret=false;
	private char clauseSeparator;
	
	/**
	 * @param c
	 */
	public MockPrologWriter(char clauseSeparator) {
		this.clauseSeparator = clauseSeparator;
	}

	public MockPrologWriter() {
		this('.');
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
		writeRule(string, param, null);
	}
	
	/**
	 * @see IPrologWriter#writeRule(String, String[], String[])
	 */
	
	public void writeRule(String string, String[] param, String[] condi) {
		StringBuffer buf = new StringBuffer();
		
		if (indent)
			for (int i = 0; i < indention; i++){
				buf.append('\t');
			}
		
		buf.append(string);
		buf.append("(");
		
		if (param != null && param.length > 0){
			for (int i = 0; i < param.length - 1; i++){
				buf.append(param[i]);
				buf.append(", ");
			}
		
			buf.append(param[param.length - 1]);
		}
		
		buf.append(")");
		
		if (condi != null && condi.length > 0){
			buf.append(" :- ");
			
			for (int i = 0; i < condi.length - 1; i++){
				buf.append(condi[i]);
				buf.append(", ");
			}
			
			buf.append(condi[condi.length - 1]);
		}
		buf.append( clauseSeparator + "\n");
	
		this.buf.append(buf.toString());
	}
	
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
