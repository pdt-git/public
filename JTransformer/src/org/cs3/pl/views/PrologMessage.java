/*
 * Created on 28.02.2004
 *
 * To change the template for this generated file go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
package org.cs3.pl.views;

/**
 * @author xproot
 *
 * To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
public class PrologMessage {
	
	String str;
	long line;
	String kind;
	public static final String WARNING = "WARNING:";
	public static final String ERROR = "ERROR:";
	protected static final String REFERENCE = "REFERENCE:";
	/**
	 * @return Returns the kind.
	 */
	public String getKind() {
		return kind;
	}
	/**
	 * @return Returns the line.
	 */
	public long getLine() {
		return line;
	}
	/**
	 * @param line The line to set.
	 */
	public void setLine(long line) {
		this.line = line;
	}
	/**
	 * @return Returns the str.
	 */
	public String getStr() {
		return str;
	}
	/**
	 * @param str The str to set.
	 */
	public void setStr(String str) {
		this.str = str;
	}
	/**
	 * @param str
	 * @param line
	 * @param kind
	 */
	public PrologMessage(String str, long line, String kind) {
		super();
		this.str = str;
		this.line = line;
		this.kind = kind;
	}
	
	public int getReferencedLine(){
		int start = str.indexOf(".pl:") + 4;
		int end = start;
		while(str.charAt(end) >= '0' && str.charAt(end) <= '9')
			end ++;
		return Integer.parseInt(str.substring(start,end));
	}
	/**
	 * @return
	 */
	public String getReferencedFilename() {
		int start = 0;
		start = kind.length();
		while(notPartOfFileName(str.charAt(start)))
			start++;
		int end = str.indexOf(".pl:") + 3;
		return str.substring(start, end);
		
	}
	/**
	 * @param c
	 * @return
	 */
	private boolean notPartOfFileName(char c) {
		switch(c) {
			case ' ':
			case '(':
			case '\t':
			case '\n':
			case '\r':
				return true;
			default:
				return false;
		}
	}
}
