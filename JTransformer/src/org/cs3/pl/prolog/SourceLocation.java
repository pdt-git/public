/*
 * Created on 30.01.2004
 *
 * To change the template for this generated file go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
package org.cs3.pl.prolog;

import java.io.Serializable;

/**
 * @author xproot
 *
 * To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
public class SourceLocation implements Serializable {
	public int line;
	public String file;
	
	public String toString() {
		return line + "/" + line;
	}


	public SourceLocation() {
	}
	/**
	 * @param line
	 * @param file
	 */
	public SourceLocation(int line, String file) {
		super();
		this.line = line;
		this.file = file;
	}
}
