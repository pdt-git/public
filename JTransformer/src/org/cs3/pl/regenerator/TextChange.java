/*
 * Created on 10.08.2004
 *
 * TODO To change the template for this generated file go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
package org.cs3.pl.regenerator;

/**
 * @author xproot
 *
 * TODO To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
public class TextChange implements ITextChange {

	private String src;
	
	protected TextChange(String src){
		this.src = src;
	}
	
	public String getSource() {
		return src;
	}

}
