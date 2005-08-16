/*
 * Created on 17.06.2004
 *
 * TODO To change the template for this generated file go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
package org.cs3.pl.doc;

import org.cs3.pl.metadata.Predicate;

/**
 * @author xproot
 *
 * TODO To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
public class PrologModule {
	
	private Predicate[] elements; 
	private String name;
	private String fileName;
	private String help;

	public PrologModule(String name, String filename, String help, Predicate[] elements) {
		this.name = name;
		this.fileName = filename;
		this.help = help;
		this.elements = elements;
	}
	
	/**
	 * @return Returns the elements.
	 */
	public Predicate[] getElements() {
		return elements;
	}
	/**
	 * @return Returns the filename.
	 */
	public String getFilename() {
		return fileName;
	}
	/**
	 * @return Returns the name.
	 */
	public String getName() {
		return name;
	}
	/**
	 * @return Returns the help.
	 */
	public String getHelp() {
		return help;
	}
	/**
	 * @param help The help to set.
	 */
	public void setHelp(String help) {
		this.help = help;
	}

	/**
	 * @return
	 */
	public String getHelpFilename() {
		return getFilename() + ".html";
	}
}
