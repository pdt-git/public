/*
 * Created on 17.06.2004
 *
 * TODO To change the template for this generated file go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
package org.cs3.pl.doc;

import org.cs3.pl.prolog.PrologElementData;
import org.eclipse.core.resources.IFile;

/**
 * @author xproot
 *
 * TODO To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
public class PrologModule {
	
	private PrologElementData[] elements; 
	private String name;
	private IFile file;
	private String help;

	public PrologModule(String name, IFile file, String help, PrologElementData[] elements) {
		this.name = name;
		this.file = file;
		this.help = help;
		this.elements = elements;
	}
	
	/**
	 * @return Returns the elements.
	 */
	public PrologElementData[] getElements() {
		return elements;
	}
	/**
	 * @return Returns the filename.
	 */
	public String getFilename() {
		return file.getFullPath().toOSString();
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
