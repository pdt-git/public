package org.cs3.pdt.internal.views;

import java.io.File;
import java.util.Set;


public interface PEFNode {
	public String getType();
	public String getLabel();
	public File getFile();
	public int getStartPosition();	
	public int getEndPosition();
	public Set getTags();
	public int getId();
	
	
}
