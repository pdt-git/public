package org.cs3.pdt.internal.views;

import java.io.File;
import java.util.Set;

import org.cs3.pdt.core.PEFHandle;


public interface PEFNode extends PEFHandle {
	
	public String getLabel();
	public File getFile();
	public int getStartPosition();	
	public int getEndPosition();
	public Set<String> getTags();
	public String getType();

}
