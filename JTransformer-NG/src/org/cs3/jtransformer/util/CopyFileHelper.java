package org.cs3.jtransformer.util;

import java.util.regex.Pattern;

/**
 * Encapsulates some data used during file copy.
 * 
 * @author Mark Schmatz
 *
 */
public class CopyFileHelper
{
	private String fileName;
	private boolean needsAdaptation;
	private Pattern pattern;
	
	public CopyFileHelper(String fileName)
	{
		this.fileName = fileName;
		this.needsAdaptation = false;
		this.pattern = null;
	}
	
	public CopyFileHelper(String fileName, Pattern pattern)
	{
		this.fileName = fileName;
		this.needsAdaptation = true;
		this.pattern = pattern;
	}

	public String getFileName()
	{
		return fileName;
	}

	public boolean needsAdaptation()
	{
		return needsAdaptation;
	}

	public Pattern getPattern()
	{
		return pattern;
	}
}
