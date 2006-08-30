package org.cs3.jtransformer.util;


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
	private String regexPattern;
	private String newString;
	
	
	public CopyFileHelper(String fileName)
	{
		this.fileName = fileName;
		this.needsAdaptation = false;
		this.regexPattern = null;
		this.newString = null;
	}
	
	public CopyFileHelper(String fileName, String regexPattern, String newString)
	{
		this.fileName = fileName;
		this.needsAdaptation = true;
		this.regexPattern = regexPattern;
		this.newString = newString;
	}

	public String getFileName()
	{
		return fileName;
	}

	public boolean needsAdaptation()
	{
		return needsAdaptation;
	}

	public String getRegexPattern()
	{
		return regexPattern;
	}

	public String getNewString()
	{
		return newString;
	}
}
