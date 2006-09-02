package org.cs3.jtransformer.util;

import java.util.HashMap;
import java.util.Map;


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
	private Map regexPatternsWithNewStrings;
	
	
	public CopyFileHelper(String fileName)
	{
		this.fileName = fileName;
		this.needsAdaptation = false;
		this.regexPatternsWithNewStrings = null;
	}
	
	public CopyFileHelper(String fileName, String regexPattern, String newString)
	{
		this.fileName = fileName;
		this.needsAdaptation = true;
		regexPatternsWithNewStrings = new HashMap();
		regexPatternsWithNewStrings.put(regexPattern, newString);
	}

	public CopyFileHelper(String fileName, Map regexPatternWithNewString)
	{
		this.fileName = fileName;
		this.needsAdaptation = true;
		this.regexPatternsWithNewStrings = regexPatternWithNewString;
	}

	public String getFileName()
	{
		return fileName;
	}

	public boolean needsAdaptation()
	{
		return needsAdaptation;
	}

	/**
	 * Returns the Map which contains for each key (the pattern which is to
	 * be searched) a replacement value.
	 * 
	 * @return Map
	 */
	public Map getRegexPatternsWithNewStrings()
	{
		return regexPatternsWithNewStrings;
	}
}
