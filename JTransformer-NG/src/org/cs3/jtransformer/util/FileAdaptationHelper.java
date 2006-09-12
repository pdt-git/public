package org.cs3.jtransformer.util;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;


/**
 * Encapsulates some data used for adapting file contents.
 * 
 * @author Mark Schmatz
 *
 */
public class FileAdaptationHelper
{
	public static final String REGEX_BACKSLASH_TOKEN = "\\\\\\\\";
	
	private String fileName;
	private boolean needsAdaptation;
	private Map regexPatternsWithNewStrings;
	
	
	/**
	 * Use this constructor if no adaptation is needed.
	 * 
	 * @param fileName
	 */
	public FileAdaptationHelper(String fileName)
	{
		this.fileName = fileName;
		this.needsAdaptation = false;
		this.regexPatternsWithNewStrings = null;
	}
	
	/**
	 * Adaptation via <tt>regexPattern</tt> and <tt>newString</tt>.<br>
	 * Means that it is searched for any String matching <tt>regexPattern</tt>
	 * which is then replaced with <tt>newString</tt>.<br>
	 * Capturing groups are allowed. They are refrenced via
	 * <tt>${CAPT_GROUP=n}</tt> (<tt>n</tt> is number of the capturing
	 * group starting with <tt>1</tt>)
	 * 
	 * @param fileName
	 * @param regexPattern 
	 * @param newString
	 */
	public FileAdaptationHelper(String fileName, String regexPattern, String newString)
	{
		this.fileName = fileName;
		this.needsAdaptation = true;
		regexPatternsWithNewStrings = new HashMap();
		regexPatternsWithNewStrings.put(regexPattern, newString);
	}

	/**
	 * Like <tt>CopyFileHelper(String, String, String)</tt> but here
	 * you can define more pattern/replacement string pairs.
	 * 
	 * @see CopyFileHelper(String, String, String)
	 * @param fileName
	 * @param regexPatternWithNewString
	 */
	public FileAdaptationHelper(String fileName, Map regexPatternWithNewString)
	{
		this.fileName = fileName;
		this.needsAdaptation = true;
		this.regexPatternsWithNewStrings = regexPatternWithNewString;
	}

	public String getFileName()
	{
		return fileName;
	}

	/**
	 * Returns <tt>true</tt> if the file corresponding to this
	 * helper needs to be adapted; <tt>false</tt> otherwise.
	 * 
	 * @return boolean
	 */
	public boolean needsAdaptation()
	{
		return needsAdaptation;
	}

	/**
	 * Returns the Map which contains for each key (the pattern which is
	 * searched for) the corresponding replacement value.
	 * 
	 * @return Map
	 */
	public Map getRegexPatternsWithNewStrings()
	{
		return regexPatternsWithNewStrings;
	}

	/**
	 * Adapts the given content String due to the given regex patterns
	 * and replacement Strings in the Map.
	 * 
	 * @param content The content as String
	 * @param regexPatternsWithNewStrings Map containing regex pattern as key and replacement String as value
	 * @return String The adapted content
	 */
	public static String adaptContent(String content, Map regexPatternsWithNewStrings)
	{
		Iterator iterator = regexPatternsWithNewStrings.keySet().iterator();
		while( iterator.hasNext() )
		{
			String key = (String) iterator.next();
			String val = (String) regexPatternsWithNewStrings.get(key);
			
			Pattern pattern = Pattern.compile(key);
			Matcher matcher = pattern.matcher(content);
			if( matcher.find() )
			{
				if( matcher.groupCount() > 0 )
				{
					for( int cpc=1 ; cpc <=matcher.groupCount() ; cpc++ )
					{
						String captGroup = matcher.group(cpc);
						captGroup = captGroup.replace("\\", REGEX_BACKSLASH_TOKEN);
						val = val.replaceAll("\\$\\{CAPT_GROUP=" + cpc + "\\}", captGroup);
					}
				}
				content = content.replaceAll(key, val);
			}
		}
		
		return content;
	}
}
