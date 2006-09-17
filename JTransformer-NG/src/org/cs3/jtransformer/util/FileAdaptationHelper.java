package org.cs3.jtransformer.util;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
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
	private static final String START_TEMPLATE_VAR_TOKENS = "\\$\\{";
	private static final String END_TEMPLATE_VAR_TOKENS = "\\}";
	private static final String CAPT_GROUP_TOKEN = "CAPT_GROUP";
	
	public static final String REGEX_BACKSLASH_TOKEN = "\\\\\\\\";
	
	
	
	private String fileName;
	private boolean needsAdaptation;
	private Map regexPatternsWithNewStrings;
	private String tabuString;
	private List notAdaptedPatterns = new ArrayList();
	
	
	/**
	 * Only for testing
	 *
	 */
	public FileAdaptationHelper()
	{
	}
	
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

	/**
	 * Like <tt>CopyFileHelper(String, String, String)</tt> but here
	 * you can define more pattern/replacement string pairs.
	 * 
	 * @see CopyFileHelper(String, String, String)
	 * @param fileName
	 * @param regexPatternWithNewString
	 * @param tabuString Don't do anything if the tabu string exists in the matched String
	 */
	public FileAdaptationHelper(String fileName, Map regexPatternWithNewString, String tabuString)
	{
		this.fileName = fileName;
		this.needsAdaptation = true;
		this.regexPatternsWithNewStrings = regexPatternWithNewString;
		this.tabuString = tabuString;
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

	public String adaptContent(String content)
	{
		return adaptContent(content, this.regexPatternsWithNewStrings, this.tabuString);
	}
	
	public String adaptContent(String content, Map regexPatternsWithNewStrings)
	{
		return adaptContent(content, regexPatternsWithNewStrings, this.tabuString);
	}
	
	/**
	 * Adapts the given content String due to the given regex patterns
	 * and replacement Strings in the Map.
	 * 
	 * @param content The content as String
	 * @param regexPatternsWithNewStrings Map containing regex pattern as key and replacement String as value
	 * @return String The adapted content
	 */
	public String adaptContent(String content, Map regexPatternsWithNewStrings, String tabuString)
	{
		Iterator iterator = regexPatternsWithNewStrings.keySet().iterator();
		while( iterator.hasNext() )
		{
			String key = (String) iterator.next();
			String val = (String) regexPatternsWithNewStrings.get(key);
			
			Pattern pattern = Pattern.compile(key, Pattern.DOTALL&Pattern.UNIX_LINES);
			Matcher matcher = pattern.matcher(content);
			if( matcher.find() )
			{
				if ( tabuString == null || matcher.group().indexOf(tabuString) == -1 )
				{
					if( matcher.groupCount() > 0 )
					{
						for( int groupCount=1 ; groupCount <=matcher.groupCount() ; groupCount++ )
						{
							String captGroup = matcher.group(groupCount);
							captGroup = captGroup.replace("\\", REGEX_BACKSLASH_TOKEN);
							val = val.replaceAll(
									START_TEMPLATE_VAR_TOKENS + 
									CAPT_GROUP_TOKEN + "=" + groupCount +
									END_TEMPLATE_VAR_TOKENS,
									captGroup);
						}
					}
					content = content.replaceAll(key, val);
				}
			}
			else
			{
				notAdaptedPatterns.add(key);
			}
		}
		
		return content;
	}

	public List getNotAdaptedPatterns()
	{
		return notAdaptedPatterns;
	}
}
