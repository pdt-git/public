package org.cs3.prolog.connector.common;

public class ParserUtils {

	/**
	 * @param c
	 * @return
	 */
	public static boolean isVarChar(char c) {
		if (c == '_')
			return true;
		if (c >= 'A' && c <= 'Z')
			return true;
		if (c >= 'a' && c <= 'z')
			return true;
		if (c >= '0' && c <= '9')
			return true;
		return false;
	}

	/**
	 * @param c
	 * @return true if prefix is a variable prefix (upper case letter or underscore)
	 */
	public static boolean isVarPrefix(char c) {
		return (Character.isUpperCase(c) || c == '_');
	}

	/**
	 * @param prefix
	 * @return true if prefix is a functor prefix (lower case letter)
	 */
	public static boolean isFunctorPrefix(String prefix) {
		if (prefix == null | prefix.length() == 0)
			return false;
		if (prefix.charAt(0) >= 'a' && prefix.charAt(0) <= 'z')
			return true;
	
		return false;
	}

	/**
	 * @param prefix
	 * @return
	 */
	public static boolean isVarPrefix(String prefix) {
		if (prefix.length() == 0)
			return false;
		return isVarPrefix(prefix.charAt(0));
	}

	/**
	 * Returns true if c is a valid character as part of a Prolog
	 * predicate name that is NOT enclosed in simple quotes.
	 * @param c character in question
	 * @return 
	 */
	static public boolean isNormalPredicateNameChar(char c) {
		if (c >= 'a' && c <= 'z') return true;
		if (c >= '0' && c <= '9') return true;
		if (c >= 'A' && c <= 'Z') return true;
		if (c == '_' || c == ':')  return true;
		return false;
	}

	/**
	 * Returns true if c is a character that may be contained in a Prolog
	 * predicate name that IS enclosed in simple quotes.
	 * @param c character in question
	 * @return 
	 */
	static public boolean isSpecialPredicateNameChar(char c) {
		return (c == '\''  
			 || c == '\\'
			 || c == '.' 
			 || c == '+' 
			 || c == '-' 
			 || c == '*' 
	         || c == '$'
	    // TODO: add all the other special characters!
		);
	}

	/**
	 * Returns true if c is a valid character as part of a Prolog
	 * predicate name (including module definition).
	 * @param c character in question
	 * @return 
	 */
	public static boolean isPredicateNameChar(char c) {
		return (isNormalPredicateNameChar(c) || isSpecialPredicateNameChar(c));
	}

	public static boolean isNonQualifiedPredicateNameChar(char c) {
		return isPredicateNameChar(c) && c != ':';
	}

	public static boolean isFunctorChar(char c) {
		if (c >= 'a' && c <= 'z')
			return true;
		if (c >= '0' && c <= '9')
			return true;
		if (c >= 'A' && c <= 'Z')
			return true;
		if (c == '_')
			return true;
	
		return false;
	}

	public static boolean isSingleSecondChar(char c) {
		if (c >= '0' && c <= '9')
			return true;
		if (c >= 'A' && c <= 'Z')
			return true;
		return false;
	}

}
