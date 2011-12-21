package org.cs3.pl.metadata;

import org.cs3.pl.common.Debug;
import org.cs3.pl.common.Util;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;

public class PredicateReadingUtilities {
	

	/**
	 * Returns true, if the character at the postion in the document
	 * does indicate the end of a term head("." or ":-"), else false.
	 * 
	 * @param document
	 * @param pos
	 * @return true if  head ends at pos
	 */
	public static boolean isEndOfHead(IDocument document, int pos) {
		try {
			if (document.getChar(pos) == '.')
				return true;
			if (document.getChar(pos) == ':' && document.getLength() > (pos + 1)
					&& document.getChar(pos + 1) == '-')
				return true;
		} catch (BadLocationException e) {
			Debug.report(e);
		}
		return false;
	}

	public static int findEndOfWhiteSpace(IDocument document, int offset,
			int end) throws BadLocationException {
		while (offset < end) {
			char c = document.getChar(offset);
			if (c != ' ' && c != '\t') {
				return offset;
			}
			offset++;
		}
		return end;
	}

	/**
	 * @param document
	 * @param end
	 * @param c
	 * @return
	 */
	public static int consume(IDocument document, int end, char endChar)
			throws BadLocationException {
		while (!(document.getChar(end) == endChar)) {
			char c = document.getChar(end);
			switch (c) {
			case '\\':
				end = end + 2;
				break;
			case '(':
				end = consume(document, end + 1, ')');
				break;
			case '[':
				end = consume(document, end + 1, ']');
				break;
			case '"':
				end = consumeString(document, end + 1, '"');
				break;
			case '\'':
				end = consumeString(document, end + 1, '\'');
				break;
			default:
				end++;
			}
		}
		return end + 1;
	}

	/**
	 * @param document
	 * @param i
	 * @param c
	 * @return
	 */
	public static int consumeString(IDocument document, int end, char endChar)
			throws BadLocationException {
		while (!(document.getChar(end) == endChar)) {
			char c = document.getChar(end);
			switch (c) {
			case '\\':
				end = end + 2;
				break;
			case '"':
				throw new RuntimeException(
						"This point should never been reached");
			default:
				end++;
			}
		}
		return end + 1;
	}

	// Set by findBeginOfPredicateName() and findEndOfPredicateName().
	// Used by isPredicateNameChar():
	private static boolean predicate_name_is_enclosed_in_quotes = false;
	
	public static int findBeginOfPredicateName(IDocument document, int begin)
			throws BadLocationException {
		int start = begin;
		while (Util.isPredicateNameChar(document.getChar(start)) && start > 0) {
			start--; // scan left until first non-predicate-name  char
		}
		start++; // start is now the position of the first predicate char
		if (document.getChar(start) == '\'') {
			predicate_name_is_enclosed_in_quotes = true;
			// start++; // quotes are not part of the name
		}
		return start;
	}

	public static int findEndOfPredicateName(IDocument document, int end)
			throws BadLocationException {
		
		if (predicate_name_is_enclosed_in_quotes) {
			// Accept any character up to the next simple quote:
			while ( document.getChar(end)!='\'' 
					&& end < document.getLength() ) {
				end++;
			}
			// Include the terminal quote
			end++;
			// Reset flag for next predicate name reading
			predicate_name_is_enclosed_in_quotes = false;
		} else {
			// Do not accept special characters that may only occur within quotes:
			while (Util.isNormalPredicateNameChar(document.getChar(end)) 
					&& end < document.getLength()) {
				end++;
			}
		}

		return end;
	}

	public static String extractFunctor(String predicateModuleAndName) {
		/*if(predicateModuleAndName.endsWith(":"))  //hier war mal was zur Sonderbehandlung, wenn : am ende...
			return predicateModuleAndName;*/
		String[] nameSegments = predicateModuleAndName.split(":");
		int length = nameSegments.length;
		if (length > 0)
			return nameSegments[length-1];
		else
			return predicateModuleAndName;      
	}

	public static String extractModule(String predicateModuleAndName) {
		/*if(predicateModuleAndName.endsWith(":"))  //hier war mal was zur Sonderbehandlung, wenn : am ende...
		return null;*/
	
		String[] nameSegments = predicateModuleAndName.split(":");
		int length2 = nameSegments.length;
		
		if (length2 >= 2) {
			return nameSegments[length2-2];
		} else {
			return null;
		}
	}
}