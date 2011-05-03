package org.cs3.pl.metadata;

import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;

public class GoalDataProvider {

	/**
	 * @param document
	 * @param offset
	 * @return
	 */
	public static Goal getPrologDataFromOffset(String file, IDocument document,
			int offset) throws BadLocationException {
	
		int start = PredicateReadingUtilities.findBeginOfPredicateName(document, offset);
		int end = PredicateReadingUtilities.findEndOfPredicateName(document, offset);
	
		if (start > end) {
			return null;
		}
		String predicateModuleAndName = document.get(start, end - start);
		
		String functor = PredicateReadingUtilities.extractFunctor(predicateModuleAndName);
		String module= PredicateReadingUtilities.extractModule(predicateModuleAndName);
	
		int endOfWhiteSpace = PredicateReadingUtilities.findEndOfWhiteSpace(document, end, document
				.getLength());
		int endOfTerm = endOfWhiteSpace;
		int arity;
		
		if ((document.getLength() == endOfWhiteSpace)  ||
				 (    (document.getChar(endOfWhiteSpace) != '(')
				   && (document.getChar(endOfWhiteSpace) != '/'))){
			
			arity=0;
			
		} else if (document.getChar(endOfWhiteSpace) == '/') {
			endOfTerm = endOfWhiteSpace + 1;
			String arityBuffer = "";
			while (endOfTerm < document.getLength() 
					&& document.getChar(endOfTerm) >= '0'
					&& document.getChar(endOfTerm) <= '9') {
				arityBuffer += document.getChar(endOfTerm);
				endOfTerm++;
			}
			
			if (arityBuffer.length() == 0)
				return null;
			arity = Integer.parseInt(arityBuffer);
		} else {   // document.getChar(endOfWhiteSpace) == '(')
			endOfTerm = endOfWhiteSpace + 1;
			arity=1;
			whileLoop: while (!PredicateReadingUtilities.isEndOfHead(document, endOfTerm)) {
				char c = document.getChar(endOfTerm);
				switch (c) {
				case '(':
					endOfTerm = PredicateReadingUtilities.consume(document, endOfTerm + 1, ')');
					break;
				case '[':
					endOfTerm = PredicateReadingUtilities.consume(document, endOfTerm + 1, ']');
					break;
				case '"':
					endOfTerm = PredicateReadingUtilities.consumeString(document, endOfTerm + 1, '"');
					break;
				case '\'':
					endOfTerm = PredicateReadingUtilities.consumeString(document, endOfTerm + 1, '\'');
					break;
				case ',':
					arity++;
					endOfTerm++;
					break;
				case ')':
					break whileLoop;
				default:
					endOfTerm++;
				}
			};
			endOfTerm++;
		}
		String term = document.get(start, endOfTerm - start);
		return new Goal(file,module, functor, arity, term);

	}

}
