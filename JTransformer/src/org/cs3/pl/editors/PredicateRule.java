/*
 * Created on 29.01.2004
 *
 * To change the template for this generated file go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
package org.cs3.pl.editors;

import org.eclipse.jface.text.rules.ICharacterScanner;
import org.eclipse.jface.text.rules.IPredicateRule;
import org.eclipse.jface.text.rules.IToken;
import org.eclipse.jface.text.rules.Token;

/**
 * @author xproot
 *
 * To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
public class PredicateRule implements IPredicateRule {

	/* (non-Javadoc)
	 * @see org.eclipse.jface.text.rules.IPredicateRule#getSuccessToken()
	 */
	IToken token;
	PLWhitespaceDetector wsdetector;
	
	public PredicateRule(IToken token) {
		this.token =token;
		wsdetector = new PLWhitespaceDetector();
	}
	
	public IToken getSuccessToken() {
		return token;
	}

	/* (non-Javadoc)
	 * @see org.eclipse.jface.text.rules.IPredicateRule#evaluate(org.eclipse.jface.text.rules.ICharacterScanner, boolean)
	 */
	public IToken evaluate(ICharacterScanner scanner, boolean resume) {
		int size = 0;
		int c =scanner.read();// = scanner.read();
		if (c < (int)'A' || c > (int)'z')
			return Token.UNDEFINED;
		while (c != '.' &&  c != ICharacterScanner.EOF) {
			c = scanner.read();
			size++;
		}
//		System.out.println("size: "+size);
//		scanner.unread();
		return getSuccessToken();
	}

	/* (non-Javadoc)
	 * @see org.eclipse.jface.text.rules.IRule#evaluate(org.eclipse.jface.text.rules.ICharacterScanner)
	 */
	public IToken evaluate(ICharacterScanner scanner) {
		return evaluate(scanner, false);
	}

}
