package org.cs3.pdt.internal.views.lightweightOutline;


/**
 * String matcher that can match two patterns.
 *
 * @since 3.2
 */
class OrStringMatcher extends StringMatcher {

	private StringMatcher fMatcher1;
	private StringMatcher fMatcher2;

	OrStringMatcher(String pattern1, String pattern2, boolean ignoreCase, boolean foo) {
		super("", false, false); //$NON-NLS-1$
		fMatcher1= new StringMatcher(pattern1, ignoreCase, false);
		fMatcher2= new StringMatcher(pattern2, ignoreCase, false);
	}

	public boolean match(String text) {
		return fMatcher2.match(text) || fMatcher1.match(text);
	}

}