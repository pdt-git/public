/* $LICENSE_MSG$(ld) */

package org.cs3.pdt.internal.editors;

import org.eclipse.jface.text.rules.IWhitespaceDetector;

public class PLWhitespaceDetector implements IWhitespaceDetector {

	@Override
	public boolean isWhitespace(char c) {
		return (c == ' ' || c == '\t' || c == '\n' || c == '\r' || c == ',' || c == ':' ||
				c == '-' || c == '+' || c == ';' || c == '\\' || c == '/' ||  c == '=' ||
				c == '(' ||  c == ')' || c == '[' ||  c == ']' ||  c == '|');
	}
}

