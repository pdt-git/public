/*******************************************************************************
 * Copyright (c) 2000, 2003 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/cpl-v10.html
 * 
 * Contributors:
 *     IBM Corporation - initial API and implementation
 * Modified by:
 *     Tobias Windeln
 *******************************************************************************/

package org.cs3.pdt.internal.editors;

import org.eclipse.jface.text.rules.IWordDetector;

/**
 * A Java aware word detector.
 */
public class WordDetector implements IWordDetector {

	/**
	 * @see IWordDetector#isWordStart
	 */
	@Override
	public boolean isWordStart(char c) {
		return Character.isJavaIdentifierStart(c);
	}
	
	/**
	 * @see IWordDetector#isWordPart
	 */
	@Override
	public boolean isWordPart(char c) {
		return Character.isJavaIdentifierPart(c);
	}
} 


