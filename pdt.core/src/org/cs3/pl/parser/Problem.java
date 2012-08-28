/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Lukas Degener (among others)
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2004-2012, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

package org.cs3.pl.parser;

public class Problem {
	public String message;
	public int firstRow;
	public int firstColumn;
	public int lastRow;
	public int lastColumn;
	public int beginOffset;
	public int endOffset;	
	public int severity;
	public static final int INFO = 0;
	public static final int WARNING = 1;
	public static final int ERROR = 2;
}


