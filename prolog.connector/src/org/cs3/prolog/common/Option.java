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

/*
 */
package org.cs3.prolog.common;

/**
 */
public interface Option {
	public final static int FLAG = 0;

	public final static int NUMBER = 1;

	public final static int STRING = 2;

	public final static int FILE = 3;

	public final static int DIR = 4;

	public final static int FILES = 5;

	public final static int DIRS = 6;

//	public final static int PATH = 7;

	public static final int ENUM = 8;

//	public static final int FONT = 9;
	
//	public static final int COLOR = 10;

	public String getDefault();

	public String getDescription();

	public String getId();

	public String getLabel();

	public String[][] getEnumValues();

	public int getType();

	/**
	 * 
	 * @param The
	 *            value to be validated
	 * @return An error message, if the given value is invalid. An empty String,
	 *         if the value is valid. null if the option does not perform
	 *         validation. Note: if you want to express that validation is not
	 *         neccessary, you should always return an empty string, since
	 *         otherwise ui classes (field editors, etc) may perform there own
	 *         validation.
	 */
	public String validate(String value);
	
	/**
	 * options can be hidden from the ui by letting this method return
	 * false.
	 * @return
	 */
	public boolean isVisible();
	
	/**
	 * options can be made read-only in the ui by letting this method return
	 * false.
	 * @return
	 */
	public boolean isEditable();
	
	/**
	 * get the value of a custom property.
	 * 
	 * This method is intended to allow implementations to provide additional
	 * information on how the option should be presented to the user. Keys and values
	 * as well as their semantics depend on the respective application.  
	 * @param key the property key. 
	 * @return the value or null if the property is not set.
	 */
	public String getHint(String key);
}


