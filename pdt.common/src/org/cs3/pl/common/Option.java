/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Lukas Degener (among others) 
 * E-mail: degenerl@cs.uni-bonn.de
 * WWW: http://roots.iai.uni-bonn.de/research/pdt 
 * Copyright (C): 2004-2006, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms 
 * of the Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 * In addition, you may at your option use, modify and redistribute any
 * part of this program under the terms of the GNU Lesser General Public
 * License (LGPL), version 2.1 or, at your option, any later version of the
 * same license, as long as
 * 
 * 1) The program part in question does not depend, either directly or
 *   indirectly, on parts of the Eclipse framework and
 *   
 * 2) the program part in question does not include files that contain or
 *   are derived from third-party work and are therefor covered by special
 *   license agreements.
 *   
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 *   
 * ad 1: A program part is said to "depend, either directly or indirectly,
 *   on parts of the Eclipse framework", if it cannot be compiled or cannot
 *   be run without the help or presence of some part of the Eclipse
 *   framework. All java classes in packages containing the "pdt" package
 *   fragment in their name fall into this category.
 *   
 * ad 2: "Third-party code" means any code that was originaly written as
 *   part of a project other than the PDT. Files that contain or are based on
 *   such code contain a notice telling you so, and telling you the
 *   particular conditions under which they may be used, modified and/or
 *   distributed.
 ****************************************************************************/

/*
 */
package org.cs3.pl.common;

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

	public final static int PATH = 7;

	public static final int ENUM = 8;

	public static final int FONT = 9;

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
