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
}
