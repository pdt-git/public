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
