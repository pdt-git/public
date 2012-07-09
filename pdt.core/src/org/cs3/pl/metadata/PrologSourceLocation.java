package org.cs3.pl.metadata;

public class PrologSourceLocation {
	private String filePath;
	private int line;

	public PrologSourceLocation(String filePath, int line) {
		this.filePath = filePath;
		this.line = line;
	}

	public String getFilePath() {
		return filePath;
	}

	public int getLine() {
		return line;
	}

}