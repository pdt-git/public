package org.cs3.pl.metadata;

import java.io.Serializable;

/**
 * a tupel describing a position within a sourcefile.
 * <br>
 * This is just used as a means of using structured data as argument
 * and return value. 
 * <br>Rows are counted starting from 1.
 * <br>Offsets are counted starting from 0.
 * <br>Last rows are always inclusive.
 * <br>End offsets are always exclusive.
 */
public final class SourceLocation implements Serializable {
	private static final long serialVersionUID = 1L;
	
	/**
	 * The line within the source code.
	 * <br>
	 * This is only used for line based source locations.
	 * The first line in a file is line 1;
	 */
    public int line=1;
	
	/**
	 * The offset within the source file / source line.
	 * <br>
	 * If this is a line-based source location, this is the character
	 * position within the containing line. The first character in a line is
	 * character 0.
	 * <br>
	 * If this location is NOT row-based, this is the offset within the containing 
	 * character stream. The first character in the file is at offset 0.
	 */
	public int offset=0;
	
	/**
	 * The last line of a range. (inclusive)
	 * <br>
	 * For line-based locations this is the last line that overlaps
	 * with this range. For stream-based locations, this value is meaningless.
	 */
	public int lastLine=1;
	
	/**
	 * The end offset of a range. (exclusive)
	 * <br>
	 * For line-based locations, this is the position of the first character that does NOT
	 * belong to the range within the last line that DOES belong to the range.
	 * <br>
	 * For stream-based locations, this is the offset of the first character that does NOT
	 * belong to the range. 
	 */
	public int endOffset=0;
	
	/**
	 * The absolute path to the file containing this location.
	 * <br>
	 * The should be interpreted as workspace path or as filesystem path, depending 
	 * on the value of isWorkspacePath.
	 */
	public String file;
	
	/**
	 * Determines wether this is a row-based location.
	 */
	public boolean isRowBased;
	
	/**
	 * Determines wether the path is workspace-relative.
	 */
	public boolean isWorkspacePath;
	
	public SourceLocation(String file,boolean isWorkspacePath,boolean isRowBased){
		this.file=file;
		this.isWorkspacePath=isWorkspacePath;
		this.isRowBased=isRowBased;
	}
	
	public String toString() {
		if(isRowBased){
			return file + "/" + line+","+offset;	
		}
		return file + "/" + offset;
	}

    
}
