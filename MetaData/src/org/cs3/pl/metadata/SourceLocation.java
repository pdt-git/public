package org.cs3.pl.metadata;

import java.io.Serializable;


public class SourceLocation implements Serializable {
	public int line;
	public String file;
	
	public String toString() {
		return file + "/" + line;
	}


	public SourceLocation(int line, String file) {
		super();
		this.line = line;
		this.file = file;
	}


    /**
     * 
     */
    public SourceLocation() {
        
        // TODO Auto-generated constructor stub
    }
}
