/*
 */
package org.cs3.pl.model;

import java.io.InputStream;

/**
 */
public interface ISourceFile extends IPrologElement{
    InputStream getContents();
    String getContents(int offset, int length);
    IModule getModule();
    IClause[] getClauses();   
}
