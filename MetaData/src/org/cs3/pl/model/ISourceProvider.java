package org.cs3.pl.model;

import java.io.InputStream;

/**
 * 
 */
public interface ISourceProvider {
    InputStream getContents(String filename);    
    
}
