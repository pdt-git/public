/*
 */
package org.cs3.pl.common;

import java.io.PrintStream;

/**
 */
public interface LogBuffer {
    public void log(String key,char c);
    public void log(String key,byte[] buf, int offset, int len);
    public void log(String key,String s);
    public void log(String key,byte[] b);
    
    public void printLog(PrintStream out);    
}
