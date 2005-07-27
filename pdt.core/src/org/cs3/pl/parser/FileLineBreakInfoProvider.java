/*
 */
package org.cs3.pl.parser;

import java.io.BufferedReader;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.PrintWriter;

import org.cs3.pl.common.Debug;

/**
 */
public class FileLineBreakInfoProvider extends StringLineBreakInfoProvider {
    
    public static String getText(File file) {
    	try {
    		BufferedReader in = new BufferedReader(new FileReader(file));
    		ByteArrayOutputStream out = new ByteArrayOutputStream();
    		PrintWriter outWriter = new PrintWriter(out);
    		char[] buf = new char[1024];
    		int read = in.read(buf);
    		while (read > 0) {
    			outWriter.write(buf, 0, read);
    			read = in.read(buf);
    		}
    		return out.toString();
    	} catch (IOException e) {
    	    Debug.report(e);
    	}
    	return "";
    }
    public FileLineBreakInfoProvider(File file){        
       super(getText(file));
       
    }
 
}
