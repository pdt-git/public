/*
 */
package org.cs3.pl.parser;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;

import org.cs3.pl.common.Debug;

/**
 */
public class FileLineBreakInfoProvider extends StringLineBreakInfoProvider {
    
    public static String getText(File file) {
    	try {
    		InputStream in = new FileInputStream(file);
    		ByteArrayOutputStream out = new ByteArrayOutputStream();
    		byte[] buf = new byte[1024];
    		int read = in.read(buf);
    		while (read > 0) {
    			out.write(buf, 0, read);
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
