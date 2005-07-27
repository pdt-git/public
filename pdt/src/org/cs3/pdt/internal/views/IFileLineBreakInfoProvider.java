/*
 */
package org.cs3.pdt.internal.views;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;

import org.cs3.pl.common.Debug;
import org.cs3.pl.parser.StringLineBreakInfoProvider;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;

/**
 */
public class IFileLineBreakInfoProvider extends StringLineBreakInfoProvider {
    
    public static String getText(IFile file) {
    	try {
    		InputStream in = file.getContents();
    		ByteArrayOutputStream out = new ByteArrayOutputStream();
    		byte[] buf = new byte[1024];
    		int read = in.read(buf);
    		while (read > 0) {
    			out.write(buf, 0, read);
    			read = in.read(buf);
    		}
    		return out.toString();
    	} catch (CoreException e) {
    	    Debug.report(e);
    	} catch (IOException e) {
    	    Debug.report(e);
    	}
    	return "";
    }
    public IFileLineBreakInfoProvider(IFile file){                
       super(getText(file));
    }
 
}
