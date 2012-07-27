/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Lukas Degener (among others)
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2004-2012, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

/*
 */
package org.cs3.pl.parser;

import java.io.BufferedReader;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.PrintWriter;

import org.cs3.pl.common.logging.Debug;

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


