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

import java.util.Iterator;
import java.util.Vector;

import org.cs3.pl.common.logging.Debug;

/**
 */
public class StringLineBreakInfoProvider implements LineBreakInfoProvider {
    Vector<Integer> lineBreaks = new Vector<Integer>();
    private String nl;

    public StringLineBreakInfoProvider(String text,String nl) {

        this.nl = nl;
        int offset = text.indexOf(nl);
        while (offset >= 0 && offset <= text.length()) {
            lineBreaks.add(new Integer(offset));
            offset = text.indexOf(nl, offset +nl.length());
        }
    }

    /**
     * @param text
     */
    public StringLineBreakInfoProvider(String text) {
        //this(text,System.getProperty("line.separator"));
    	this(text,"\n");
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.parser.LineBreakInfoProvider#getOffsetAtLine(int)
     */
    @Override
	public int getOffsetAtLine(int line) {
        try{
        	return line == 0 ? 0 : lineBreaks.get(line - 1).intValue()+nl.length();
        }
        catch(ArrayIndexOutOfBoundsException e){
        	Debug.error("AaaaAAAAaaaaAAAAaaaAAAaaargh.");
        	throw e;
        }
    }

    @Override
	public int getLineAtOffset(int offset){
    	int r=0;
    	for (Iterator<Integer> it = lineBreaks.iterator(); it.hasNext();) {
			Integer i = it.next();
			if(i.intValue()<offset){
				r++;
			}else{
				break;
			}
		}
    	return r;
    }
    
    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.parser.LineBreakInfoProvider#getLineCount()
     */
    @Override
	public int getLineCount() {
        return lineBreaks.size();
    }

}


