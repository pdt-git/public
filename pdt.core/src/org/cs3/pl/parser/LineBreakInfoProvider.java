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

package org.cs3.pl.parser;

/**
 * Simple helper that knows about the position of all line breaks within e.g.
 * some character stream.
 */
public interface LineBreakInfoProvider {
    /**
     * @return the offset at which the given line starts.
     * The offset at 0 is always 0. For a line index >0 this method should return
     * the offset of the first character in this line.
     */
    public int getOffsetAtLine(int line);
    
    public int getLineAtOffset(int offset);
    
    
    /**
     * @return the total number of lines.
     */
    public int getLineCount();
}


