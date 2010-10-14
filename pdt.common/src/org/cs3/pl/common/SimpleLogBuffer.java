/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Lukas Degener (among others) 
 * E-mail: degenerl@cs.uni-bonn.de
 * WWW: http://roots.iai.uni-bonn.de/research/pdt 
 * Copyright (C): 2004-2006, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms 
 * of the Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 * In addition, you may at your option use, modify and redistribute any
 * part of this program under the terms of the GNU Lesser General Public
 * License (LGPL), version 2.1 or, at your option, any later version of the
 * same license, as long as
 * 
 * 1) The program part in question does not depend, either directly or
 *   indirectly, on parts of the Eclipse framework and
 *   
 * 2) the program part in question does not include files that contain or
 *   are derived from third-party work and are therefor covered by special
 *   license agreements.
 *   
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 *   
 * ad 1: A program part is said to "depend, either directly or indirectly,
 *   on parts of the Eclipse framework", if it cannot be compiled or cannot
 *   be run without the help or presence of some part of the Eclipse
 *   framework. All java classes in packages containing the "pdt" package
 *   fragment in their name fall into this category.
 *   
 * ad 2: "Third-party code" means any code that was originaly written as
 *   part of a project other than the PDT. Files that contain or are based on
 *   such code contain a notice telling you so, and telling you the
 *   particular conditions under which they may be used, modified and/or
 *   distributed.
 ****************************************************************************/

/*
 */
package org.cs3.pl.common;

import java.io.PrintStream;

public class SimpleLogBuffer implements LogBuffer {
    private final static int MAX_LENGTH=500000;//this should be enough. 
    private StringBuffer buffer=new StringBuffer();
    private String lastKey=null;
    /* (non-Javadoc)
     * @see org.cs3.pl.common.LogBuffer#log(java.lang.String, char)
     */
    @Override
	public synchronized void log(String key, char c) {
        log(key,new byte[]{(byte) c});
    }

    private synchronized void cutHead(){
        int cut = buffer.length()-MAX_LENGTH;
        if(cut>0){
            buffer.delete(0,Math.min(cut*2,buffer.length()+1/2));
        }
    }
    
    private synchronized void setKey(String key) {
		if(lastKey!=null){
			buffer.append("</"+lastKey+">\n");
		} else if(key!=null || !lastKey.equals(key)){
			buffer.append("<"+key+">");
            lastKey=key;
        }			
        cutHead();
    }

    /* (non-Javadoc)
     * @see org.cs3.pl.common.LogBuffer#log(java.lang.String, byte[], int, int)
     */
    @Override
	public synchronized void log(String key, byte[] buf, int offset, int len) {
        setKey(key);
        if(len>0) {
            String string = new String(buf,offset,len);
			this.buffer.append(string);
        }
        else{
            this.buffer.append("<<EOF>>");
        }
        cutHead();
    }

    /* (non-Javadoc)
     * @see org.cs3.pl.common.LogBuffer#log(java.lang.String, java.lang.String)
     */
    @Override
	public synchronized void log(String key, String s) {
        byte[] bytes = s.getBytes();
		log(key,bytes,0,bytes.length);		
    }

    /* (non-Javadoc)
     * @see org.cs3.pl.common.LogBuffer#log(java.lang.String, byte[])
     */
    @Override
	public synchronized void log(String key, byte[] b) {
		log(key,b,0,b.length);
    }

    /* (non-Javadoc)
     * @see org.cs3.pl.common.LogBuffer#printLog(java.io.PrintStream)
     */
    @Override
	public synchronized void printLog(PrintStream out) {
       out.println(buffer.toString());        
    }

    /* (non-Javadoc)
     * @see java.lang.Object#toString()
     */
    @Override
	public synchronized String toString() {     
        cutHead();
        return buffer.toString();
    }
}
