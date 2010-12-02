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
package org.cs3.pdt.internal.editors;

import org.cs3.pl.common.Debug;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.ITypedRegion;
import org.eclipse.jface.text.Region;
import org.eclipse.jface.text.source.ICharacterPairMatcher;

/**
 */
public class PLCharacterPairMatcher implements ICharacterPairMatcher {

    private int anchor;
    private StringBuffer stack = new StringBuffer();

    /* (non-Javadoc)
     * @see org.eclipse.jface.text.source.ICharacterPairMatcher#dispose()
     */
	@Override
    public void dispose() {
    	;
    }

    /* (non-Javadoc)
     * @see org.eclipse.jface.text.source.ICharacterPairMatcher#clear()
     */
	@Override
    public void clear() {
       anchor = -1;
       stack=new StringBuffer();
       
    }

    /* (non-Javadoc)
     * @see org.eclipse.jface.text.source.ICharacterPairMatcher#match(org.eclipse.jface.text.IDocument, int)
     */
	@Override
    public IRegion match(IDocument document, int offset) {
        try {
            offset--;//XXX: why decrement? causes  BadLocationException below when offset==0
            		//think it is because we always try to match the char left of the cursor.
                    //if we are at 0, there is nothing to match.
            if(offset<0){
            	return null;
            }
            String partitionType = document.getContentType(offset);	
			
            char nextChar = document.getChar(Math.max(offset,0));
            if(!isBracket(nextChar)){
                return null;
            }
            clear();
            boolean direction = isOpening(nextChar);
            int step=direction ? 1 : -1;
            anchor=direction ? LEFT : RIGHT;
            push(nextChar);
            
            for(int i=offset+step;i<document.getLength()&&i>=0;i+=step){
                ITypedRegion region = document.getPartition(i);
                String type = region.getType();
                if(!partitionType.equals(type)){
                    i=direction ? region.getOffset()+region.getLength()-1:
                        region.getOffset();      
                    if(i<0||i>=document.getLength()){
                        return null;
                    }
                    continue;
                }
                nextChar= document.getChar(i);
                if(!isBracket(nextChar)){
                    continue;
                }
                char topChar = peek();
                boolean topDirection = isOpening(topChar);
                boolean nextDirection = isOpening(nextChar);
                if(nextDirection==topDirection){
                    push(nextChar);
                }else {//mathing direction
                    if(topChar!=getPeer(nextChar)){//bracket mismatch
                        //Debug.warning("bracket missmatch!");
                        return null;
                    }
                    else{//matching bracket
                        pop();
                        if(isStackEmpty()){//heureka!
                            int start = Math.min(i,offset);
                            int length = Math.max(i,offset)-Math.min(i,offset)+1;
                            return new Region(start,length);
                        }//end of heureka!
                    }//end of matching bracket
                }//end of mathing direction                
            }//end of for loop
            return null;
        } catch (Throwable e) {
            Debug.report(e);
        }
        return null;
    }

    /**
     * @return
     */
    private boolean isStackEmpty() {
        return stack.length()==0;
    }

    /**
     * @param nextChar
     * @return
     */
    private boolean isBracket(char nextChar) {        
        return getPeer(nextChar)!=nextChar;
    }

    /**
     * @param nextChar
     * @return
     */
    private char getPeer(char c) {
        switch(c){
        	case '(':return ')';
        	case '{':return '}';
        	case '[':return ']';
        	case ')':return '(';
        	case '}':return '{';
        	case ']':return '[';        	
        	default: return c;
        }
    }

    private boolean  isOpening(char c){
        return c=='('||c=='{'||c=='[';
    }
    /* (non-Javadoc)
     * @see org.eclipse.jface.text.source.ICharacterPairMatcher#getAnchor()
     */
	@Override
    public int getAnchor() {      
        return anchor;
    }
    private void push(char c){
        stack.append(c);
    }
    private char peek(){
        return stack.charAt(stack.length()-1);
    }
    
    private char pop(){
        char temp = peek();
        stack.replace(stack.length()-1,stack.length(),"");
        return temp;
    }
    
}
