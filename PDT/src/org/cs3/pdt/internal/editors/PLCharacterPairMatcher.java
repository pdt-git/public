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

    private int offset;
    private IDocument document;
    private int anchor;
    private StringBuffer stack = new StringBuffer();

    /* (non-Javadoc)
     * @see org.eclipse.jface.text.source.ICharacterPairMatcher#dispose()
     */
    public void dispose() {
        // TODO Auto-generated method stub

    }

    /* (non-Javadoc)
     * @see org.eclipse.jface.text.source.ICharacterPairMatcher#clear()
     */
    public void clear() {
       anchor = -1;
       document = null;
       offset = -1;
       stack=new StringBuffer();
       
    }

    /* (non-Javadoc)
     * @see org.eclipse.jface.text.source.ICharacterPairMatcher#match(org.eclipse.jface.text.IDocument, int)
     */
    public IRegion match(IDocument document, int offset) {
        try {
            this.document = document;
            offset--;
            this.offset = offset;
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
