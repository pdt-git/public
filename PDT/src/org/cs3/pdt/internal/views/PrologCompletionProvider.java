package org.cs3.pdt.internal.views;
import java.util.Iterator;
import java.util.TreeSet;

import org.cs3.pl.common.Debug;
import org.cs3.pl.console.CompoletionResult;
import org.cs3.pl.console.ConsoleCompletionProvider;
import org.cs3.pl.metadata.DefaultMetaInfoProvider;
import org.cs3.pl.metadata.IMetaInfoProvider;
import org.cs3.pl.metadata.PrologElementData;
import org.cs3.pl.prolog.PrologException;
import org.cs3.pl.prolog.PrologInterface;

public class PrologCompletionProvider implements ConsoleCompletionProvider {
	/**
	 * @return Returns the prologInterface.
	 */
	public PrologInterface getPrologInterface() {
		return prologInterface;
	}
	/**
	 * @param prologInterface The prologInterface to set.
	 */
	public void setPrologInterface(PrologInterface prologInterface) {
		this.prologInterface = prologInterface;
	}
    PrologInterface prologInterface=null;
	private class _Result implements CompoletionResult{

        public String getOriginalLineContent() {
            return line;
        }
        public int getOriginalCaretPosition() {
            return pos;
        }

    	public String[] getOptions() {
    		if(options==null){
    			return null;
    		}
    		String[] result = new String[options.size()];
    		int i=0;
    		for (Iterator it = options.iterator(); it.hasNext();i++) {
    			String o = (String) it.next();
    			result[i]=o;
    		}
    		return result;
    	}
        /* (non-Javadoc)
         * @see org.cs3.pl.views.ConsoleCompletionProvider#getCaretPosition()
         */
        public int getNewCaretPosition() {
        	return newPos;
        }
        /* (non-Javadoc)
         * @see org.cs3.pl.views.ConsoleCompletionProvider#getNewLineContent()
         */
        public String getNewLineContent() {
        	return newLine;
        }
        String line = null;
        String newLine = null;
        int newPos = -1;
        TreeSet options = null;
        int pos = -1;
        
    }
	TreeSet completions=null;
	/*
	 * (non-Javadoc)
	 * 
	 * @see org.cs3.pl.views.ConsoleCompletionProvider#getCompletion(java.lang.String,
	 *      int)
	 */
	public CompoletionResult doCompletion(String line, int pos) {
	    if(prologInterface==null){
	    	return null;
	    }
		_Result r = new _Result();
		r.line = line;
		r.pos=pos;		
		String head = line.substring(0, pos);		
		String tail = line.substring(pos);
		
		String[] split = head.split("[^\\w]");
        String prefix = split[split.length-1];
		
		IMetaInfoProvider metaInfo = new DefaultMetaInfoProvider(prologInterface);
		PrologElementData[] elems = null;
		
		try {
			elems = metaInfo.getPredicatesWithPrefix(null, prefix);
			r.options = new TreeSet();
			
			completions = new TreeSet();
			for (int i = 0; i < elems.length; i++) {
				r.options.add(elems[i].getSignature() );
				completions.add(elems[i].getLabel());
			}
		} catch (NumberFormatException e) {
			Debug.report(e);
		} catch (PrologException e) {
			Debug.report(e);
		}
		
		String completion = completions==null||completions.isEmpty() ? "": (String) completions.first();
		if(elems==null||elems.length==0){
			r.newLine=line;
			r.newPos=pos;			
		}
		else if(elems.length==1){			
			r.newLine= head + completion.substring(prefix.length()) + tail;
			r.newPos=pos-prefix.length()+completion.length();
		}
		else{
			int commonLength = getCommonLength();			
			String commonPart = completion.substring(prefix.length(), commonLength);
			r.newLine=head + commonPart	+ tail;
			r.newPos=pos-prefix.length()+commonLength;
		}		
		return r;
	}

	//propably there is a smarter way of doing this...
	int getCommonLength() {
		int len = 1;
		while(true){
			
			String first =(String)completions.first();
			String last =(String)completions.last();
			if(first.length()<len||last.length()<len){
				break;
			}
			String a = first.substring(0,len);
			String b = last.substring(0,len);
			if(! a.equals(b)){
				break;
			}
			len ++;
		}
		return len -1;
	}
}