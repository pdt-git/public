/*
 * Created on 31.01.2004
 *
 * To change the template for this generated file go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
package org.cs3.pl.metadata;

import java.io.Serializable;
import java.util.Comparator;
import java.util.Map;

import org.cs3.pl.prolog.PrologException;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologSession;

/**
 * @author xproot
 *
 * To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
public class PrologElementData implements Serializable, Comparable{

	/**
     * Comment for <code>serialVersionUID</code>
     */
    private static final long serialVersionUID = 1L;
    /* (non-Javadoc)
	 * @see org.eclipse.core.runtime.IAdaptable#getAdapter(java.lang.Class)
	 */
	protected String label;
	protected int arity;
	protected int pos;
	
	
	protected boolean dynamic;
	protected boolean multifile;
	private int length;
	private boolean pub;

	private PrologInterface prologInterface=null;
	private String modulePrefix=""; 
	
	/**
	 * @param name
	 * @param arity
	 */
	public PrologElementData(String label, int arity,boolean pub, int pos,int length, boolean dynamic, boolean multifile) {
		this.pub = pub;
		this.label = label;
		this.arity = arity;
		this.pos = pos;
		this.length = length;
		this.dynamic = dynamic;
		this.multifile =multifile;
		
//		if (dynamic)
//			imageDescriptorId = ImageRepository.PE_DYNAMIC;
//		if (multifile)
//			imageDescriptorId = ImageRepository.PE_MULTIFILE;
	}


	/**
	 * @param line
	 */
	public PrologElementData(String line) {
		label = getPredicateName(line);
		arity = getPredicateArity(line);
	}


	/**
	 * Creates a PrologElementData Entity. This class is a container for
	 * Prolog elements like facts, clauses or modules. 
	 * 
	 * @param elementName
	 * @param arity if arity is -1 the element is a module.
	 */
	public PrologElementData(String elementName, int arity) {
		label = elementName;
		this.arity = arity;
	}


	/**
	 * Returns the signature of the predicate:
	 * name/arity.
	 * 
	 * @return
	 */
	public String getSignature() {
		if(arity == -1)
			return label + " (module)";
		return label + "/" + arity;
	}

	public String toString() {
		return getSignature()+ " ( pos " + pos + ")";
	}


	public void setPosition(int pos) {
		this.pos = pos;
	}


	public int getPosition() {
		return pos;
	}


	public void setArity(int arity) {
		this.arity = arity;
	}


	public int getArity() {
		return arity;
	}

	public void setDynamic(boolean dynamic) {
		this.dynamic = dynamic;
	}

	public boolean isDynamic() {
		return dynamic;
	}

	public void setMultifile(boolean multifile) {
		this.multifile = multifile;
	}

	public boolean isMultifile() {
		return multifile;
	}


	public void setLabel(String label) {
		this.label = label;
	}


	public String getLabel() {
		return label;
	}
		
	static public String getPredicateName(String line) {
		line = line.substring(0, line.indexOf('('));
		return line;
	}
	
	/**
	 * 
	 * TODO This function must be replaced by a parser!
	 * By now it count the number of counts the number of commas
	 * that occure bevore the first ')'.
	 * 
	 * @author windeln
	 * @return
	 */
	
	
	static public int getPredicateArity(String line) {
		if (line.lastIndexOf(')') == -1)
			return 0;
		line = line.substring(0, line.lastIndexOf(')'));
		int i = 1;
		while(line.indexOf(',') != -1) {
			line = line.substring(line.indexOf(',')+1);
			i++;
		}
		return i;
	}

	public boolean belongsToSamePredicate(PrologElementData data){
		if (data.arity == arity &&
			data.label.equals(label))
			return true;
		else
			return false;
	}
	/**
	 * @return Returns the length.
	 */
	public int getLength() {
		return length;
	}
	/**
	 * @param length The length to set.
	 */
	public void setLength(int length) {
		this.length = length;
	}
	
	public String getHelp() throws PrologException {
		if(prologInterface==null){
			return "If i head a PrologInterface, i could give you more information...";
		}
		PrologSession session = prologInterface.getSession();
		try {
		    Map table = session.query(modulePrefix+"manual_entry("+getLabel()+","+getArity()+",Info)");
		    if (table != null)
		        return table.get("Info").toString();
		    return null;
		} finally {
		    session.dispose();
		}
	}
	public String getSummary() throws PrologException {
		String help = getHelp();
		if (help == null)
			return null;
		return help.substring(0,help.indexOf('\n'));
	}

	static public Comparator getComparator() {
		return new Comparator() {

			public int compare(Object arg0, Object arg1) {
				return ((PrologElementData) arg0).getSignature().compareTo(
						((PrologElementData) arg1).getSignature());
			}
			
		};
	}
	
	public int compareTo(Object arg0) {
		return getSignature().compareTo(((PrologElementData)arg0).getSignature());
	}
	

/**
 * @return Returns the pub.
 */
public boolean isPublic() {
	return pub;
}
/**
 * @param pub The pub to set.
 */
public void setPublic(boolean pub) {
	this.pub = pub;
}

/**
 * @return checks if this prolog element is a module.
 */
public boolean isModule() {
	return arity == -1;
}

}
