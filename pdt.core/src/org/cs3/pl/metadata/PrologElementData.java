package org.cs3.pl.metadata;

import java.io.Serializable;
import java.util.Comparator;


/**
 * a tuple describing a logical prolog element like a predicate or a module..
 * 
 *@deprecated
 *
 */
public class PrologElementData implements Serializable, Comparable{

	/**
     * Comment for <code>serialVersionUID</code>
     */
    private static final long serialVersionUID = 1L;
    
	protected String module;
	protected String label;
	protected int arity;
	
	protected boolean dynamic;
	protected boolean multifile;
	protected boolean pub;

	protected String modulePrefix="";

	protected SourceLocation knownDefinition = null;

	protected boolean m_hasKnownDefinition;

	protected boolean isKnown;
	
	
	/**
	 * @param name
	 * @param arity
	 */
	protected PrologElementData(String module,String label, int arity,boolean pub, boolean dynamic, boolean multifile, SourceLocation knownDefinition) {
		this.pub = pub;
		this.label = label;
		this.arity = arity;
		this.dynamic = dynamic;
		this.multifile =multifile;
		this.module=module;		
		this.isKnown=true;
		this.knownDefinition=knownDefinition;
		this.m_hasKnownDefinition=true;
	}


	/**
	 * @param name
	 * @param arity
	 */
	protected PrologElementData(String module,String label, int arity,boolean pub, boolean dynamic, boolean multifile) {
		this.pub = pub;
		this.label = label;
		this.arity = arity;
		this.dynamic = dynamic;
		this.multifile =multifile;
		this.module=module;		
		this.isKnown=true;
		this.m_hasKnownDefinition=false;
	}
	
	public int hashCode() {
		return getSignature().hashCode();
	}
	public boolean equals(Object obj) {
		if(obj==null){
			return false;
		}
		if(obj instanceof PrologElementData){
			return ((PrologElementData)obj).getSignature().equals(getSignature());
		}
		return super.equals(obj);
	}

	/**
	 * Creates a PrologElementData Entity. This class is a container for
	 * Prolog elements like facts, clauses or modules. 
	 * 
	 * @param elementName
	 * @param arity if arity is -1 the element is a module.
	 */
	protected PrologElementData(String module,String elementName, int arity) {
		this.module=module;
		label = elementName;
		this.arity = arity;
		this.isKnown=false;
		this.m_hasKnownDefinition=false;
	}


	/**
	 * Returns the signature of the predicate:
	 * module:name/arity.
	 * 
	 * @return
	 */
	public String getSignature() {
		if(arity == -1)
			return label + " (module)";
		return module+":"+label + "/" + arity;
	}

	public String toString() {
		return getSignature();
	}


	


	/**
	 * @deprecated use getKnownDefinition.getOffset()
	 * @return
	 */
	public int getPosition() {
		return knownDefinition==null?-1:knownDefinition.offset;
	}




	public int getArity() {
		return arity;
	}

	

	public boolean isDynamic() {
		if(!isKnown){
			throw new UnsupportedOperationException("Not enough information.");
		}
		return dynamic;
	}

	

	public boolean isMultifile() {
		if(!isKnown){
			throw new UnsupportedOperationException("Not enough information.");
		}
		return multifile;
	}




	public String getName() {
		return label;
	}
		
	static public String getPredicateName(String line) {
		line = line.substring(0, line.indexOf('('));
		return line;
	}
	
	

	public boolean belongsToSamePredicate(Clause data){
		if (data.getArity() == arity &&
			data.getName().equals(label))
			return true;
		else
			return false;
	}
	/**
	 * @return Returns the length.
	 * @deprecated use getKnownDefinition.getEndOffset()-getKnownDefinition().getOffset()
	 */
	public int getLength() {
		return knownDefinition==null?-1:knownDefinition.endOffset-knownDefinition.offset;
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
		return getSignature().compareTo(((Predicate)arg0).getSignature());
	}
	

/**
 * @return Returns the pub.
 */
public boolean isPublic() {
	if(!isKnown){
		throw new UnsupportedOperationException("Not enough information.");
	}
	return pub;
}


/**
 * @return checks if this prolog element is a module.
 */
public boolean isModule() {
	return arity == -1;
}





public SourceLocation getSourceLocation() {
	return knownDefinition;
}


public boolean hasKnownDefinition() {
	return m_hasKnownDefinition;
}


public boolean isKnown() {
	return isKnown;
}


public String getModule() {
	return module;
}







}
