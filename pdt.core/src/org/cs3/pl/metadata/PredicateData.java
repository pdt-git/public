package org.cs3.pl.metadata;
/**@deprecated*/
public class PredicateData extends PrologElementData implements Predicate {
	
	private static final long serialVersionUID = 1L;

	public PredicateData(String module,String label, int arity,boolean pub, boolean dynamic, boolean multifile){
		super(module,label, arity,pub, dynamic, multifile);
	}

	public String getPredicateProperty(String property) {
		if (EXPORTED.equals(property)) {
			return isPublic()&&! "user".equals(getModule()) ? "true":"false";
		}
		if (DYNAMIC.equals(property)){
			return isDynamic()?"true":"false";
		}
		if (MULTIFILE.equals(property)){
			return isMultifile()?"true":"false";
		}
		return null;
	}

	public void setPredicateProperty(String property, String value) {
		//TODO or not todo, that's the question. 
		
	}
}
