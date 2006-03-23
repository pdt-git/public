package org.cs3.pl.metadata;


/**
 * A handle Prolog predicate.
 * 
 * An instance of this class is a handle to a prolog predicate.
 * 
 * Two predicates are considered equal, if there module, name and arity are equal.
 * 
 * An instance is really not much more than a name to identify a predicate, e.g. when
 * passing it arround as argument to method calls.
 * 
 * Other properties besides module, name and arity may be attached to an instance. 
 * Predefined property names are EXPORTED, MULTIFILE, DYNAMIC, MODULE_TRANSPARENT.
 * 
 * Implementations are not required to attach this information. 
 * Implementations are allowed to attach any other information. However, the predefined
 * property keys may only be used with the meaning declared in the description of the
 * respective key.    
 *  
 *  
 * @author lukas
 *
 */
public interface Predicate extends Comparable{

	/**
	 * predicate property exported.
	 * 
	 * May be used to indicate wether this predicate is exported, i.e. its signatrue 
	 * occurs in the definition module's export list.
	 * 
	 * Valid values are "true" if this predicate is exported, "false" if it is not exported.
	 * null means absence of knowledge, i.e. either may be true.
	 */
	public final static String EXPORTED = "exported";
	
	/**
	 * predicate property multifile.
	 * 
	 * May be used to indicate wether this predicate is a multifile predicate, i.e. 
	 * its signatrue apears in a directive as argument to the builtin multifile/1
	 * 
	 * Valid values are "true" if this predicate is a multifile predicate, 
	 * "false" if it is not a multifile predicate.
	 * null means absence of knowledge, i.e. either may be true.
	 */
	public final static String MULTIFILE = "multifile";
	
	/**
	 * predicate property dynamic.
	 * 
	 * May be used to indicate wether this predicate is a dynamic predicate, i.e. 
	 * its signatrue apears in a directive as argument to the builtin dynamic/1
	 * 
	 * Valid values are "true" if this predicate is a dynamic predicate, 
	 * "false" if it is not a dynamic predicate.
	 * null means absence of knowledge, i.e. either may be true.
	 */
	public final static String DYNAMIC = "dynamic";
	
	/**
	 * predicate property module_transparent.
	 * 
	 * May be used to indicate wether this predicate is a module-transparent predicate, i.e. 
	 * its signatrue apears in a directive as argument to the builtin module_transparent/1
	 * 
	 * Valid values are "true" if this predicate is a module transparent predicate, 
	 * "false" if it is not a module transparent predicate.
	 * null means absence of knowledge, i.e. either may be true.
	 */	
	public final static String MODULE_TRANSPARENT = "module_transparent";
	
	
	/**
	 * 
	 * @param the property name.
	 * @return the property value or null if the property is not set.
	 */
	public String getPredicateProperty(String property);
	
	public void setPredicateProperty(String property, String value);
	
	/**
	 *  
	 * @return the signature of the predicate:
	 * name/arity.
	 * @deprecated this method is redundant and will be removed
	 */
	public String getSignature();

	/**
	 * get the predicate name.
	 * 
	 *  really only the name atom. No arity, no module.
	 *  e.g. for the builtin system:current_thread/2, this method 
	 *  returns "current_thread" 
	 * 
	 * @return the predicate name.
	 */
	public String getName();
	
	/**
	 * @return the arity, i.e. number of arguments of this predicate.
	 */
	public int getArity();

	/**
	 * @return the definition module of this predicate.
	 */
	public String getModule();
	
	/**
	 * @deprecated use getPredicateProperty(DYNAMIC) instead
	 */
	public boolean isDynamic();

	/**
	 * @deprecated getPredicateProperty(MULTIFILE) instead
	 */	
	public boolean isMultifile();
	
	
	

	

	/**
	 * @deprecated use getModule() and getPredicateProperty(EXPORTED) instead
	 */
	public boolean isPublic();
	

	
	

}