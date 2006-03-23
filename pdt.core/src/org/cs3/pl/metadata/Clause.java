package org.cs3.pl.metadata;


/**
 * A prolog clause handle.
 * 
 * An instance of this class serves as a handle to a piece of 
 * prolog code. Note that instances are not related to runtime clauses/clause references 
 * that are subject to builtins clause/3 or clause_property/2. Instead, instaces of this class
 * are handles to entities of the source code model.
 * 
 * In particular, clauses asserted at runtime are not instances of this class.
 * A clause is always defined in some well known file that is persisted on a filesystem
 * A clause has a well defined position within this file, an interval of characters that
 * 	- contains the definition of this clause, 
 *  - does not intersect the definition of any other 
 *  - member or comment and does not contain any leading or trailing whitespace.
 *  
 * Both, the file aswell as the definition of the file can be obtained using the member
 * method getSourceLocation(). Implementations that fail to provide this inforamtion are in 
 * violation with the contract of this interface. 
 * 
 * Two clauses are considered equal if there their source location property is equal.
 * @author lukas
 *
 */
public interface Clause extends Comparable{


	/**
	 * @return the location in the source code where this clause is 
	 * defined.
	 */
	public SourceLocation getSourceLocation();
	
	
	
	/**
	 * @return a label, suitable for displaying in list or tree views to 
	 * represent this clause.
	 * @deprecated constructing the label should be the job of a label provider
	 */
	public String getName();
	

	/**
	 * 
	 * @deprecated use getKnownDefinition.getEndOffset()-getKnownDefinition().getOffset()
	 */
	public int getLength();

	

	/**
	 * @deprecated use getPredicate.isPublic()
	 */
	public boolean isPublic();

	/**
	 * @deprecated use getPredicate().getSigniture()
	 * 
	 */
	public String getSignature();
	
	/**
	 * @deprecated use getKnownDefinition.getOffset()
	 * 
	 */
	public int getPosition();

	/**
	 * @deprecated use getPredicate().getArity();
	 */
	public int getArity();

	/**
	 * @deprecated use getPredicate().isDynamic();
	 */
	public boolean isDynamic();

	/**
	 * @deprecated use getPredicate().isMultifile();
	 */
	public boolean isMultifile();

	
	/**
	 * @return a handle to the Predicate this clause contributes to.
	 */
	public Predicate getPredicate();

}