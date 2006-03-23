package org.cs3.pl.metadata;

/**
 * A prolog directive handle.
 * 
 * WARNING: work in progress.
 * 
 * An instance of this class serves as a handle to a piece of 
 * prolog code. 
 * 
 * 
 * A directive is always defined in some well known file that is persisted on a filesystem
 * A directive has a well defined position within this file, an interval of characters that
 * 	- contains the definition of this directive, 
 *  - does not intersect the definition of any other member or comment and does 
 *    not contain any leading or trailing whitespace.
 *  
 * Both, the file aswell as the definition of the file can be obtained using the member
 * method getSourceLocation().
 *
 * Two directives are considered equal if there their source location property is equal.
 */
public interface Directive extends Comparable{

	/**
	 * @return the location in the source code where this directive is 
	 * defined.
	 */
	public SourceLocation getSourceLocation();
	
	/**
	 * @return the body goal of this directive
	 */
	public Goal getBody();
}

