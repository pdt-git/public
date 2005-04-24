package org.cs3.pl.metadata;

public interface Predicate {

	/**
	 * Returns the signature of the predicate:
	 * name/arity.
	 * 
	 * @return
	 */
	public String getSignature();

	public int getArity();

	public boolean isDynamic();

	public boolean isMultifile();

	public String getLabel();

	/**
	 * @return Returns the pub.
	 */
	public boolean isPublic();

	public String getModule();
	

}