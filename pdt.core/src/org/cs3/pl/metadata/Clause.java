package org.cs3.pl.metadata;

public interface Clause {

	/**
	 * Returns the signature of the predicate:
	 * module:name/arity.
	 * 
	 * @return
	 */
	public String getSignature();
	public SourceLocation getKnownDefinition();
	/**
	 * @deprecated use getKnownDefinition.getOffset()
	 * @return
	 */
	public int getPosition();

	public int getArity();

	public boolean isDynamic();

	public boolean isMultifile();

	public String getLabel();

	public boolean belongsToSamePredicate(Clause definition);

	/**
	 * @return Returns the length.
	 * @deprecated use getKnownDefinition.getEndOffset()-getKnownDefinition().getOffset()
	 */
	public int getLength();

	public int compareTo(Object arg0);

	/**
	 * @return Returns the pub.
	 */
	public boolean isPublic();

	/**
	 * @return checks if this prolog element is a module.
	 */
	public boolean isModule();

	public String getModule();
	
	public Predicate getPredicate();

}