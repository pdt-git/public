/*
 * Created on 29.04.2004
 *
 * TODO To change the template for this generated file go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
package org.cs3.pl.astvisitor;

import java.util.List;

/**
 * Provides a way to abstract from the concrete handling of fully qualified
 * names by the Prolog system. The FQNManager is used to transform fqn(...) terms
 * to some other representation, that may be smaller.
 * 
 * @author schulzs1
 */
public interface FQNTranslator {

	/**
	 * generates a List of PrologWriterActions for a PrependablePrologWriter.
	 * These Actions are sufficient to include the Facts that link local IDs to
	 * FQN Terms. They should be prepended to the actual output. Note that
	 * the actual terms, and how many (possibly none!) are generated depend
	 * on the actual implementation of the FQNManager
	 * 
	 * @return a Collection of PrologWriterActions. They should all be prepended
	 * to a PrependablePrologWriter
	 */
	abstract public List getFQNMapping();
	
	/**
	 * transforms an fqn(...) term into a suitable local representation.
	 * This representation can be either the term itself, a global identifier,
	 * a local identifier, or any other String that guarantees that the 
	 * global ID table remains consistent, and can be interpreted by the
	 * prolog System, once the facts produced by getFQNMapping() are
	 * loaded
	 * 
	 * @param string - A fqn(...) term
	 * @return a suitable representation
	 */
	public abstract String transformFQN(String string);
}
