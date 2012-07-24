/* $LICENSE_MSG$ */

package org.cs3.prolog.lifecycle;

import org.cs3.prolog.pif.PrologInterfaceException;
import org.cs3.prolog.pif.PrologInterfaceListener;

public interface IPrologEventDispatcher {

	public void addPrologInterfaceListener(String subject,
			PrologInterfaceListener l) throws PrologInterfaceException;

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.cs3.pl.prolog.IPrologInterface#removePrologInterfaceListener(java.lang.String,
	 *      org.cs3.pl.prolog.PrologInterfaceListener)
	 */
	public void removePrologInterfaceListener(String subject,
			PrologInterfaceListener l) throws PrologInterfaceException;

}

