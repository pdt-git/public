/* $LICENSE_MSG$(ld) */

/*
 */
package org.cs3.prolog.pif;


/**
 * A PrologInterfaceException is thrown when a PrologInterface looses
 * the connection to the prolog process due to an error.
 */
public class PrologInterfaceException extends Exception {

	private static final long serialVersionUID = 1298404946804072338L;


    public PrologInterfaceException() {
        super();
    }


    public PrologInterfaceException(String message) {
        super(message);
    }
    
    public PrologInterfaceException(Throwable cause) {
        super(cause);
    }

    public PrologInterfaceException(String message, Throwable cause) {
        super(message, cause);
     }

}

