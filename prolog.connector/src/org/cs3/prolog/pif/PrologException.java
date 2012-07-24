/* $LICENSE_MSG$(ld) */

/*
 */
package org.cs3.prolog.pif;


/**
 * A prolog exception is thrown when a prolog query
 * executed through a PrologSession raised an exception in 
 * the prolog runtime.
 */
public class PrologException extends RuntimeException {

    /**
     * Comment for <code>serialVersionUID</code>
     */
    private static final long serialVersionUID = 2832762018351845476L;

    public PrologException(String message) {
        super(message);
    }

    public PrologException(Throwable cause) {
        super(cause);
     }

    public PrologException(String message, Throwable cause) {
        super(message, cause);
     }

}

