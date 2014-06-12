/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Lukas Degener (among others)
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2004-2012, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

/*
 */
package org.cs3.prolog.connector.process;


/**
 * A PrologProcessException is thrown when a PrologProcess looses
 * the connection to the prolog process due to an error.
 */
public class PrologProcessException extends Exception {

	private static final long serialVersionUID = 1298404946804072338L;


    public PrologProcessException() {
        super();
    }


    public PrologProcessException(String message) {
        super(message);
    }
    
    public PrologProcessException(Throwable cause) {
        super(cause);
    }

    public PrologProcessException(String message, Throwable cause) {
        super(message, cause);
     }

}


