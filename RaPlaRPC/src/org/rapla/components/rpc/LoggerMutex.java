/*--------------------------------------------------------------------------*
 | Copyright (C) 2001 Christopher Kohlhaas                                  |
 |                                                                          |
 | This program is free software; you can redistribute it and/or modify     |
 | it under the terms of the GNU General Public License as published by the |
 | Free Software Foundation. A copy of the license has been included with   |
 | these distribution in the COPYING file, if not go to www.fsf.org         |
 |                                                                          |
 | As a special exception, you are granted the permissions to link this     |
 | program with every library, which license fulfills the Open Source       |
 | Definition as published by the Open Source Initiative (OSI).             |
 *--------------------------------------------------------------------------*/
package org.rapla.components.rpc;


/** Wrapper for logging the mutex messages with avalon */
public class LoggerMutex extends Mutex {
    Logger logger;
    
    /**
	 * @param name
	 */
	public LoggerMutex(Logger logger, String name) {
		this.name = name;
		this.logger = logger;
	}

	void log(String message) {
        if ( logger != null && logger.isDebugEnabled() )
            logger.debug( getLoggingPrefix() + message );
    }
    
    public void enableLogging( Logger logger ) {
        this.logger = logger;
    }
}




