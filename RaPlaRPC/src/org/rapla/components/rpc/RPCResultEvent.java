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

class RPCResultEvent implements java.io.Serializable {
    // Don't forget to increase the serialVersionUID when you change the fields
    private static final long serialVersionUID = 1;
    Object result;
    Throwable exception;

    public RPCResultEvent() {
        this.result = null;
    }

    public RPCResultEvent(Object result) {
        this.result = result;
    }

    public void setCause(Throwable cause) {
        this.exception = cause;
    }

    public Object getResult() {
        return result;
    }
    public Throwable getCause() {
        return exception;
    }
}
