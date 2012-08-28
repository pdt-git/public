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
package org.cs3.prolog.pif;

import java.util.EventObject;

/**
 */
public class PrologInterfaceEvent extends EventObject {
    /**
     * Comment for <code>serialVersionUID</code>
     */
    private static final long serialVersionUID = 1L;
    private String subject;
    private String event;

    /**
     * @return Returns the event.
     */
    public String getEvent() {
        return event;
    }
    /**
     * @return Returns the subject.
     */
    public String getSubject() {
        return subject;
    }
    /**
     * @param source
     * @param event
     * @param subject
     */
    public PrologInterfaceEvent(Object source, String subject, String event) {
        super(source);
        this.subject=subject;
        this.event=event;
    }
    
    public PrologInterfaceEvent(Object source) {
		super(source);
	}
    
}


