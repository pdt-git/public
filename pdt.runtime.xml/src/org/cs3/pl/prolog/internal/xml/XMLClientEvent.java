/*
 */
package org.cs3.pl.prolog.internal.xml;

import org.cs3.pl.prolog.PrologInterfaceEvent;

/**
 */
public class XMLClientEvent extends PrologInterfaceEvent {

    /**
     * Comment for <code>serialVersionUID</code>
     */
    private static final long serialVersionUID = 1L;

    /**
     * @param source
     * @param subject
     * @param event
     */
    public XMLClientEvent(Object source, String subject, String event) {
        super(source, subject, event);

    }
   
}
